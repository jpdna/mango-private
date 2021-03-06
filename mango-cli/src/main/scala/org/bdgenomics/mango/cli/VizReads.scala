/**
 * Licensed to Big Data Genomics (BDG) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The BDG licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.bdgenomics.mango.cli

import java.io.FileNotFoundException
import java.util

import com.google.protobuf.Message
import net.liftweb.json.Serialization.write
import net.liftweb.json._
import net.liftweb.json.Serialization.write
import org.apache.spark.SparkContext
import org.bdgenomics.adam.models.{ ReferenceRegion, SequenceDictionary }
import org.bdgenomics.mango.core.util.{ VizCacheIndicator, VizUtils }
import org.bdgenomics.mango.filters._
import org.bdgenomics.mango.layout.{ GenotypeJson, GenotypeString, VariantJson }
import org.bdgenomics.mango.models._
import org.bdgenomics.utils.cli._
import org.bdgenomics.utils.instrumentation.Metrics
import org.bdgenomics.utils.misc.Logging
import org.fusesource.scalate.TemplateEngine
import org.kohsuke.args4j.{ Argument, Option => Args4jOption }
import org.scalatra._
import ga4gh.VariantServiceOuterClass
import ga4gh.VariantServiceOuterClass.SearchVariantsRequest
import ga4gh.VariantServiceOuterClass.SearchVariantsRequest.Builder
import ga4gh.Variants.Variant
import ga4gh.Variants.Variant.Builder
import net.liftweb.json.JsonAST.JValue

import org.bdgenomics.mango.core.util.GA4GHutils
import org.bdgenomics.mango.core.util.SearchVariantsRequestGA4GH

//import shaded.ga4gh.com.google.protobuf.Descriptors.FieldDescriptor
//import com.fasterxml.jackson.databind.ObjectMapper
//import com.fasterxml.jackson.core.JsonGenerationException
//import com.fasterxml.jackson.core
//import com.fasterxml.jackson.core.`type`.TypeReference

//import scala.beans.BeanProperty
//import com.fasterxml.jackson.core.`type`.TypeReferencetype.TypeReference
//import com.fasterxml.jackson.databind.JsonMappingException
//import shaded.ga4gh.com.google.protobuf.ProtocolStringList

import scala.collection.JavaConverters._

import org.bdgenomics.mango.core.util.GA4GHVariantJson

import scala.collection.mutable

object VizTimers extends Metrics {
  //HTTP requests
  val ReadsRequest = timer("GET reads")
  val CoverageRequest = timer("GET coverage")
  val FreqRequest = timer("GET frequency")
  val VarRequest = timer("GET variants")
  val VarFreqRequest = timer("Get variant frequency")
  val FeatRequest = timer("GET features")
  val AlignmentRequest = timer("GET alignment")

  //RDD operations
  val FreqRDDTimer = timer("RDD Freq operations")
  val VarRDDTimer = timer("RDD Var operations")
  val FeatRDDTimer = timer("RDD Feat operations")
  val RefRDDTimer = timer("RDD Ref operations")
  val GetPartChunkTimer = timer("Calculate block chunk")

  //Generating Json
  val MakingTrack = timer("Making Track")
  val DoingCollect = timer("Doing Collect")
  val PrintReferenceTimer = timer("JSON get reference string")
}

object VizReads extends BDGCommandCompanion with Logging {

  val commandName: String = "viz"
  val commandDescription: String = "Genomic visualization for ADAM"
  implicit val formats = net.liftweb.json.DefaultFormats

  var sc: SparkContext = null
  var server: org.eclipse.jetty.server.Server = null
  var globalDict: SequenceDictionary = null

  // Gene URL
  var genes: Option[String] = None

  // Structures storing data types. All but reference is optional
  var annotationRDD: AnnotationMaterialization = null
  var readsData: Option[AlignmentRecordMaterialization] = None

  var coverageData: Option[CoverageMaterialization] = None

  var variantContextData: Option[VariantContextMaterialization] = None

  var featureData: Option[FeatureMaterialization] = None

  // variables tracking whether optional datatypes were loaded
  def readsExist: Boolean = readsData.isDefined
  def coveragesExist: Boolean = coverageData.isDefined
  def variantsExist: Boolean = variantContextData.isDefined
  def featuresExist: Boolean = featureData.isDefined

  // placeholder for indicators
  val region = ReferenceRegion("N", 0, 1)

  // reads cache
  object readsWait
  var readsCache: Map[String, String] = Map.empty[String, String]
  var readsIndicator = VizCacheIndicator(region, 1)

  // coverage reads cache
  object readsCoverageWait
  var readsCoverageCache: Map[String, String] = Map.empty[String, String]
  var readsCoverageIndicator = VizCacheIndicator(region, 1)

  // coverage cache
  object coverageWait
  var coverageCache: Map[String, String] = Map.empty[String, String]
  var coverageIndicator = VizCacheIndicator(region, 1)

  // variant cache
  object variantsWait
  var variantsCache: Map[String, String] = Map.empty[String, String]
  var variantsIndicator = VizCacheIndicator(region, 1)
  var showGenotypes: Boolean = false
  var currVariantPaths = ""

  // features cache
  object featuresWait
  var featuresCache: Map[String, String] = Map.empty[String, String]
  var featuresIndicator = VizCacheIndicator(region, 1)

  // regions to prefetch during discovery. sent to front
  // end for visual processing
  var prefetchedRegions: List[(ReferenceRegion, Double)] = List()

  // used to determine size of data tiles
  var chunkSize: Int = 1000

  // thresholds used for visualization binning and limits
  var screenSize: Int = 1000

  // HTTP ERROR RESPONSES
  object errors {
    var outOfBounds = NotFound("Region not found in Reference Sequence Dictionary")
    var largeRegion = RequestEntityTooLarge("Region too large")
    var unprocessableFile = UnprocessableEntity("File type not supported")
    var notFound = NotFound("File not found")
    def noContent(region: ReferenceRegion): ActionResult = {
      val msg = s"No content available at ${region.toString}"
      NoContent(Map.empty, msg)
    }
  }

  def apply(cmdLine: Array[String]): BDGCommand = {
    new VizReads(Args4j[VizReadsArgs](cmdLine))
  }

  /**
   * Returns stringified version of sequence dictionary
   *
   * @param dict: dictionary to format to a string
   * @return List of sequence dictionary strings of form referenceName:0-referenceName.length
   */
  def formatDictionaryOpts(dict: SequenceDictionary): String = {
    val sorted = dict.records.sortBy(_.length).reverse
    sorted.map(r => r.name + ":0-" + r.length).mkString(",")
  }

  /**
   * Returns stringified version of sequence dictionary
   *
   * @param regions: regions to format to string
   * @return list of strinified reference regions
   */
  def formatClickableRegions(regions: List[(ReferenceRegion, Double)]): String = {
    regions.map(r => s"${r._1.referenceName}:${r._1.start}-${r._1.end}" +
      s"-${BigDecimal(r._2).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble}").mkString(",")
  }

  //Correctly shuts down the server
  def quit() {
    val thread = new Thread {
      override def run() {
        try {
          log.info("Shutting down the server")
          server.stop()
          log.info("Server has stopped")
        } catch {
          case e: Exception => {
            log.info("Error when stopping Jetty server: " + e.getMessage, e)
          }
        }
      }
    }
    thread.start()
  }

}

case class ReferenceJson(reference: String, position: Long)

class VizReadsArgs extends Args4jBase with ParquetArgs {
  @Argument(required = true, metaVar = "reference", usage = "The reference file to view, required", index = 0)
  var referencePath: String = null

  @Args4jOption(required = false, name = "-genes", usage = "Gene URL.")
  var genePath: String = null

  @Args4jOption(required = false, name = "-reads", usage = "A list of reads files to view, separated by commas (,)")
  var readsPaths: String = null

  @Args4jOption(required = false, name = "-coverage", usage = "A list of coverage files to view, separated by commas (,)")
  var coveragePaths: String = null

  @Args4jOption(required = false, name = "-variants", usage = "A list of variants files to view, separated by commas (,). " +
    "Vcf files require a corresponding tbi index.")
  var variantsPaths: String = null

  @Args4jOption(required = false, name = "-show_genotypes", usage = "Shows genotypes if available in variant files.")
  var showGenotypes: Boolean = false

  @Args4jOption(required = false, name = "-features", usage = "The feature files to view, separated by commas (,)")
  var featurePaths: String = null

  @Args4jOption(required = false, name = "-port", usage = "The port to bind to for visualization. The default is 8080.")
  var port: Int = 8080

  @Args4jOption(required = false, name = "-test", usage = "For debugging purposes.")
  var testMode: Boolean = false

  @Args4jOption(required = false, name = "-discover", usage = "This turns on discovery mode on start up.")
  var discoveryMode: Boolean = false
}

class VizServlet extends ScalatraServlet {
  implicit val formats = net.liftweb.json.DefaultFormats

  get("/?") {
    redirect("/overall")
  }

  get("/quit") {
    VizReads.quit()
  }

  get("/overall") {
    contentType = "text/html"
    val templateEngine = new TemplateEngine
    // set initial referenceRegion so it is defined. pick first chromosome to view
    val firstChr = VizReads.globalDict.records.head.name
    session("referenceRegion") = ReferenceRegion(firstChr, 1, 100)
    templateEngine.layout("mango-cli/src/main/webapp/WEB-INF/layouts/overall.ssp",
      Map("dictionary" -> VizReads.formatDictionaryOpts(VizReads.globalDict),
        "regions" -> VizReads.formatClickableRegions(VizReads.prefetchedRegions)))
  }

  get("/setContig/:ref") {
    val viewRegion = ReferenceRegion(params("ref"), params("start").toLong, params("end").toLong)
    session("referenceRegion") = viewRegion
  }

  get("/browser") {
    contentType = "text/html"
    // if session variable for reference region is not yet set, randomly set it
    try {
      session("referenceRegion")
    } catch {
      case e: Exception =>
        val firstChr = VizReads.globalDict.records.head.name
        session("referenceRegion") = ReferenceRegion(firstChr, 0, 100)
    }

    val templateEngine = new TemplateEngine
    // set initial referenceRegion so it is defined
    val region = session("referenceRegion").asInstanceOf[ReferenceRegion]
    val indicator = VizCacheIndicator(region, 1)
    VizReads.readsIndicator = indicator
    VizReads.variantsIndicator = indicator
    VizReads.featuresIndicator = indicator

    // generate file keys for front end
    val readsSamples: Option[List[(String, Option[String])]] = try {
      val reads = VizReads.readsData.get.getFiles.map(r => LazyMaterialization.filterKeyFromFile(r))

      // check if there are precomputed coverage files for reads. If so, send this information to the frontend
      // to avoid extra coverage computation
      if (VizReads.coverageData.isDefined) {
        Some(reads.map(r => {
          val coverage = VizReads.coverageData.get.getFiles.map(c => LazyMaterialization.filterKeyFromFile(c))
            .find(c => {
              c.contains(r)
            })
          (r, coverage)
        }))
      } else Some(reads.map((_, None)))

    } catch {
      case e: Exception => None
    }

    val coverageSamples = try {
      val coverage = VizReads.coverageData.get.getFiles.map(r => LazyMaterialization.filterKeyFromFile(r))

      // filter out coverage samples that will be displayed with reads
      if (readsSamples.isDefined) {
        val readsCoverage = readsSamples.get.map(_._2).flatten
        Some(coverage.filter(c => !readsCoverage.contains(c)))
      } else Some(coverage)
    } catch {
      case e: Exception => None
    }

    val variantSamples = try {
      if (VizReads.showGenotypes)
        Some(VizReads.variantContextData.get.getGenotypeSamples().map(r => (LazyMaterialization.filterKeyFromFile(r._1), r._2.mkString(","))))
      else Some(VizReads.variantContextData.get.getFiles.map(r => (LazyMaterialization.filterKeyFromFile(r), "")))
    } catch {
      case e: Exception => None
    }

    val featureSamples = try {
      Some(VizReads.featureData.get.getFiles.map(r => LazyMaterialization.filterKeyFromFile(r)))
    } catch {
      case e: Exception => None
    }

    templateEngine.layout("mango-cli/src/main/webapp/WEB-INF/layouts/browser.ssp",
      Map("dictionary" -> VizReads.formatDictionaryOpts(VizReads.globalDict),
        "genes" -> VizReads.genes,
        "reads" -> readsSamples,
        "coverage" -> coverageSamples,
        "variants" -> variantSamples,
        "features" -> featureSamples,
        "contig" -> session("referenceRegion").asInstanceOf[ReferenceRegion].referenceName,
        "start" -> session("referenceRegion").asInstanceOf[ReferenceRegion].start.toString,
        "end" -> session("referenceRegion").asInstanceOf[ReferenceRegion].end.toString))
  }

  get("/reference/:ref") {
    val viewRegion = ReferenceRegion(params("ref"), params("start").toLong, params("end").toLong)
    session("referenceRegion") = viewRegion
    val dictOpt = VizReads.globalDict(viewRegion.referenceName)
    if (dictOpt.isDefined) {
      Ok(write(VizReads.annotationRDD.getReferenceString(viewRegion)))
    } else VizReads.errors.outOfBounds
  }

  get("/sequenceDictionary") {
    Ok(write(VizReads.annotationRDD.getSequenceDictionary.records))
  }

  get("/reads/:key/:ref") {
    VizTimers.ReadsRequest.time {

      if (!VizReads.readsExist) {
        VizReads.errors.notFound
      } else {
        val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
          VizUtils.getEnd(params("end").toLong, VizReads.globalDict(params("ref"))))
        val key: String = params("key")
        contentType = "json"

        val dictOpt = VizReads.globalDict(viewRegion.referenceName)
        if (dictOpt.isDefined) {
          var results: Option[String] = None
          VizReads.readsWait.synchronized {
            // region was already collected, grab from cache
            if (viewRegion != VizReads.readsIndicator.region) {
              VizReads.readsCache = VizReads.readsData.get.getJson(viewRegion)
              VizReads.readsIndicator = VizCacheIndicator(viewRegion, 1)
            }
            results = VizReads.readsCache.get(key)
          }
          if (results.isDefined) {
            Ok(results.get)
          } else VizReads.errors.noContent(viewRegion)
        } else VizReads.errors.outOfBounds
      }
    }
  }

  get("/coverage/:key/:ref") {
    val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
      VizUtils.getEnd(params("end").toLong, VizReads.globalDict(params("ref"))))
    val key: String = params("key")
    val binning: Int =
      try
        params("binning").toInt
      catch {
        case e: Exception => 1
      }
    getCoverage(viewRegion, key, binning)
  }

  get("/reads/coverage/:key/:ref") {
    VizTimers.ReadsRequest.time {

      if (!VizReads.readsExist) {
        VizReads.errors.notFound
      } else {
        val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
          VizUtils.getEnd(params("end").toLong, VizReads.globalDict(params("ref"))))
        val key: String = params("key")
        contentType = "json"

        // get all coverage files that have been loaded
        val coverageFiles =
          if (VizReads.coverageData.isDefined) {
            Some(VizReads.coverageData.get.getFiles.map(f => LazyMaterialization.filterKeyFromFile(f)))
          } else None

        // check if there is a precomputed coverage file for this reads file
        if (coverageFiles.isDefined && coverageFiles.get.contains(key)) {
          // TODO: I dont know if this is correct for getting keys
          val binning: Int =
            try
              params("binning").toInt
            catch {
              case e: Exception => 1
            }

          getCoverage(viewRegion, key, binning)
        } else {
          // no precomputed coverage
          val dictOpt = VizReads.globalDict(viewRegion.referenceName)
          if (dictOpt.isDefined) {
            var results: Option[String] = None
            VizReads.readsCoverageWait.synchronized {
              // region was already collected, grab from cache
              if (viewRegion != VizReads.readsCoverageIndicator.region) {
                VizReads.readsCoverageCache = VizReads.readsData.get.getCoverage(viewRegion)
                VizReads.readsIndicator = VizCacheIndicator(viewRegion, 1)
              }
              results = VizReads.readsCoverageCache.get(key)
            }
            if (results.isDefined) {
              Ok(results.get)
            } else VizReads.errors.noContent(viewRegion)
          } else VizReads.errors.outOfBounds
        }
      }
    }
  }

  get("/variants/:key/:ref") {
    VizTimers.VarRequest.time {
      if (!VizReads.variantsExist)
        VizReads.errors.notFound
      else {
        val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
          VizUtils.getEnd(params("end").toLong, VizReads.globalDict(params("ref"))))

        //print("\n### viewRegion: " + viewRegion)
        // val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
        //  params("end").toLong, VizReads.globalDict(params("ref"))))

        val key: String = params("key")
        contentType = "json"

        // if region is in bounds of reference, return data
        val dictOpt = VizReads.globalDict(viewRegion.referenceName)
        if (dictOpt.isDefined) {
          var results: Option[String] = None
          val binning: Int =
            try {
              params("binning").toInt
            } catch {
              case e: Exception => 1
            }
          VizReads.variantsWait.synchronized {
            // region was already collected, grab from cache
            if (VizCacheIndicator(viewRegion, binning) != VizReads.variantsIndicator) {
              VizReads.variantsCache = VizReads.variantContextData.get.getJson(viewRegion,
                VizReads.showGenotypes,
                binning)
              VizReads.variantsIndicator = VizCacheIndicator(viewRegion, binning)
            }
            results = VizReads.variantsCache.get(key)
          }
          print("\n### GET reults: " + results)
          if (results.isDefined) {
            // extract variants only and parse to stringified json
            Ok(results.get)
          } else VizReads.errors.noContent(viewRegion)
        } else VizReads.errors.outOfBounds
      }
    }
  }

  post("/ga4gh/variants/search") {
    VizTimers.VarRequest.time {

      val jsonPostString = request.body

      val searchVariantsRequest = net.liftweb.json.parse(jsonPostString)
        .extract[SearchVariantsRequestGA4GH]

      if (!VizReads.variantsExist)
        VizReads.errors.notFound
      else {

        val viewRegion = ReferenceRegion(searchVariantsRequest.referenceName,
          searchVariantsRequest.start.toLong,
          VizUtils.getEnd(searchVariantsRequest.end.toLong,
            VizReads.globalDict(searchVariantsRequest.referenceName)))

        val key: String = "ALL_chr17_7500000-7515000_phase3_shapeit2_mvncall_integrated_v5a_20130502_genotypes_vcf"
        contentType = "json"

        // if region is in bounds of reference, return data
        val dictOpt = VizReads.globalDict(viewRegion.referenceName)
        if (dictOpt.isDefined) {
          var results: Option[String] = None
          val binning: Int =
            try {
              params("binning").toInt
            } catch {
              case e: Exception => 1
            }
          VizReads.variantsWait.synchronized {
            // region was already collected, grab from cache
            if (VizCacheIndicator(viewRegion, binning) != VizReads.variantsIndicator) {
              println("\n### About to call getJson in the cache thing\n")
              VizReads.variantsCache = VizReads.variantContextData.get.getJson(viewRegion,
                true,
                1)
              VizReads.variantsIndicator = VizCacheIndicator(viewRegion, binning)
            }
            results = VizReads.variantsCache.get(key)
          }

          if (results.isDefined) {
            val ga4ghVariantJSONString = GA4GHutils.genotypeStringJsonToGA4GH(results.get, VizReads.currVariantPaths)

            Ok(ga4ghVariantJSONString)
            // extract variants only and parse to stringified json
            //Ok(results.get)
          } else VizReads.errors.noContent(viewRegion)
        } else VizReads.errors.outOfBounds
      }
    }

  }

  get("/features/:key/:ref") {
    VizTimers.FeatRequest.time {
      if (!VizReads.featuresExist)
        VizReads.errors.notFound
      else {
        val viewRegion = ReferenceRegion(params("ref"), params("start").toLong,
          VizUtils.getEnd(params("end").toLong, VizReads.globalDict(params("ref"))))
        val key: String = params("key")
        contentType = "json"

        // if region is in bounds of reference, return data
        val dictOpt = VizReads.globalDict(viewRegion.referenceName)
        if (dictOpt.isDefined) {
          var results: Option[String] = None
          val binning: Int =
            try {
              params("binning").toInt
            } catch {
              case e: Exception => 1
            }
          VizReads.featuresWait.synchronized {
            // region was already collected, grab from cache
            if (VizCacheIndicator(viewRegion, binning) != VizReads.featuresIndicator) {
              VizReads.featuresCache = VizReads.featureData.get.getJson(viewRegion, binning)
              VizReads.featuresIndicator = VizCacheIndicator(viewRegion, binning)
            }
            results = VizReads.featuresCache.get(key)
          }
          if (results.isDefined) {
            Ok(results.get)
          } else VizReads.errors.noContent(viewRegion)
        } else VizReads.errors.outOfBounds
      }
    }
  }

  /**
   * Gets Coverage for a get Request. This is used to get both Reads based coverage and generic coverage.
   * @param viewRegion ReferenceRegion to view coverage over
   * @param key key for coverage file (see LazyMaterialization)
   * @return ActionResult of coverage json
   */
  def getCoverage(viewRegion: ReferenceRegion, key: String, binning: Int = 1): ActionResult = {
    VizTimers.CoverageRequest.time {
      if (!VizReads.coveragesExist) {
        VizReads.errors.notFound
      } else {
        contentType = "json"
        val dictOpt = VizReads.globalDict(viewRegion.referenceName)
        if (dictOpt.isDefined) {
          var results: Option[String] = None
          VizReads.coverageWait.synchronized {
            // region was already collected, grab from cache
            if (viewRegion != VizReads.coverageIndicator.region) {
              VizReads.coverageCache = VizReads.coverageData.get.getCoverage(viewRegion, binning)
              VizReads.coverageIndicator = VizCacheIndicator(viewRegion, 1)
            }
            results = VizReads.coverageCache.get(key)
          }
          if (results.isDefined) {
            Ok(results.get)
          } else VizReads.errors.noContent(viewRegion)
        } else VizReads.errors.outOfBounds
      }
    }
  }
}

class VizReads(protected val args: VizReadsArgs) extends BDGSparkCommand[VizReadsArgs] with Logging {
  val companion: BDGCommandCompanion = VizReads

  override def run(sc: SparkContext): Unit = {
    VizReads.sc = sc

    // choose prefetch size
    val prefetch =
      if (sc.isLocal) 10000
      else 100000

    // initialize all datasets
    initAnnotations
    initAlignments
    initCoverages
    initVariantContext
    initFeatures

    // run discovery mode if it is specified in the startup script
    if (args.discoveryMode) {
      VizReads.prefetchedRegions = discoverFrequencies()
      preprocess(VizReads.prefetchedRegions)
    }

    // check whether genePath was supplied
    if (args.genePath != null) {
      VizReads.genes = Some(args.genePath)
    }

    // start server
    if (!args.testMode) startServer()

    /*
   * Initialize required reference file
   */
    def initAnnotations() = {
      val referencePath = Option(args.referencePath).getOrElse({
        throw new FileNotFoundException("reference file not provided")
      })

      VizReads.annotationRDD = new AnnotationMaterialization(sc, referencePath)
      VizReads.globalDict = VizReads.annotationRDD.getSequenceDictionary
    }

    /*
   * Initialize loaded alignment files
   */
    def initAlignments = {
      if (Option(args.readsPaths).isDefined) {
        val readsPaths = args.readsPaths.split(",").toList

        if (readsPaths.nonEmpty) {
          VizReads.readsData = Some(new AlignmentRecordMaterialization(sc, readsPaths, VizReads.globalDict, Some(prefetch)))
        }
      }
    }

    /*
   * Initialize coverage files
   */
    def initCoverages = {
      if (Option(args.coveragePaths).isDefined) {
        val coveragePaths = args.coveragePaths.split(",").toList

        if (coveragePaths.nonEmpty) {
          VizReads.coverageData = Some(new CoverageMaterialization(sc, coveragePaths, VizReads.globalDict, Some(prefetch)))
        }
      }
    }

    /**
     * Initialize loaded variant files
     */
    def initVariantContext() = {
      // set flag for visualizing genotypes
      VizReads.showGenotypes = args.showGenotypes
      VizReads.currVariantPaths = args.variantsPaths

      if (Option(args.variantsPaths).isDefined) {
        val variantsPaths = args.variantsPaths.split(",").toList

        if (variantsPaths.nonEmpty) {
          VizReads.variantContextData = Some(new VariantContextMaterialization(sc, variantsPaths, VizReads.globalDict, Some(prefetch)))
        }
      }
    }

    /**
     * Initialize loaded feature files
     */
    def initFeatures() = {
      val featurePaths = Option(args.featurePaths)
      if (featurePaths.isDefined) {
        val featurePaths = args.featurePaths.split(",").toList
        if (featurePaths.nonEmpty) {
          VizReads.featureData = Some(new FeatureMaterialization(sc, featurePaths, VizReads.globalDict, Some(prefetch)))
        }
      }
    }

    /**
     * Runs total data scan over all feature, variant and coverage files, calculating the normalied frequency at all
     * windows in the genome.
     *
     * @return Returns list of windowed regions in the genome and their corresponding normalized frequencies
     */
    def discoverFrequencies(): List[(ReferenceRegion, Double)] = {

      val discovery = Discovery(VizReads.annotationRDD.getSequenceDictionary)
      var regions: List[(ReferenceRegion, Double)] = List()

      // get feature frequency
      if (VizReads.featuresExist) {
        val featureRegions = VizReads.featureData.get.getAll().map(ReferenceRegion.unstranded(_))
        regions = regions ++ discovery.getFrequencies(featureRegions)
      }

      // get variant frequency
      if (VizReads.variantsExist) {
        val variantRegions = VizReads.variantContextData.get.getAll().map(r => ReferenceRegion(r.variant))
        regions = regions ++ discovery.getFrequencies(variantRegions)
      }

      // get coverage frequency
      // Note: calculating coverage frequency is an expensive operation. Only perform if sc is not local.
      if (VizReads.coveragesExist && !sc.isLocal) {
        val coverageRegions = VizReads.coverageData.get.getAll().map(ReferenceRegion(_))
        regions = regions ++ discovery.getFrequencies(coverageRegions)
      }

      // group all regions together and reduce down for all data types
      regions = regions.groupBy(_._1).map(r => (r._1, r._2.map(a => a._2).sum)).toList

      // normalize and filter by regions with data
      val max = regions.map(_._2).reduceOption(_ max _).getOrElse(1.0)
      regions.map(r => (r._1, r._2 / max))
        .filter(_._2 > 0.0)
    }

    /**
     * preprocesses data by loading specified regions into memory for reads, coverage, variants and features
     *
     * @param regions Regions to be preprocessed
     */
    def preprocess(regions: List[(ReferenceRegion, Double)]) = {
      // select two of the highest occupied regions to load
      // The number of selected regions is low to reduce unnecessary loading while
      // jump starting Thread setup for Spark on the specific data files
      val selectedRegions = regions.sortBy(_._2).takeRight(2).map(_._1)

      for (region <- selectedRegions) {
        if (VizReads.featureData.isDefined)
          VizReads.featureData.get.get(region)
        if (VizReads.readsData.isDefined)
          VizReads.readsData.get.get(region)
        if (VizReads.coverageData.isDefined)
          VizReads.coverageData.get.get(region)
        if (VizReads.variantContextData.isDefined)
          VizReads.variantContextData.get.get(region)
      }
    }

    /**
     * Starts server once on startup
     */
    def startServer() = {
      VizReads.server = new org.eclipse.jetty.server.Server(args.port)
      val handlers = new org.eclipse.jetty.server.handler.ContextHandlerCollection()
      VizReads.server.setHandler(handlers)
      handlers.addHandler(new org.eclipse.jetty.webapp.WebAppContext("mango-cli/src/main/webapp", "/"))
      VizReads.server.start()
      println("View the visualization at: " + args.port)
      println("Here is show_genotypes: " + VizReads.showGenotypes)
      println("Quit at: /quit")

      VizReads.server.join()
    }

  }
}
