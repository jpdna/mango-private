package org.bdgenomics.mango.core.util

import com.google.protobuf.Message
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.{ compact, render }
import org.bdgenomics.mango.layout.GenotypeJson
import ga4gh.Variants.Variant

import scala.collection.JavaConverters._
import com.google.protobuf.util.JsonFormat.Printer

case class GA4GHVariantJson(id: String = "",
                            variant_set_id: String = "",
                            names: Array[String] = new Array[String](0), // this will be changed to a List[String]
                            reference_name: String = "",
                            start: Long = 0,
                            end: Long = 0,
                            reference_bases: String = "",
                            alternate_bases: String = "",
                            created: Long = 0,
                            updated: Long = 0)

case class SearchVariantsRequestGA4GH(variantSetId: String,
                                      start: String,
                                      end: String,
                                      pageSize: String,
                                      pageToken: String,
                                      referenceName: String,
                                      callSetIds: Array[String] = new Array[String](0))

case class SearchVariantsResponseGA4GH(variants: Array[GA4GHVariantJson] = new Array[GA4GHVariantJson](0),
                                       next_page_token: String = "")

object GA4GHutils {
  def genotypeStringJsonToGA4GH(genotypeStringJSON: String, datasetkey: String): String = {
    val parsedJSON: Seq[JValue] = net.liftweb.json.parse(genotypeStringJSON).children

    val resultsAsGenotypeJSON: Seq[GenotypeJson] = parsedJSON.map(f => {
      GenotypeJson(compact(render(f)).replaceAll("\\\\", "").replaceAll("^.|.$", ""))
    })

    // Convert into GA4GH schema define variant
    val resultsAsGA4GHVariant: Seq[Variant] = resultsAsGenotypeJSON.map((f: GenotypeJson) => {
      val ga4ghVariantBuilder = ga4gh.Variants.Variant.newBuilder()

      // convert alt bases to java ArrayList
      val altBases = new java.util.ArrayList[String]();
      altBases.add(f.variant.getAlternateAllele())

      ga4ghVariantBuilder.setVariantSetId("mango_variant_set_id_stub")
        .addAllNames(f.variant.getNames())
        .setReferenceName(f.variant.getContigName)
        .setStart(f.variant.getStart)
        .setEnd(f.variant.getStart + f.variant.getReferenceAllele.length)
        .setReferenceBases(f.variant.getReferenceAllele)
        .addAllAlternateBases(altBases)

      ga4ghVariantBuilder.build()
    })

    //val testVarPN: Variant = resultsAsGA4GHVariant(0)
    //val x = com.google.protobuf.util.JsonFormat.printer.print(testVarPN)
    //print("#!#!!#!: " + x)

    // Convert the ga4ghVariant into a custom case class GA4GHVariantJSON
    // note: this is because currently we have trouble getting the proto3 defined object to generate JSON
    // this ouught to work: val x = com.google.protobuf.util.JsonFormat.printer.print(resultsAsGA4GHVariant(0))
    // but apparently a GeneratedMessageV3 is not a Message

    val resultsAsGA4GHVariantJson: Seq[GA4GHVariantJson] = resultsAsGA4GHVariant.map(f => {
      GA4GHVariantJson(datasetkey + "_" + f.getReferenceName + "_" + f.getStart + "_" + f.getEnd + "_" + f.getReferenceBases + "_" + f.getAlternateBases(0),
        f.getVariantSetId,
        f.getNamesList.asScala.toArray,
        f.getReferenceName,
        f.getStart,
        f.getEnd,
        f.getReferenceBases,
        f.getAlternateBases(0))
    })

    @transient implicit val formats = net.liftweb.json.DefaultFormats

    val response = SearchVariantsResponseGA4GH(resultsAsGA4GHVariantJson.toArray)

    val ga4ghVariantJSONString = net.liftweb.json.Serialization.write(response)(formats)
    //write JSON string
    //val ga4ghVariantJSONString = net.liftweb.json.Serialization.write(resultsAsGA4GHVariantJson)(formats)

    ga4ghVariantJSONString
  }
}