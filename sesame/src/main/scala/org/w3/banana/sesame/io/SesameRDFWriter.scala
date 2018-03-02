package org.w3.banana.sesame.io

import org.w3.banana.io._
import org.w3.banana.sesame._
import java.io.{Writer, _}

import org.openrdf.model.Statement
import org.openrdf.rio.RioSetting
import org.openrdf.rio.helpers.{JSONLDMode, JSONLDSettings, RioSettingImpl}
import org.openrdf.rio.jsonld.FramingJSONLDWriter

import scala.util._

class SesameRDFWriter[T](implicit
  ops: SesameOps,
  sesameSyntax: SesameSyntax[T]
) extends RDFWriter[Sesame, Try, T] {

  def write(graph: Sesame#Graph, os: OutputStream, base: String, writerConfig: Map[RioSetting[Any], Any]): Try[Unit] = Try {
    val sWriter = sesameSyntax.rdfWriter(os, base, writerConfig)
    sWriter.startRDF()
    ops.getTriples(graph) foreach sWriter.handleStatement
    sWriter.endRDF()
  }

  def asString(graph: Sesame#Graph, base: String, writerConfig: Map[RioSetting[Any], Any]): Try[String] = Try {
    val result = new StringWriter()
    val sWriter = sesameSyntax.rdfWriter(result, base, writerConfig)
    sWriter.startRDF()
    ops.getTriples(graph) foreach sWriter.handleStatement
    sWriter.endRDF()
    result.toString
  }

  def write(graph: Sesame#Graph, os: OutputStream, base: String): Try[Unit] = write(graph, os, base, Map.empty)

  def asString(graph: Sesame#Graph, base: String): Try[String] = asString(graph, base, Map.empty)
}

class SesameRDFWriterHelper(implicit ops: SesameOps) {

  implicit val rdfxmlWriter: RDFWriter[Sesame, Try, RDFXML] = new SesameRDFWriter[RDFXML]

  implicit val turtleWriter: RDFWriter[Sesame, Try, Turtle] = new SesameRDFWriter[Turtle]

  implicit val jsonldCompactedWriter: SesameRDFWriter[JsonLdCompacted] = new SesameRDFWriter[JsonLdCompacted]

  implicit val jsonldExpandedWriter: SesameRDFWriter[JsonLdExpanded] = new SesameRDFWriter[JsonLdExpanded]

  implicit val jsonldFlattenedWriter: SesameRDFWriter[JsonLdFlattened] = new SesameRDFWriter[JsonLdFlattened]
}
