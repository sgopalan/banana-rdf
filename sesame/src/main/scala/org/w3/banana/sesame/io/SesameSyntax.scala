package org.w3.banana.sesame.io

import java.io.{OutputStream, Writer}
import java.net.{URI => jURI}

import org.openrdf.model.{IRI, Statement}
import org.openrdf.rio.{RDFWriter, RioSetting}
import org.openrdf.rio.helpers.{BasicWriterSettings, JSONLDMode, JSONLDSettings, RioSettingImpl}
import org.openrdf.rio.jsonld.{FramingJSONLDWriter, JSONLDWriter}
import org.openrdf.rio.rdfxml.{RDFXMLWriter => SRdfXmlWriter}
import org.openrdf.rio.turtle.{TurtleWriter => STurtleWriter}
import org.w3.banana.RDF
import org.w3.banana.io._

import scalaz.{Comonad, Monad}

/** Typeclass that reflects a Sesame String that can be used to construct an [[RDFWriter]]. */
trait SesameSyntax[T] {
  def rdfWriter(os: OutputStream, base: String): RDFWriter = rdfWriter(os, base, Map.empty[RioSetting[Any], Any])
  def rdfWriter(wr: Writer, base: String): RDFWriter = rdfWriter(wr, base, Map.empty[RioSetting[Any], Any])
  def rdfWriter(os: OutputStream, base: String, writerConfig: Map[RioSetting[Any], Any]): RDFWriter
  def rdfWriter(wr: Writer, base: String, writerConfig: Map[RioSetting[Any], Any]): RDFWriter
}

object SesameSyntax {

  implicit val RDFXML: SesameSyntax[RDFXML] = new SesameSyntax[RDFXML] {
    import org.w3.banana.sesame.Sesame.ops._
    // Sesame's parser does not handle relative URI, but let us override the behavior :-)
    def rdfWriter(os: OutputStream, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val writer = new SRdfXmlWriter(os) {

        val baseUri = URI(base)

        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}
      writer
    }

    def rdfWriter(wr: Writer, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val writer = new SRdfXmlWriter(wr) {
        val baseUri = URI(base)

        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }

      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}
      writer
    }
  }

  implicit val Turtle: SesameSyntax[Turtle] = new SesameSyntax[Turtle] {
    // Sesame's parser does not handle relative URI, but let us override the behavior :-)
    def write(uri: IRI, writer: Writer, baseURI: jURI) = {
      val juri = new jURI(uri.toString)
      val uriToWrite = baseURI.relativize(juri)
      writer.write("<" + uriToWrite + ">")
    }

    def rdfWriter(os: OutputStream, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val writer = new STurtleWriter(os) {
        val baseUri = new jURI(base)

        override def writeURI(uri: IRI): Unit = write(uri, writer, baseUri)
      }
      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}
      writer
    }

    def rdfWriter(wr: Writer, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val writer = new STurtleWriter(wr) {
        val baseUri = new jURI(base)

        override def writeURI(uri: IRI): Unit = write(uri, writer, baseUri)
      }
      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}
      writer
    }
  }

  implicit val jsonLdCompacted: SesameSyntax[JsonLdCompacted] = jsonldSyntax(JSONLDMode.COMPACT)

  implicit val jsonLdExpanded: SesameSyntax[JsonLdExpanded] = jsonldSyntax(JSONLDMode.EXPAND)

  implicit val jsonLdFlattened: SesameSyntax[JsonLdFlattened] = jsonldSyntax(JSONLDMode.FLATTEN)

  private def jsonldSyntax[T](mode: JSONLDMode) = new SesameSyntax[T] {

    import org.w3.banana.sesame.Sesame.ops._

    def rdfWriter(os: OutputStream, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val baseUri = URI(base)
      val writer = new FramingJSONLDWriter(os) {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, mode);

      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}

      writer
    }

    def rdfWriter(wr: Writer, base: String, writerConfig: Map[RioSetting[Any], Any]) = {
      val baseUri = URI(base)
      val writer = new FramingJSONLDWriter(wr) {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, mode);

      writerConfig.foreach { config => writer.getWriterConfig().set(config._1, config._2)}

      writer
    }
  }
}
