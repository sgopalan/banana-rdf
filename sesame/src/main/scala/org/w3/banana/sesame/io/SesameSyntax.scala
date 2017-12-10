package org.w3.banana.sesame.io

import java.io.{OutputStream, Writer}
import java.net.{URI => jURI}

import org.openrdf.model.{IRI, Statement}
import org.openrdf.rio.RDFWriter
import org.openrdf.rio.helpers.{BasicWriterSettings, JSONLDMode, JSONLDSettings}
import org.openrdf.rio.jsonld.{FramingSesameJSONLDWriter, JSONLDWriter}
import org.openrdf.rio.rdfxml.{RDFXMLWriter => SRdfXmlWriter}
import org.openrdf.rio.turtle.{TurtleWriter => STurtleWriter}
import org.w3.banana.io._

/** Typeclass that reflects a Sesame String that can be used to construct an [[RDFWriter]]. */
trait SesameSyntax[T] {
  def rdfWriter(os: OutputStream, base: String): RDFWriter
  def rdfWriter(wr: Writer, base: String): RDFWriter
}

object SesameSyntax {

  implicit val RDFXML: SesameSyntax[RDFXML] = new SesameSyntax[RDFXML] {
    import org.w3.banana.sesame.Sesame.ops._
    // Sesame's parser does not handle relative URI, but let us override the behavior :-)
    def rdfWriter(os: OutputStream, base: String) = new SRdfXmlWriter(os) {
      val baseUri = URI(base)
      override def handleStatement(st: Statement) = {
        super.handleStatement(st.relativizeAgainst(baseUri))
      }
    }

    def rdfWriter(wr: Writer, base: String) = new SRdfXmlWriter(wr) {
      val baseUri = URI(base)
      override def handleStatement(st: Statement) = {
        super.handleStatement(st.relativizeAgainst(baseUri))
      }
    }
  }

  implicit val Turtle: SesameSyntax[Turtle] = new SesameSyntax[Turtle] {
    // Sesame's parser does not handle relative URI, but let us override the behavior :-)
    def write(uri: IRI, writer: Writer, baseURI: jURI) = {
      val juri = new jURI(uri.toString)
      val uriToWrite = baseURI.relativize(juri)
      writer.write("<" + uriToWrite + ">")
    }

    def rdfWriter(os: OutputStream, base: String) = new STurtleWriter(os) {
      val baseUri = new jURI(base)
      override def writeURI(uri: IRI): Unit = write(uri, writer, baseUri)
    }

    def rdfWriter(wr: Writer, base: String) = new STurtleWriter(wr) {
      val baseUri = new jURI(base)
      override def writeURI(uri: IRI): Unit = write(uri, writer, baseUri)
    }
  }

  implicit val jsonLdCompacted: SesameSyntax[JsonLdCompacted] = jsonldSyntax(JSONLDMode.COMPACT)

  implicit val jsonLdExpanded: SesameSyntax[JsonLdExpanded] = jsonldSyntax(JSONLDMode.EXPAND)

  implicit val jsonLdFlattened: SesameSyntax[JsonLdFlattened] = jsonldSyntax(JSONLDMode.FLATTEN)

  private def jsonldSyntax[T](mode: JSONLDMode) = new SesameSyntax[T] {
    import org.w3.banana.sesame.Sesame.ops._

    def rdfWriter(os: OutputStream, base: String) = {
      val baseUri = URI(base)
      val writer = new JSONLDWriter(os) {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, mode);
      writer
    }
    def rdfWriter(wr: Writer, base: String) = {
      val baseUri = URI(base)
      val writer = new JSONLDWriter(wr)  {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, mode);
      writer
    }
  }

  def jsonLdFramingSyntax(
      frame: java.util.Map[String, Object] = null,
      mode: JSONLDMode = JSONLDMode.COMPACT,
      prettyPrint : Boolean) = new SesameSyntax[JsonLdFramed] {
    import org.w3.banana.sesame.Sesame.ops._

    def rdfWriter(os: OutputStream, base: String) = {
      val baseUri = URI(base)
      val writer = new FramingSesameJSONLDWriter(os) {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, mode);

      // Optionally define what JSON-LD profile is to be used
      // The Expand mode is used by default
      //writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, JSONLDMode.FLATTEN);
      // Switch from the default JSON pretty-print to a white-space reduced JSON representation
      if (prettyPrint) {
        writer.getWriterConfig().set(BasicWriterSettings.PRETTY_PRINT, java.lang.Boolean.TRUE);
      }

      //HACK for framing
      //val frameJson = JSONUtils.fromString(frame)//.asInstanceOf[java.util.Map]
      writer.getWriterConfig().set(FramingSesameJSONLDWriter.JSONLD_FRAMING, frame)

      writer
    }
    def rdfWriter(wr: Writer, base: String) = {
      val baseUri = URI(base)
      val writer = new FramingSesameJSONLDWriter(wr)  {
        override def handleStatement(st: Statement) = {
          super.handleStatement(st.relativizeAgainst(baseUri))
        }
      }
      writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, JSONLDMode.COMPACT);

      // Optionally define what JSON-LD profile is to be used
      // The Expand mode is used by default
      //writer.getWriterConfig().set(JSONLDSettings.JSONLD_MODE, JSONLDMode.FLATTEN);
      // Switch from the default JSON pretty-print to a white-space reduced JSON representation
      writer.getWriterConfig().set(BasicWriterSettings.PRETTY_PRINT, java.lang.Boolean.TRUE);

      //HACK for framing
      //val frameJson = JSONUtils.fromString(frame)//.asInstanceOf[java.util.Map]
      writer.getWriterConfig().set(FramingSesameJSONLDWriter.JSONLD_FRAMING, frame)
      writer
    }
  }

}
