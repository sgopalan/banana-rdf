package org.openrdf.rio.jsonld

import java.io._
import java.nio.charset.Charset
import java.util

import com.fasterxml.jackson.core.JsonGenerationException
import com.fasterxml.jackson.databind.JsonMappingException
import com.github.jsonldjava.core.{JsonLdError, JsonLdOptions, JsonLdProcessor}
import com.github.jsonldjava.utils.JsonUtils
import org.openrdf.model.impl.LinkedHashModel
import org.openrdf.model.{Namespace, Statement}
import org.openrdf.rio.helpers._
import org.openrdf.rio.{RDFFormat, RDFHandlerException, RDFWriter}

object FramingSesameJSONLDWriter  {
  val JSONLD_FRAMING = new RioSettingImpl[Object]("jsonld-framing", "jsonld-framing", null)
}

class FramingSesameJSONLDWriter(final val writer : Writer) extends AbstractRDFWriter with RDFWriter {

  private val model = new LinkedHashModel

  private val statementCollector = new StatementCollector(model)

  /**
    * Create a SesameJSONLDWriter using a {@link java.io.OutputStream}
    *
    * @param outputStream
    * The OutputStream to write to.
    */
  def this(outputStream: OutputStream) {
    this(new BufferedWriter(new OutputStreamWriter(outputStream, Charset.forName("UTF-8"))))
  }

  @throws[RDFHandlerException]
  override def handleNamespace(prefix: String, uri: String): Unit = {
    model.setNamespace(prefix, uri)
  }

  @throws[RDFHandlerException]
  override def startRDF(): Unit = {
    statementCollector.clear()
    model.clear()
  }

  @throws[RDFHandlerException]
  def endRDF(): Unit = {
    val serialiser = new JSONLDInternalRDFParser
    try {
      var output = JsonLdProcessor.fromRDF(model, serialiser)
      val mode = getWriterConfig.get(JSONLDSettings.JSONLD_MODE)
      val opts = new JsonLdOptions
      // opts.addBlankNodeIDs =
      // getWriterConfig().get(BasicParserSettings.PRESERVE_BNODE_IDS);
      opts.setUseRdfType(getWriterConfig.get(JSONLDSettings.USE_RDF_TYPE))
      opts.setUseNativeTypes(getWriterConfig.get(JSONLDSettings.USE_NATIVE_TYPES))
      // opts.optimize = getWriterConfig().get(JSONLDSettings.OPTIMIZE);
      if (mode eq JSONLDMode.EXPAND) output = JsonLdProcessor.expand(output, opts)
      // TODO: Implement inframe in JSONLDSettings
      ////////////////////////////////////////////////////////////////////////////
      //
      // This is the start of the patch
      //
      ////////////////////////////////////////////////////////////////////////////
      val inframe = getWriterConfig()
        .get(FramingSesameJSONLDWriter.JSONLD_FRAMING).asInstanceOf[util.Map[String, Object]]
      ////////////////////////////////////////////////////////////////////////////
      //
      // This is the end of the patch
      //
      ////////////////////////////////////////////////////////////////////////////

      if (mode eq JSONLDMode.FLATTEN) output = JsonLdProcessor.flatten(output, inframe, opts)
      if (mode eq JSONLDMode.COMPACT) {
        val ctx = new util.LinkedHashMap[String, AnyRef]
        addPrefixes(ctx, model.getNamespaces)
        val localCtx = new util.HashMap[String, AnyRef]
        localCtx.put("@context", ctx)
        output = JsonLdProcessor.compact(output, localCtx, opts)
      }
      if (getWriterConfig.get(BasicWriterSettings.PRETTY_PRINT)) JsonUtils.writePrettyPrint(writer, output)
      else JsonUtils.write(writer, output)
    } catch {
      case e: JsonLdError =>
        throw new RDFHandlerException("Could not render JSONLD", e)
      case e: JsonGenerationException =>
        throw new RDFHandlerException("Could not render JSONLD", e)
      case e: JsonMappingException =>
        throw new RDFHandlerException("Could not render JSONLD", e)
      case e: IOException =>
        throw new RDFHandlerException("Could not render JSONLD", e)
    }
  }

  @throws[RDFHandlerException]
  override def handleStatement(st: Statement): Unit = {
    statementCollector.handleStatement(st)
  }

  @throws[RDFHandlerException]
  override def handleComment(comment: String): Unit = {
  }

  override def getRDFFormat: RDFFormat = RDFFormat.JSONLD

  private def addPrefixes(ctx: util.Map[String, AnyRef], namespaces: util.Set[Namespace]): Unit = {
    import scala.collection.JavaConverters._
    for (ns <- namespaces.asScala) {
      ctx.put(ns.getPrefix, ns.getName)
    }
  }
}
