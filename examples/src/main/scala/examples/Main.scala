package org.w3.banana.examples

/** examples for doing IO
  * from sbt:
  *   project examples
  *   run-main org.w3.banana.examples.IOExample
  */
object IOExample {

  import org.w3.banana._
  import scala.util.Properties
  import java.io.File
  import scalax.io.Resource

  // just because we can :-)
  implicit class FileW(val file: File) extends AnyVal {
    def /(child: String): File = new File(file, child)
  }

  def main(args: Array[String]): Unit = {
    /* gets the important modules
     * you could use directly the ones declared in the package object
     */

    // RDFOps holds all the operations to manipulate RDF graphs, plus some syntax constructs
    val ops = RDFOps[Rdf]
    import ops._

    // gets Turtle reader
    val reader = RDFReader[Rdf, Turtle]

    // gets RDF/XML writer
    val writer = RDFWriter[Rdf, RDFXML]

    /* reads TimBL's card in Turtle */

    val timblCard = "http://www.w3.org/People/Berners-Lee/card.ttl"
    // note: as we're using scalax.io, we don't need to worry about closing the streams
    val from = Resource.fromURL(timblCard)
    // reading from a stream can fail so in real life, you would have to deal with the Try[Rdf#Graph]
    val graph: Rdf#Graph = reader.read(from, base = timblCard) getOrElse sys.error("couldn't read TimBL's card")
    
    /* prints TimBL's card to a file as RDF/XML */

    val tmpFile = new File(Properties.tmpDir) / "card.ttl"
    val to = Resource.fromFile(tmpFile)
    val ret = writer.write(graph, to, base = timblCard)
    if (ret.isSuccess)
      println(s"successfuly wrote TimBL's card to ${tmpFile.getAbsolutePath}")

    /* prints 10 triples to stdout */

    val graph10Triples = Graph(graph.toIterable.take(10).toSet)
    val graphAsString = writer.asString(graph10Triples, base = timblCard) getOrElse sys.error("coudn't serialize the graph")
    println(graphAsString)
  }

}

/** examples for doing SPARQL queries
  * from sbt:
  *   project examples
  *   run-main org.w3.banana.examples.SPARQLExample
  * 
  * In this scenario, we search on dbpedia all the languages
  */
object SPARQLExample {

  import org.w3.banana._
  import org.w3.banana.syntax._
  import java.net.URL
  import scalaz.Id.Id

  def main(args: Array[String]): Unit = {
    /* Scala is unable to infer the types correctly when using only type projection
     * a workaround is to parameterize over RDF
     */
    def aux[Rdf <: RDF]()(implicit ops: RDFOps[Rdf], sparqlOps: SparqlOps[Rdf], sparqlHttp: SparqlHttp[Rdf, Id]): Unit = {
      import ops._
      import sparqlOps._

      /* gets a SparqlEngine out of a Sparql endpoint */

      val client = sparqlHttp(new URL("http://dbpedia.org/sparql/"))

      /* creates a Sparql Select query */

      val query = SelectQuery("""
PREFIX ont: <http://dbpedia.org/ontology/>
SELECT DISTINCT ?language WHERE {
 ?language a ont:ProgrammingLanguage .
 ?language ont:influencedBy ?other .
 ?other ont:influencedBy ?language .
} LIMIT 100
""")

      /* executes the query */

      val answers: Rdf#Solutions = client.executeSelect(query)

      /* iterate through the solutions */

      val languages: Iterable[Rdf#URI] = answers.toIterable map { row =>
        /* row is an Rdf#Solution, we can get an Rdf#Node from the variable name */
        /* both the #Rdf#Node projection and the transformation to Rdf#URI can fail in the Try type, hense the flatMap */
        row("language").flatMap(_.as[Rdf#URI]) getOrElse sys.error("die")
      }
      println(languages.toList)
    }
    aux()
  }

}
