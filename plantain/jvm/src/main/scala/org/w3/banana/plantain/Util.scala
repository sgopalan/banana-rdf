package org.w3.banana.plantain

import akka.http.scaladsl.model.Uri
import org.openrdf.model.impl._
import org.openrdf.{model => sesame}


object Util {

  val valueFactory = SimpleValueFactory.getInstance()

  def toStatement(s: Plantain#Node, p: Plantain#URI, o: Plantain#Node): sesame.Statement = {
    val subject: sesame.Resource = s match {
      case uri: Uri                         => valueFactory.createIRI(uri.toString)
      case model.BNode(label)               => valueFactory.createBNode(label)
      case literal @ model.Literal(_, _, _) => throw new IllegalArgumentException(s"$literal was in subject position")
    }
    val predicate: sesame.IRI = p match {
      case uri: Uri => valueFactory.createIRI(uri.toString)
    }
    val objectt: sesame.Value = o match {
      case uri: Uri                              => valueFactory.createIRI(uri.toString)
      case model.BNode(label)                    => valueFactory.createBNode(label)
      case model.Literal(lexicalForm, uri, null) => valueFactory.createLiteral(lexicalForm, valueFactory.createIRI(uri.toString))
      case model.Literal(lexicalForm, _, lang)   => valueFactory.createLiteral(lexicalForm, lang)
    }
    valueFactory.createStatement(subject, predicate, objectt)
  }

  def toSesameGraph(graph: Plantain#Graph): sesame.Model = {
    val g = new LinkedHashModel
    graph.triples.foreach { case (s, p, o) =>
      g.add(toStatement(s, p, o))
    }
    g
  }

}
