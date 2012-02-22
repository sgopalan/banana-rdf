package org.w3.rdf.simple

import org.w3.rdf._

object SimpleModule extends RDFModule {

  type Graph = Set[Triple]
  
  object Graph extends GraphCompanionObject {
    def empty: Graph = Set[Triple]()
    def apply(elems: Triple*): Graph = Set[Triple](elems:_*)
    def apply(it: Iterable[Triple]): Graph = it.toSet
    def union(left: Graph, right: Graph): Graph = left ++ right
    def toIterable(graph: Graph): Iterable[Triple] = graph.toIterable
  }
  
  case class Triple(s: Node, p: IRI, o: Node)
  object Triple extends TripleCompanionObject
  
  sealed trait Node
  
  object Node extends NodeCompanionObject {
    def fold[T](node: Node)(funIRI: IRI => T, funBNode: BNode => T, funLiteral: Literal => T): T = node match {
      case iri: IRI => funIRI(iri)
      case bnode: BNode => funBNode(bnode)
      case literal: Literal => funLiteral(literal)
    }
  }

  case class IRI(iri: String) extends Node
  object IRI extends IRICompanionObject

  case class BNode(label: String) extends Node
  object BNode extends BNodeCompanionObject

  sealed trait Literal extends Node {
    val lexicalForm: String
    val datatype: IRI
  }
  
  object Literal extends LiteralCompanionObject {
    def fold[T](literal: Literal)(funTL: TypedLiteral => T, funLL: LangLiteral => T): T = literal match {
      case tl: TypedLiteral => funTL(tl)
      case ll: LangLiteral => funLL(ll)
    }
  }
  
  case class TypedLiteral(lexicalForm: String, datatype: IRI) extends Literal
  object TypedLiteral extends TypedLiteralCompanionObject
  
  case class LangLiteral(lexicalForm: String, lang: Lang) extends Literal {
    val datatype = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")
  }
  object LangLiteral extends LangLiteralCompanionObject

  type Lang = String
  object Lang extends LangCompanionObject {
    def apply(langString: String) = langString
    def unapply(lang: Lang) = Some(lang)
  }

}