package emarsys

import scala.annotation.tailrec
import scala.collection.mutable

final case class CyclicGraphError(message: String) extends Error(message)
package object emarsys {

  /* https://en.m.wikipedia.org/wiki/Topological_sorting */
  def tsort[A](
      nodes: List[A],
      edges: Set[(A, A)]
  ): Either[CyclicGraphError, List[A]] = {
    @tailrec
    def loop(stack: List[A], edges: Set[(A, A)], acc: List[A]): List[A] =
      stack match {
        case Nil =>
          if (edges.isEmpty) acc
          else
            throw new CyclicGraphError(
              s"At least one of the nodes has a circular dependency"
            )
        case head :: tail =>
          val (deps, remainder) = edges.partition(_._1 == head)
          val insert = deps
            .map(_._2)
            .filterNot(remainder.map(_._2).contains(_))
            .toList
          loop(tail ::: insert, remainder, head :: acc)
      }

    def edgesTo(n: A, e: Set[(A, A)]) = e.filter(_._2 == n)
    def dependenciesOf(n: A, e: Set[(A, A)]) = edgesTo(n, e).map(_._1).toList

    try {
      if (nodes == Nil) return Right(Nil)
      val initial =
        nodes.filter(dependenciesOf(_, edges).isEmpty) match {
          case Nil =>
            throw new CyclicGraphError(
              "No single node found without a dependency."
            )
          case h :: t => h :: t
        }
      Right(loop(initial, edges, Nil).reverse.distinct)
    } catch {
      case e: CyclicGraphError => Left(e)
    }
  }
  // might want to impose partial order from external source
  // def tsort[A](
  //     elems: Iterable[A]
  // )(implicit po: PartialOrdering[A]): Either[CyclicGraphError, List[A]] = {
  //   val uniquePairs = for {
  //     (a, ia) <- elems.zipWithIndex
  //     (b, ib) <- elems.zipWithIndex
  //     if ia <= ib
  //   } yield (a, b)
  //   val relations = uniquePairs
  //     .map { case (a, b) =>
  //       po.tryCompare(a, b).map(i => if (i < 0) (a, b) else (b, a))
  //     }
  //     .flatten
  //     .toSet
  //   tsort(elems.toList, relations)
  // }
}
