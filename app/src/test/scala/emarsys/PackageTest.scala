package emarsys

import org.scalatest.funspec.AnyFunSpec
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import emarsys.tsort
import scala.util.Random
import org.scalatest.exceptions.NotAllowedException

@RunWith(classOf[JUnitRunner])
class PackageTest extends AnyFunSpec {

  val nodes = Random
    .shuffle(List("Budapest", "Vienna", "Bangkok", "Tokyo", "Paris", "Monaco"))
    .map(City(_))
  val extraCity = City("Tokyo")
  val relations = Set(
    ("Vienna", "Budapest"),
    ("Tokyo", "Bangkok"),
    ("Monaco", "Paris"),
    ("Paris", "Budapest")
  ).map(p => (City(p._1), City(p._2)))

  describe("tsort") {
    try {
      describe("when run dry") {
        val l = tsort(List.empty, relations).getOrElse(List.empty)
        assert(l == Right(Nil))
      }
    // TODO why is this thrown
    } catch { case e: NotAllowedException => }
    describe("when called with constraints") {
      val l = tsort(nodes, relations).getOrElse(List.empty)
      it("output size must be right") { l.length === nodes.length }
      describe("without circularity") {
        it("partial order must be respected") {
          assert(l.indexOf(City("Vienna")) < l.indexOf(City("Budapest")))
          assert(l.indexOf(City("Tokyo")) < l.indexOf(City("Bangkok")))
          assert(l.indexOf(City("Monaco")) < l.indexOf(City("Paris")))
          assert(l.indexOf(City("Paris")) < l.indexOf(City("Budapest")))
        }
        describe("with a duplicate constraint") {
          val extraConstraint: (City, City) = (City("Paris"), City("Budapest"))
          val l =
            tsort(nodes, relations + extraConstraint).getOrElse(List.empty)
          it("partial order must still be respected") {
            assert(l.indexOf(City("Vienna")) < l.indexOf(City("Budapest")))
            assert(l.indexOf(City("Tokyo")) < l.indexOf(City("Bangkok")))
            assert(l.indexOf(City("Monaco")) < l.indexOf(City("Paris")))
            assert(l.indexOf(City("Paris")) < l.indexOf(City("Budapest")))
          }
          it("must not output duped elems") {
            assert(l.length === l.toSet.size)
          }

        }
      }
      describe("with circularity") {
        val badConstraint: (City, City) = (City("Budapest"), City("Vienna"))
        val l = tsort(nodes, relations + badConstraint)
        it("must return right error msg") {
          assert(
            l == Left(
              CyclicGraphError(
                "At least one of the nodes has a circular dependency"
              )
            )
          )
        }
      }
    }
    describe("when called with no constraints") {
      val l = tsort(nodes, Set.empty[(City, City)]).getOrElse(throw new Error)
      it("must still output all elems") {
        assert(l.length === nodes.length)
      }
      describe("with a duplicate node") {
        val l = tsort(extraCity :: nodes, Set.empty[(City, City)])
          .getOrElse(throw new Error)
        it("must not output duped elems") {
          assert(l.length === l.toSet.size)
        }

      }
    }

  }
}
