/*
 * This Scala source file was generated by the Gradle 'init' task.
 */
package emarsys
import scala.io.StdIn.readLine
import scala.annotation.tailrec
import emarsys.tsort
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] =
    repl(List.empty, Set.empty).as(ExitCode.Success)

  def repl(cities: List[String], edges: Set[(String, String)]): IO[Unit] = for {
    _ <- IO(println(tsort(cities, edges)))
    line <- IO(readLine().trim())
    token <- line match {
      case "" => IO(ExitCode.Success)
      case edge if edge.contains("=>") =>
        edge
          .split("=>")
          .toList
          .map(_.trim) match {
          case head :: tail =>
            repl(cities, edges + Tuple2(head, tail.head))
          case Nil => repl(cities, edges)
        }
      case city => repl(city :: cities, edges)
    }
  } yield ()
}
