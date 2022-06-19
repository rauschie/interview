package emarsys

final case class City(name: String)

object City {
  def partialOrderingForCity(
      cities: Set[(City, City)]
  ): PartialOrdering[City] =
    new PartialOrdering[City] {
      override def tryCompare(x: City, y: City): Option[Int] = cities
          .find(p => {
            val s = Set(p._1, p._2)
            s.contains(x) && s.contains(y)
          })
          .map(p => if (p._1.equals(x)) 1 else -1)
      override def lteq(x: City, y: City): Boolean = x.equals(y)
    }

}
