package emarsys

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import City.partialOrderingForCity


@RunWith(classOf[JUnitRunner])
class PartialOrderingTest extends AnyFunSuite {
val constraints = Set(
(City("Budapest"), City("Vienna"))
)
val po = partialOrderingForCity(constraints)

test("partialOrderingForCity.tryCompare returns correct value if constraint present"){
    assert(po.tryCompare(City("Budapest"),City("Vienna")) == Some(1))
    assert(po.tryCompare(City("Vienna"),City("Budapest")) == Some(-1))
}
test("partialOrderingForCity.tryCompare returns None if no constraint present"){
    assert(
    po.tryCompare(
    City("notOnTheList"),City("defoNotOnTheList")
    ) == None)
}
test("partialOrderingForCity. returns None if no constraint present"){
    assert(
    partialOrderingForCity(constraints).tryCompare(
    City("notOnTheList"),City("defoNotOnTheList")
    ) == None)
}
  
}
