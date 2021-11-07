package individual

import org.scalacheck.Prop
import org.scalacheck.Properties
import org.junit._

import org.scalacheck.Arbitrary._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Test.{check, Result, Failed, PropException}

import individual.IndividualCheck

class IndividualTaskSuite {
  def asProp(properties: Properties): Prop = Prop.all(properties.properties.map(_._2).toSeq:_*)

  @Test def `PartialFunction neededRangeAndTransform satisfies all properties`: Unit =
    Assert.assertTrue(
      check(asProp(IndividualCheck))(identity).passed
    )

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
