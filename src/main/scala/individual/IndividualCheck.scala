package individual

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.{const, frequency}
import org.scalacheck.Prop.{forAll, collect}
import org.scalacheck.{Arbitrary, Gen, Properties}

import individual.IndividualTask.neededRangeAndTransform

object IndividualCheck extends Properties("neededRangeAndTransform") {
  lazy val integerInsideBounds = for {
    x <- Gen.choose(-200, 200)
    c <- Gen.choose(-10, 10)
    n <- Gen.choose(-10, 10)
  } yield (x, c, n)

  lazy val integerOutsideBounds = for {
    x <- Gen.choose(Int.MinValue, Int.MaxValue) suchThat (n => n < -200 || n > 200)
    c <- Gen.choose(-10, 10)
    n <- Gen.choose(-10, 10)
  } yield (x, c, n)

  property("apply on value in which the function IS NOT defined returns None") =
    forAll(integerInsideBounds)(neededRangeAndTransform(_) == None)

  property("apply on value in which the function IS defined returns Some(transformedValue)") =
    forAll(integerOutsideBounds)(x =>
      if (x._1 < -200) neededRangeAndTransform(x) == Some(x._1 + x._2)
      else if (x._1 > 200) neededRangeAndTransform(x) == Some(Math.pow(x._1, x._3).toInt)
      else false
    )
}