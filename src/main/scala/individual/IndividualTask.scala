package individual

/** Variant 11 */
object IndividualTask {
  def fullyTransform: PartialFunction[(Int, Int, Int), Int] = {
    case x if x._1 < -200 => x._1 + x._2
    case x if x._1 > 200 => Math.pow(x._1, x._3).toInt
  }
  def neededRangeAndTransform = fullyTransform.lift
}