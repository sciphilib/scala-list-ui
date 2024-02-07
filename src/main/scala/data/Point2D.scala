import scala.util.Random

case class Point2D(x: Double, y: Double) {
  override def toString: String = s"Point2D($x, $y)"
}

object Point2D {
  def randomPoint(maxX: Double, maxY: Double): Point2D = {
    val randomX = Random.nextDouble() * maxX
    val randomY = Random.nextDouble() * maxY
    Point2D(randomX, randomY)
  }
}

object Point2DImplicits {
  implicit val point2DOrdering: Ordering[Point2D] = new Ordering[Point2D] {
    def compare(a: Point2D, b: Point2D): Int = {
      val distanceA = Math.sqrt(a.x * a.x + a.y * a.y)
      val distanceB = Math.sqrt(b.x * b.x + b.y * b.y)
      distanceA compare distanceB
    }
  }
}
