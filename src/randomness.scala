sealed trait Random[A] {
  def run(rng: scala.util.Random):A
  def map[B](f: A => B): Random[B] = {
    new Primitive(rng => f(run(rng)))
  }

  def flatMap[B](f: A => Random[B]): Random[B] = {
    new Primitive(rng => f(run(rng)).run(rng))
  }

  def zip[B](that: Random[B]): Random[(A,B)] =
    this flatMap { a =>
      that map { b =>
        (a, b)
      }
    }
}

class Primitive[A](f: scala.util.Random => A) extends Random[A] {
  def run(rng: scala.util.Random):A = f(rng)
}

object Random {
  def int: Random[Int] = new Primitive(_.nextInt)
  def double: Random[Double] = new Primitive(_.nextDouble)
  def always[A](in:A) = new Primitive(_ => in)
  def point(x:Random[Int], y:Random[Int]): Random[Point] = x.flatMap(_x => y.map(_y => new Point(_x, _y)))
  def point: Random[Point] = point(Random.int, Random.int)
}

case class Point(x:Int, y:Int)

//class RandomPoint extends Random[Point] {

//}

object Test extends App {
  val r = scala.util.Random
  val i = Random.always(1)
  val j = Random.int
  println(i.run(r))
  println(j.run(r))
  println(i.zip(j).run(r))
  val point = Random.point
  val point2 = Random.point(i,j)
  println(point.run(r))
  println(point2.run(r))
}
