/**
  * Created by artemkazakov on 6/30/16.
  */
sealed trait Random[A] {
  def run(rng: scala.util.Random):A
  def map[B](f: A => B): Random[B] = {
    new WithOtherRandom(this, f)
  }

  def flatMap[B](f: A => Random[B]): Random[B] = {
    new WithF(rng => f(run(rng)).run(rng))
  }

  def zip[B](that: Random[B]): Random[(A,B)] =
    this flatMap { a =>
      that map { b =>
        (a, b)
      }
    }
}

class Always[A](in:A) extends Random[A] {
  def run(rng: scala.util.Random):A = in
}

class WithF[A](f: scala.util.Random => A) extends Random[A] {
  def run(rng: scala.util.Random):A = f(rng)
}

class WithOtherRandom[A,B](other:Random[A], f:A => B) extends Random[B] {
  def run(rng: scala.util.Random):B = f(other.run(rng))
}

object Random {
  def int: Random[Int] = new WithF(_.nextInt)
  def double: Random[Double] = new WithF(_.nextDouble)
  def always[A](in:A) = new Always(in)
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
