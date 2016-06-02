package doodle.backend

import doodle.core.{Color, Line, Stroke}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.language.existentials
import scala.language.implicitConversions

sealed trait ImageAttribute[T] {
  val value: T
  val valueType: Class[T]
}

final case class ColorAttr(value:Color) extends ImageAttribute[Color] {
  val valueType = classOf[Color]
}

final case class PositionAttr(value:Layout) extends ImageAttribute[Layout] {
  val valueType = classOf[Layout]
}

object Conversion {
  implicit def toAttr(value: Color) : ImageAttribute[Color] = ColorAttr(value)
  implicit def toAttr(value: Layout) : ImageAttribute[Layout] = PositionAttr(value)

}

sealed trait ImageAttributeList
final case class SomeAttributeList(attr:ImageAttribute[_], tail:ImageAttributeList) extends ImageAttributeList
case object EmptyAttributeList extends ImageAttributeList

sealed trait Image {
  val attributeList: ImageAttributeList = EmptyAttributeList
  def set[T](imageAttribute:ImageAttribute[T]) : Image =
    ImageWithAttributes(this, SomeAttributeList(imageAttribute, attributeList))

  def get[T](implicit t:ClassTag[T]): Option[T] = _getAttr[T](attributeList)
  def getOr[T](default: T)(implicit t:ClassTag[T]): T = get[T] match {
    case Some(t) => t
    case _ => default
  }

  @tailrec
  private def _getAttr[T](attrs:ImageAttributeList)(implicit t:ClassTag[T]) : Option[T] = {
    attrs match {
      case EmptyAttributeList => None
      case SomeAttributeList(attr, tail) =>
        if (attr.valueType == t.runtimeClass) Some(attr.value.asInstanceOf[T])
        else
          _getAttr[T](tail)
    }
  }
}

case class ImageWithAttributes(image: Image, override val attributeList: ImageAttributeList) extends Image {
  override def set[T](imageAttribute:ImageAttribute[T]) : Image =
    this.copy(attributeList = SomeAttributeList(imageAttribute, attributeList))
}

final case class Circle(r: Double) extends Image
final case class Rectangle(w: Double, h: Double) extends Image

sealed trait Layout
sealed trait RelativeLayout extends Layout {
  def relativeTo:Image
}

final case class On(relativeTo:Image) extends RelativeLayout
final case class Below(relativeTo:Image) extends RelativeLayout
final case class Right(relativeTo:Image) extends RelativeLayout
final case class Left(relativeTo:Image) extends RelativeLayout
final case class ExactPosition(x:Double, y:Double) extends Layout
case object DefaultPosition extends Layout

case class Point(x:Double, y:Double) {
  def `+`(other:Point) = Point(x + other.x, y + other.y)
  def `-`(other:Point) = Point(x - other.x, y - other.y)
}

case class BoundingBox(h:Double, w:Double)
object BoundingBox {
  def apply(a:Image): BoundingBox = {
    a match {
      case Circle(r) => new BoundingBox(2*r, 2*r)
      case Rectangle(w,h) => new BoundingBox(w, h)
      case ImageWithAttributes(image, attrs) => BoundingBox(image)
    }
  }
}

object Position {
  def apply(a:Image) : Point = {
    val layout = a.get[Layout] match {
      case None => DefaultPosition
      case Some(l) => l
    }
    val p = layout match {
      case On(r) => Position(r) + Point(0, BoundingBox(r).h)
      case Below(r) => Position(r) - Point(0, BoundingBox(a).h)
      case Right(r) => Position(r) + Point(BoundingBox(r).w, 0)
      case Left(r) => Position(r) - Point(BoundingBox(a).w, 0)
      case ExactPosition(x,y) => Point(x,y)
      case DefaultPosition => Point(0,0)
    }
    p
  }
}

object Offset {
  def apply(a:Image) : Point = {
    a match {
      case Circle(r) => Point(r,r)
      case Rectangle(w,h) => Point(0, h)
      case ImageWithAttributes(img, attr) => Offset(img)
      case _ => Point(0,0)
    }
  }
}

object Draw {
  def apply(canvas: Canvas, img:Image): Unit = {
    drawParent(canvas, img)
    val pos = Position(img) + Offset(img)
    println(pos)
    drawShape(canvas, img, pos)
    colorize(canvas, img)
  }

  def drawParent(canvas: Canvas, img:Image) = {
    for (l <- img.get[Layout]) {
      l match {
        case la: RelativeLayout => Draw(canvas, la.relativeTo)
        case _ => ()
      }
    }
  }

  def drawShape(canvas: Canvas, img:Image, pos:Point):Unit = {
    img match {
      case Circle(r) => canvas.circle(pos.x, pos.y, r)
      case Rectangle(w,h) => canvas.rectangle(pos.x, pos.y, w, h)
      case ImageWithAttributes(innerImg, _) => drawShape(canvas, innerImg, pos)
    }
  }

  def colorize(canvas: Canvas, img: Image) = {
    for (color <- img.get[Color]) {
      canvas.setStroke(Stroke(3.0, color, Line.Cap.Round, Line.Join.Round))
      canvas.setFill(color)
      canvas.fill()
    }
  }
}

object DoDraw  {
  def apply(canvas:Canvas): Unit = {
    import Conversion._
    canvas.setSize(1000, 1000)
    val c = Circle(10).set(Color.blue)
    val circle = c.set(Color.red).set(On(c))
    Draw(canvas, circle)
    Draw(canvas, c.set(Color.yellow).set(Below(c)))
    Draw(canvas, Rectangle(20,10).set(Color.green).set(Right(c)))
    Draw(canvas, Rectangle(20,20).set(Color.black).set(Left(c)))
    Draw(canvas, c)
    Draw(canvas, Rectangle(10,10).set(Color.yellow))
    val r = Rectangle(10,10).set(Color.red).set(On(c))
    val c2 = Circle(10).set(Color.blue).set(Left(r))
    Draw(canvas, r)
    Draw(canvas, Rectangle(10,10).set(Color.red).set(On(c)))
    Draw(canvas, c2)

    Draw(canvas,
      Rectangle(10, 20)
        .set(Color.blue)
        .set(On(
          Circle(10)
            .set(Color.blue)
            .set(Left(
              Rectangle(10,10)
                .set(Color.red).set(ExactPosition(100,100)))
            ))))
  }
}

