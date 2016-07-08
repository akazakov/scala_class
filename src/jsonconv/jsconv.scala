package jsonconv

sealed trait JsValue
case object JsNull extends JsValue
final case class JsString(value: String) extends JsValue
final case class JsNumber(value: Double) extends JsValue
final case class JsBoolean(value: Boolean) extends JsValue
final case class JsArray(values: Seq[JsValue]) extends JsValue
final case class JsObject(values: Seq[(String, JsValue)]) extends JsValue

trait JsWriter[A]  {
  def write(v:A):JsValue
}

object JsWriter {

  implicit object DoubleJsWriter extends JsWriter[Double] {
    def write(v: Double) = {
      new JsNumber(v)
    }
  }

  implicit object IntJsWriter extends JsWriter[Int] {
    def write(v: Int) = {
      new JsNumber(v.toDouble)
    }
  }
}

object JsonSyntax {
  implicit class JsonNumber[A](val v:A)(implicit writer:JsWriter[A]) {
    def toJson:JsValue = {
      writer.write(v)
    }
  }
}

object JsUtil {
  def toJson[A](v:A)(implicit writer:JsWriter[A]) = {
    writer.write(v)
  }
}

object ttt extends App {
  import JsonSyntax._
  println(JsUtil.toJson(1))
  println(1.toJson)
  println(3.0.toJson)
}
