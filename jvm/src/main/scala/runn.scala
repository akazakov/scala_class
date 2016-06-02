package doodle
import doodle.jvm.Java2DCanvas
import doodle.backend.DoDraw


/**
  * Created by artemkazakov on 5/30/16.
  */
object runn {
  def apply = {
    val canvas = Java2DCanvas.canvas
    DoDraw(canvas)
  }
}
