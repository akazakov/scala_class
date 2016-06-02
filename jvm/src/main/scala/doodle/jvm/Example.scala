package doodle
package jvm

import doodle.core._
import doodle.backend._

object Example  {
  def draw: Unit = {
    val canvas = Java2DCanvas.canvas

    // The coordinates for this (upside down) dog in the style of Picasso comes
    // from a Jeremy Kun:
    // http://jeremykun.com/2013/05/11/bezier-curves-and-picasso/

    canvas.setSize(500, 500)
    canvas.setOrigin(-250, 200)
    canvas.beginPath()
    canvas.moveTo(180,280)
    canvas.bezierCurveTo(183,268, 186,256, 189,244) // front leg
    canvas.moveTo(191,244)
    canvas.bezierCurveTo(290,244, 300,230, 339,245)
    canvas.moveTo(340,246)
    canvas.bezierCurveTo(350,290, 360,300, 355,210)
    canvas.moveTo(353,210)
    canvas.bezierCurveTo(370,207, 380,196, 375,193)
    canvas.moveTo(375,193)
    canvas.bezierCurveTo(310,220, 190,220, 164,205) // back
    canvas.moveTo(164,205)
    canvas.bezierCurveTo(135,194, 135,265, 153,275) // ear start
    canvas.moveTo(153,275)
    canvas.bezierCurveTo(168,275, 170,180, 150,190) // ear end + head
    canvas.moveTo(149,190)
    canvas.bezierCurveTo(122,214, 142,204, 85,240)  // nose bridge
    canvas.moveTo(86,240)
    canvas.bezierCurveTo(100,247, 125,233, 140,238)   // mouth
    canvas.endPath()
    canvas.setStroke(Stroke(3.0, Color.black, Line.Cap.Round, Line.Join.Round))
    canvas.stroke()
  }
}
