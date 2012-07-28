package gg

import processing.core._
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.Graph
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.Direction
import scala.math._

object Gapp extends PApplet{

  def main(args: Array[String]): Unit = {
    var frame = new javax.swing.JFrame("Network Graph")
    var applet = Gapp
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  } 

  var nw = new Graphs  
  
  for (x <- nw.node) {
    nw.pos(x) = (random(600).toInt, random(600).toInt)
  }
  
  override def setup(): Unit = {
    size(1200, 1200) 
  }

  override def mouseReleased() {
    if (button == 1) {
      button = 0
    } else if (button == 3) {
      button = 4
    } else {
      button = 0
    }
  }
  
  override def mouseClicked() {
    if (button == 0) {
      button = 1
    }
  }
  
  var button = 0
  

  override def draw(): Unit = {
    background(51)
    rect(20, 20, 570, 570)
    cursor(processing.core.PConstants.ARROW)
    nw.force()
    fill(0) 
      rect(700, 140, 100, 50)
      rect(700, 240, 100, 50)
    fill(25)
    text("Add Node", 720, 170)
    text(button, 720, 370)
    text("Add Edge", 720, 270)
    if (button == 5) {
      nw.addEdge(nw.new1, nw.new2)
      button = 0
    }
    if ((mouseX > 700) && (mouseX < 800) &&
          (mouseY > 140) && (mouseY < 190)) {
      cursor(processing.core.PConstants.HAND)
      	if (button == 1)  {
        nw.addNode()
        button = 0
      }
    }
    if ((mouseX > 700) && (mouseX < 800) &&
          (mouseY > 240) && (mouseY < 290)) {
      cursor(processing.core.PConstants.HAND)
      	if (button == 1)  {
        button = 2
      }
    }

    println(nw.kinEnergy)
    for (x <- nw.node) {
      fill(250)
      ellipse(nw.pos(x)._1.toFloat, nw.pos(x)._2.toFloat, 10, 10)
    	
    }
    for (x <- nw.edges) {
    	line(nw.pos(x._1)._1.toFloat, nw.pos(x._1)._2.toFloat, nw.pos(x._2)._1.toFloat, nw.pos(x._2)._2.toFloat)
    }
    println(nw.edges)
    for (x <- nw.node) {
      if ((mouseX > nw.pos(x)._1 - 8) && 
          (mouseX < nw.pos(x)._1 + 8) &&
          (mouseY > nw.pos(x)._2 - 8) && 
          (mouseY < nw.pos(x)._2 + 8)) {
    	  	if (mousePressed) {
    	  	  nw.pos(x) = (mouseX, mouseY)
    	  	}
    	  	if (mousePressed && button == 2) {
    	  	  nw.new1 = x
    	  	  button = 3
    	  	}
    	  	if (mousePressed && button == 4) {
    	  	  nw.new2 = x
    	  	  button = 5
    	  	}
    	  	text("Node " + x, (nw.pos(x)._1+20).toFloat, nw.pos(x)._2.toFloat)
      }
         /* text("Node " + nw.node(x), nw.pos(x)._1+20, nw.pos(x)._2)
          text("Cluster Coefficient " + nw.clusCo(x), nw.pos(x)._1+20, nw.pos(x)._2 + 12)
      }*/
    }
  } //end draw
}


