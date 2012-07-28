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
    var frame = new javax.swing.JFrame("Shit tards")
    var applet = Gapp
    frame.getContentPane().add(applet)
    applet.init
    frame.pack
    frame.setVisible(true)
  } 

  var shit = new Graphs  
  
  for (x <- shit.node) {
    shit.pos(x) = (random(600).toInt, random(600).toInt)
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
  
  /* set up button level management
   * different modes of button tells the tool what to do 
   * during draw state
   * button = 0 - regular draw model
   * button = 1 - clicked
   * button = 2 - edge mode
   */

  override def draw(): Unit = {
    background(51)
    cursor(processing.core.PConstants.ARROW)
    shit.force()
    fill(0) 
      rect(700, 140, 100, 50)
      rect(700, 240, 100, 50)
    fill(250)
    text("Add Node", 720, 170)
    text(button, 720, 370)
    text("Add Edge", 720, 270)
    if (button == 5) {
      shit.addEdge(shit.new1, shit.new2)
      button = 0
    }
    if ((mouseX > 700) && (mouseX < 800) &&
          (mouseY > 140) && (mouseY < 190)) {
      cursor(processing.core.PConstants.HAND)
      	if (button == 1)  {
        shit.addNode()
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

    println(shit.kinEnergy)
    for (x <- shit.node) {
      fill(250)
      ellipse(shit.pos(x)._1.toFloat, shit.pos(x)._2.toFloat, 10, 10)
    	
    }
    for (x <- shit.edges) {
    	line(shit.pos(x._1)._1.toFloat, shit.pos(x._1)._2.toFloat, shit.pos(x._2)._1.toFloat, shit.pos(x._2)._2.toFloat)
    }
    println(shit.edges)
    for (x <- shit.node) {
      if ((mouseX > shit.pos(x)._1 - 8) && 
          (mouseX < shit.pos(x)._1 + 8) &&
          (mouseY > shit.pos(x)._2 - 8) && 
          (mouseY < shit.pos(x)._2 + 8)) {
    	  	if (mousePressed) {
    	  	  shit.pos(x) = (mouseX, mouseY)
    	  	}
    	  	if (mousePressed && button == 2) {
    	  	  shit.new1 = x
    	  	  button = 3
    	  	}
    	  	if (mousePressed && button == 4) {
    	  	  shit.new2 = x
    	  	  button = 5
    	  	}
    	  	text("Node " + x, (shit.pos(x)._1+20).toFloat, shit.pos(x)._2.toFloat)
      }
         /* text("Node " + shit.node(x), shit.pos(x)._1+20, shit.pos(x)._2)
          text("Cluster Coefficient " + shit.clusCo(x), shit.pos(x)._1+20, shit.pos(x)._2 + 12)
      }*/
    }
  } //end draw
}


