package gg

import scala.io._
import scala.collection.mutable.Map
import math._

class Graphs {
  //Init the edges of the graph
  var edges: List[(Int, Int)] = List()

  //Read table that holds graph, push each line into the edges list
  for (v <- Source.fromFile("g1.csv").getLines()) {
    edges = (v.split(", ")(0).toInt, v.split(", ")(1).toInt) :: edges
  }
  
  var nn: List[Int] = List()

  for ((e, r) <- edges) {
    nn = List(e, r) ::: nn
  }
  
  var node = nn.distinct.sortWith(_ < _)
  var new1 = 0
  var new2 = 0
  var pos = Map[Int, (Double, Double)]()
  var vel = Map[Int, (Double, Double)]()
  
  for (v <- node) {
    pos  += (v -> (v, v))
    vel += (v -> (0, 0))
  }
  def addNode() {
    val item = node.max+1
    node = node ++ List(item)
    pos  += (item -> (item, item))
    vel += (item -> (0, 0))
  }
  def addEdge(n1: Int, n2: Int) {
    edges = (n1, n2)  :: edges
  }
  def dist(aa: (Double, Double), bb: (Double, Double)): Double =  {
    math.sqrt(math.pow(aa._1 - bb._1, 2) + math.pow(aa._2 - bb._2, 2))
  }
  //remove node
  def remove(num: Int) = node diff List(num)
  
  //Degree function
  def degree(n: Int): Int = {
	edges.filter(x => x._1 == n || x._2 == n).length
  }
  
  //find all nodes that are adjacent
  def adj(n: Int): List[Int] = {
    edges.filter(x => x._1 == n || x._2 == n).map(x => List(x._1, x._2)).flatten.filter(_ != n).toList
  }
  
  //clustering coefficiant function
  def clusCo(n: Int): Double = {
    val x = degree(n)
    var count = 0
    val ad = adj(n)
    for  (m <- ad) {
      count += (ad intersect adj(m)).length
    }
    if (x < 2) -1 else (count / (x * (x - 1.0)))
  }
  
  //bfs function
  def bfs(node: Int): List[Int] = {
  
    //init all nodes unexplored
    val exp = scala.collection.mutable.Set(node)
    
    //Queue data structure
    val Q = scala.collection.mutable.Queue(node)
    
    //while the queue is not empty
    while (Q.nonEmpty) {
    
      //take the first element of the queue
      val v = Q.dequeue
      
      //this returns all of the nodes connected to that node
      val edge = edges.filter(x => x._1 == v).unzip._2 ++ edges.filter(x => x._2 == v).unzip._1
      
      //loop over all nodes 
      for (w <- edge) {
        
        //check whether the node has already been explored
        if (!(exp.contains(w))) {
          
          //add node to the set of explored nodes
          exp += w
          
          //add node to the end of the queue
          Q.enqueue(w)
        }
      }
    }
    //return the set of explored nodes as a list
    exp.toList
  } //end bfs
      
  var kinEnergy = 0
  val area = 600 * 600
  val kLen = 100
  val k = sqrt(area / node.length)
  def force() {
    //graph layout
    //println(shit.edges.length)
    //println(shit.edges.distinct.length)
    //check that these are the same or else have function move ofending nodes
    for (x <- node) {
      var netForce = (0., 0.)
      //for each other node
      for (y <- remove(x)) {
        var distance = dist(pos(x), pos(y))
        var kx = -(5000000 / pow(distance, 2))
        var ang = (atan2(pos(y)._2 - pos(x)._2, pos(y)._1 - pos(x)._1))
        netForce = (netForce._1 + (kx * cos(ang)), netForce._2 + (kx * sin(ang)))
      }
      
      for (y <- adj(x)) {
        var cx = (100 * (dist(pos(x), pos(y)) - kLen))
        var ang = (atan2(pos(y)._2 - pos(x)._2, pos(y)._1 - pos(x)._1))
        netForce = (netForce._1 + (cx * cos(ang)), netForce._2 + (cx * sin(ang)))     
      }
      if (netForce._1 > 1000) {
        netForce = (1000, netForce._2)
      }
      if (netForce._2 > 1000) {
        netForce = (netForce._1, 1000)
      }
      vel(x) = ((vel(x)._1 + .01 * netForce._1)*.8, (vel(x)._2 + .01 * netForce._2)*.8)
      
      pos(x) = ((pos(x)._1 + .01 * vel(x)._1), (pos(x)._2 + .01 * vel(x)._2))

      if (pos(x)._1 > 600) {
        pos(x) = (600, pos(x)._2)
        vel(x) = (-20+vel(x)._1, vel(x)._2)
      }
      if (pos(x)._2 > 600) {
        pos(x) = (pos(x)._1, 600)
        vel(x) = (vel(x)._1, -20+vel(x)._2)
      }
      if (pos(x)._1 < 30) {
        pos(x) = (30, pos(x)._2)
        vel(x) = (20+vel(x)._1, vel(x)._2)
      }
      if (pos(x)._2 < 30) {
        pos(x) = (pos(x)._1, 30)
        vel(x) = (vel(x)._1, 20+vel(x)._2)
      }
      kinEnergy = kinEnergy + degree(x) * (pow(vel(x)._1, 2) + pow(vel(x)._2, 2)).toInt
    }
  }//end force
}