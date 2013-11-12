package simulations

import math.random

class EpidemySimulator extends Simulator {
  var positions: List[List[(Int, Int)]] = List()
  def pushPositions: Unit =
    positions = persons.map(a => (a.row, a.col)) :: positions

  def randomBelow(i: Int) = (random * i).toInt
  def randomBoolean(chance: Int): Boolean =
    randomBelow(101) <= chance

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8
    
    val startInfectedChance: Int =  1
    val transmitChance     : Int = 40
    val deathChance        : Int = 25
    val moveChance         : Int = 20
    
    val incubationTime: Int =  6
    val dieTime       : Int = 14
    val immuneTime    : Int = 16
    val healTime      : Int = 18
    
    private def hasAny(pFn: Person => Boolean)(pos: (Int, Int)): Boolean = {
      val l = persons.filter(a => a.row == pos._1 && a.col == pos._2).map(pFn)
      
      if (l.isEmpty) false
      else           l.reduceLeft(_ || _)
    }
    private def hasAnyInfected = hasAny(_.infected)_
    private def hasAnyDead = hasAny(_.dead)_
    private def hasAnySick = hasAny(_.sick)_
    
    def move(p: Person) {
      def moveAction() {
        afterDelay(1) {
          val validNeighbors = if (p.sick) p.neighborRooms
                               else p.neighborRooms.filterNot(a => hasAnySick(a) || hasAnyDead(a))
          
          if (!validNeighbors.isEmpty && !p.dead && (p.daysUnmoved >= 5 || randomBoolean(moveChance))) {
            validNeighbors(randomBelow(validNeighbors.length)) match {
              case (x, y) => {
                p.row = x; p.col = y
                if (hasAnyInfected(p.row, p.col) && !p.immune && !p.infected)
                  p.infected = randomBoolean(transmitChance)
              }
            }
            p.daysUnmoved = 0
          } else p.daysUnmoved += 1
          
          move(p)
        }
      }
      
      p addAction moveAction
    }
    
    def incubate(p: Person) {
      def incubateAction() {
        afterDelay(1) {
          if (p.infected) {
            p.daysInfected += 1
            
            if (!p.dead && p.daysInfected >= healTime)        { p.infected = false; p.immune = false; p.daysInfected = 0 }
            else if (!p.dead && p.daysInfected >= immuneTime) { p.sick = false; p.immune = true; }
            else if (!p.dead && p.daysInfected >= dieTime)               { p.dead = randomBoolean(deathChance) }
            else if (!p.dead && p.daysInfected >= incubationTime) { p.sick = true }
          }
          
          incubate(p)
        }
      }
      
      p addAction incubateAction
    }
      
    def addAllActions(p: Person) {
      move(p)
      incubate(p)
    }
  }

  import SimConfig._

  val persons: List[Person] =
    (for (n <- 0 until population)
     yield {
      val p = new Person(n)
      if (n < 3) p.infected = true
      addAllActions(p)
      p
    }).toList
    
  pushPositions

  class Person (val id: Int) {
    private var actions: List[Action] = List()
    var infected = false
    var sick = false
    var immune = false
    var dead = false
    var daysUnmoved = 0
    var daysInfected = 0

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    //
    // to complete with simulation logic
    //
    def neighborRooms: List[(Int, Int)] =
      List((if (row - 1 < 0        ) roomRows - 1 else row - 1, col),
           (if (row + 1 >= roomRows) 0            else row + 1, col),
           (row                                               , if (col - 1 < 0           ) roomColumns - 1 else col - 1),
           (row                                               , if (col + 1 >= roomColumns) 0               else col + 1))
    
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }
}
