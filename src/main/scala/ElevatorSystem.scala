package com.mwt.elevators

import scala.annotation.tailrec

// immutable state solution, seperation of data and strategies / directives
//
// - directives as functions, stacked transitions (easy testing)
//
// considering:
// - put the elevators to fetch which are idling
//   - prioritize requests on floors, if no requests, then go to floor 0
//     most probable that requests are here.
//
// should also consider:
// - dont fetch people with idling elevators which are fetched by elevators 
//   are on the same route and will pick them up nevertheless
//
// improve:
// - encode state directly (fetching, delivering etc.) instead of indirectly
//

class Direction
case object Up extends Direction
case object Down extends Direction

// data model for in elevator
case class PickupRequest(f: Int, d: Direction, g: Int) {
  override def toString = s"Request on Floor $f to in direction" + 
    s"$d to Floor $g"
}

case class ElevatorState(id: Int, 
  floor: Int = 0, 
  reqs: Seq[PickupRequest] = Seq(),  // order in direction
  fetch: Option[Int] = None) { 
  
  override def toString = s"Elevator $id on $floor with $reqs"
  def copyLog(id: Int = id, floor: Int = floor, 
    reqs: Seq[PickupRequest] = reqs, fetch: Option[Int] = fetch) = { 
      val newS = this.copy(id, floor, reqs, fetch)
      println(s"$this -> $newS")
      newS
  }
  
  def isIdle: Boolean = reqs.isEmpty && fetch.isEmpty
  def isFetching: Boolean = fetch.isDefined
  def isOnFloor(f: Int) = f == floor 

  def directionRequest = direction(reqs.head.g)
  def directionFetch = direction(fetch.get) // a bit ugly
  def direction(target: Int): Direction = (target - floor) match {
    case d if d > 0 => Up
    case d if d < 0 => Down
    case _ => throw new Exception("On the same floor")
  }

  def onWay(f: Int) = directionRequest match {
    case Up => (floor < f) && (f < reqs.head.g)
    case Down => (reqs.head.g < f) && (f < floor)
  }
  
  // define helper functions here on the state
  def unload() = this.copy(reqs = reqs.tail)
  def load(rs: Seq[PickupRequest]) = this.copy(reqs = (reqs ++ rs))  
  def fetch(floor: Int) = this.copy(fetch = Some(floor))
  def move(d: Direction) = {
    val nf = d match {
      case Up => this.floor + 1
      case Down => this.floor - 1
    }
    this.copy(floor = nf)
  }
}

case class SystemState(
  reqs: Seq[PickupRequest],  
  elevs: Seq[ElevatorState]) {

  override def toString = s"Elevators: $elevs, Requests: $reqs"
  def status: Seq[ElevatorState] = this.elevs
} 

// for initializing"
object ECS {
  def apply(elevs: Int): SystemState = {
    val states = Range(0, elevs).map(ElevatorState(_)) 
    SystemState(Seq(), states)
  }

  // avoiding loading the same request by both elevators
  @tailrec
  def load(s: Seq[PickupRequest], e: Seq[ElevatorState], 
    loaded: Seq[ElevatorState]): (Seq[PickupRequest], Seq[ElevatorState]) = 
    e.toList match {
      case Nil => (s, loaded)
      case (x :: tail) => 
        val loadable = s.filter { r => x.isOnFloor(r.f) } 
        val rest = s.filter { r => !x.isOnFloor(r.f) } 
        load(rest, tail, loaded ++ Seq(x.load(loadable)))
    }
  def loadDirective(state: SystemState) = {
    val res = load(state.reqs, state.elevs, Seq())
    SystemState(res._1, res._2)
  }

  def unloadDirective(state: SystemState) = state.copy(elevs = 
    state.elevs.map { e => e.copy(reqs = e.reqs.filter { r => 
        !e.isOnFloor(r.g) })}) 

  // go to fetch mode when idle, goto 0
  def fetchIdleDirective(state: SystemState) = {
   state.copy(elevs = state.elevs
     .map { // fetch directive when idling
      case e if e.isIdle => 
        state.reqs match {
          case rs if rs.size > 0 => e.copy(fetch = Some(rs.head.f))
          case rs => if(e.floor > 0) e.copy(fetch = Some(0)) else e
        }
      case e => e 
    } 
    .map { e => // remove fetch directive when reached floor
       e.fetch match {
        case Some(f) => if(e.isOnFloor(f)) e.copy(fetch = None) else e
        case _ => e
       }
    })
  }

  def moveDirective(state: SystemState) = {
    state.copy(elevs = state.elevs.map { // fetching or delivering 
      case e if e.isFetching => e.move(e.directionFetch)
      case e if !e.isIdle => e.move(e.directionRequest)
      case e => e
    })
  }

  def step(state: SystemState, newReqs: Seq[PickupRequest] = Seq()): 
    SystemState = { 
      val transition = 
        loadDirective _ andThen
        fetchIdleDirective _ andThen
        moveDirective _ andThen
        unloadDirective _

      transition(state.copy(reqs = state.reqs ++ newReqs))
    }
}

object Main { 
  val r = scala.util.Random
  val Floors = 10
  def benoulli(p: Float): Boolean = (p * 100) > r.nextInt(100)

  def pickup = {
    val a = r.nextInt(Floors)
    val b = r.nextInt(Floors)
    if(a == b) {
      PickupRequest(a, Up, b + 1)
    }else {
      PickupRequest(a, Up, b)
    }
  }

  @tailrec
  def simulate(state: SystemState): SystemState = {
    val ground = if(benoulli(0.25f)) Seq(pickup) else Seq()
    val others = if(benoulli(0.1f)) Seq(pickup) else Seq()

    val newState = ECS.step(state, ground ++ others)
    println(newState)
    Thread.sleep(2000)
    simulate(newState)
  }

  def main(args: Array[String]): Unit= { 
    simulate(ECS(elevs = 2))  
  }
}

