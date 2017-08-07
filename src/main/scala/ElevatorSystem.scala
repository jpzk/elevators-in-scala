package com.mwt.elevators

import scala.annotation.tailrec

// In general this problem is a TDTSP, there exist multiple heuristics 
// to solve this better in terms of efficiency of the elevator system.
// However, given the time frame I came up with the following solution.
//
// It is FCFS in a way that the elevator will fetch any passengers waiting
// when it is on the same floor, but it is queued in the elevator. But, it 
// will not interrupt the route for requests not on the route. 
//
// Assumption made: 
// - On floor 0 there is a higher probability that people arrive, so the 
//   elevators are optimistically waiting when idling on floor 0.
// - If people want to go up, and there is an elevator passing, they'll
//   enter the elevator (there is no stopping semantic).
//
// Behavior: 
// - As already said, when idling they will fetch people on a certain floor
//   or if there's nothing left to do, they will wait on floor 0.
//
// Design decisions:
// - I wanted to seperate data and strategy, so multiple strategies can be used
//   together or evaluated against each other using the same state. 
// - The state is indirectly encoded in the elevator state, for instance 'fetch'
//   for an order to pickup at a certain floor, it is also used to call the elevator
//   to floor 0. 
//
// Improvement of Semantics:
// - Don't fetch people with idling elevators which are fetched by other elevators, 
//   e.g. elevators on the same route. 
// - consider Up or Down indicator for request 
//
// Code improvement:
// - Write more tests
// - Encode state directly (fetching, delivering etc.) instead of indirectly
// - Better seperation of elevator system logic and strategy

class Direction
case object Up extends Direction
case object Down extends Direction

/**
 * A pickup request is used as an on-hold request, as well as a
 * passenger riding the elevator @see ElevatorState.
 *
 * @param f Floor where the person is waiting
 * @param d Direction where the person wants to go, Up or Down?
 * @param g Floor where person wants to go to
 */
case class PickupRequest(f: Int, d: Direction, g: Int) {
  override def toString = s"Request on Floor $f to in direction" + 
    s"$d to Floor $g"
}

/**
 * The elevator state is the state of one elevator in the system.
 *
 * @param id Id of the elevator in the system
 * @param floor Floor on which the elevator is on
 * @param reqs A sequence of pickup requests in process (riding passengers)
 * @param fetch Optional fetch / goto order 
 */
case class ElevatorState(id: Int, 
  floor: Int = 0, 
  reqs: Seq[PickupRequest] = Seq(),  // order in direction
  fetch: Option[Int] = None) { 
  
  override def toString = s"Elevator $id on $floor with $reqs"
  
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

  //@todo not used yet
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

/**
 * Overall state of the system
 *
 * @param reqs a sequence of pickup requests on hold
 * @param elevs a sequence of elevators in the system
 */
case class SystemState(
  reqs: Seq[PickupRequest],  
  elevs: Seq[ElevatorState]) {

  override def toString = s"Elevators: $elevs, Requests: $reqs"
  def status: Seq[ElevatorState] = this.elevs
} 

/**
 * Elevator Control System
 */
object ECS {
  def apply(elevs: Int): SystemState = {
    val states = Range(0, elevs).map(ElevatorState(_)) 
    SystemState(Seq(), states)
  }

  /**
   * Recursion used here for avoiding loading the same 
   * request by multiple elevators.
   *
   * @param s a rest sequence of pickup requests
   * @param e a rest sequence of elevators to consider
   * @param loaded the result sequence of eventually loaded elevators
   */
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

  // loading passenger when on the same floor
  // removing from on-hold requests, adding to elevator state
  def loadDirective(state: SystemState) = {
    val res = load(state.reqs, state.elevs, Seq())
    SystemState(res._1, res._2)
  }

  // unload the passenger if reached goto floor 
  def unloadDirective(state: SystemState) = state.copy(elevs = 
    state.elevs.map { e => e.copy(reqs = e.reqs.filter { r => 
        !e.isOnFloor(r.g) })}) 

  // go to fetch mode, when there's a request goto request floor 
  // otherwise when idle, goto 0
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

  // move elevator according to state of elevator
  def moveDirective(state: SystemState) = {
    state.copy(elevs = state.elevs.map { // fetching or delivering 
      case e if e.isFetching => e.move(e.directionFetch)
      case e if !e.isIdle => e.move(e.directionRequest)
      case e => e
    })
  }

  // overall step function containing a function stack of directives for 
  // the system.
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
  def benoulli(p: Float): Boolean = (p * 100) > r.nextInt(100)

  @tailrec
  def pickup(floors: Int): PickupRequest = 
    (r.nextInt(floors), r.nextInt(floors)) match {
      case (a,b) if (a == b) => pickup(floors)
      case (a,b) => PickupRequest(a, Up, b)
    }

  @tailrec
  def simulate(floors: Int, state: SystemState): SystemState = {
    val ground = if(benoulli(0.25f)) Seq(PickupRequest(0,Up,floors)) else Seq()
    val others = if(benoulli(0.1f)) Seq(pickup(floors)) else Seq()
    val newState = ECS.step(state, ground ++ others)
    println(newState)
    Thread.sleep(2000)
    simulate(floors, newState)
  }

  def main(args: Array[String]): Unit= { 
    simulate(5, ECS(elevs = 2))  
  }
}

