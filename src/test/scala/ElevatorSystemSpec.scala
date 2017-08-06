package com.mwt.elevators

import org.scalatest.{FlatSpec, Matchers}

class ElevatorSystemSpec extends FlatSpec with Matchers { 
  behavior of "ElevatorSystem"

  it should "fetch when idling, request other floor" in {
    val requests = Seq(PickupRequest(3, Up, 4))
    val end = ECS.step(SystemState(requests, Seq(ElevatorState(0,1))))
    end.elevs(0).fetch shouldEqual Some(3)
  }

  it should "remove fetch directive when on floor" in {
    val end = ECS.step(ECS.step(SystemState(Seq(), Seq(ElevatorState(0,1)))))
    end.elevs(0).fetch shouldEqual None
  }

  it should "re-prioritize by distance ASC" in {
    val requests = Seq(PickupRequest(3, Up, 4), PickupRequest(2, Up, 3))
    val end = ECS.step(SystemState(Seq(), Seq(ElevatorState(0,1, requests))))
    end.elevs(0).reqs(0).f shouldEqual 2
  }

  it should "re-prioritize by distance DESC" in {
    val requests = Seq(PickupRequest(3, Down, 2), PickupRequest(2, Down, 1))
    val end = ECS.step(SystemState(Seq(), Seq(ElevatorState(0,5, requests))))
    end.elevs(0).directionRequest shouldEqual Down
    end.elevs(0).reqs.head.f shouldEqual 3 
  }

  it should "initialize elevators correctly" in {
    ECS(elevs = 10).status.forall { case (elev) => 
      elev.floor == ECS.Ground && elev.reqs.isEmpty && (!elev.fetch.isDefined)
    } shouldEqual true
  }

  it should "empty elevators to floor zero" in {
    val s = ECS(elevs = 1).copy(elevs = Seq(ElevatorState(0, 1)))
    ECS.step(s).elevs(0).fetch shouldEqual Some(0)
  }

  it should "load when on same floor and there is a request" in {
    val s = ECS(elevs = 1).copy(elevs = Seq(ElevatorState(0, 1)))
    val end = ECS.step(s, Seq(PickupRequest(1, Up, 3)))
    end.elevs(0).reqs.size shouldEqual 1
  }

  it should "unload when on same floor" in {
    val requests = Seq(PickupRequest(2, Up, 3))
    val end = ECS.step(ECS.step(SystemState(Seq(), Seq(ElevatorState(0,1, requests)))))
    end.elevs(0).reqs.size shouldEqual 0 
  }
}

