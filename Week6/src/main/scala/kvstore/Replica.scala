package kvstore

import akka.actor.{ OneForOneStrategy, Props, ActorRef, Actor }
import kvstore.Arbiter._
import scala.collection.immutable.Queue
import akka.actor.SupervisorStrategy.Restart
import scala.annotation.tailrec
import akka.pattern.{ ask, pipe }
import akka.actor.Terminated
import scala.concurrent.duration._
import akka.actor.PoisonPill
import akka.actor.OneForOneStrategy
import akka.actor.SupervisorStrategy
import akka.util.Timeout

import scala.concurrent.{ Promise, Future }

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  
  arbiter ! Join
  
  // Creates a future that finishes after a duration
  // which performs a given function
  def delayFuture(d: Duration): Future[Unit] = {
    val p = Promise[Unit]
    
    Future { Thread.sleep(d.toMillis) } onComplete { _ => p.success(Unit) }
    
    p.future
  }
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  
  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  def receive = {
    case JoinedPrimary   => context.become(leader)
    case JoinedSecondary => context.become(replica)
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    // Inserting a kv pair
    case Insert   (key, value, id) => {
      delayFuture(1 second) onComplete(_ => context.sender ! OperationFailed(id))
      
      kv = kv + (key -> value)
      secondaries.foreach(_._1 ! Replicate(key, Some(value), id))
      secondaries.map(_ match { case (rd, r) => rd ! Replicate(key, Some(value), id) })
      
      context.sender ! OperationAck(id)
    }
    
    // Removing a kv pair
    case Remove   (key,        id) => {
      delayFuture(1 second) onComplete(_ => context.sender ! OperationFailed(id))
      
      kv = kv - key
      secondaries.foreach(_._1 ! Replicate(key, None       , id))
      
      context.sender ! OperationAck(id)
      
    }
    
    // Accessing a kv pair
    case Get      (key,        id) =>
      context.sender ! GetResult(key, kv.get(key), id)
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Get      (key,        id) =>
      context.sender ! GetResult(key, kv.get(key), id)
      
    case Replicate(key, value, id) => {
      value match {
        case Some(v) => kv = kv + (key -> v)
        case None    => kv = kv - key
      }
      
      secondaries(self) ! Snapshot(key, value, 0) // TODO: Find out how to use seq
      context.sender ! Replicated(key, id)
    }
  }

}
