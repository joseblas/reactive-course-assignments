package kvstore

import akka.actor.Props
import akka.actor.Actor
import akka.actor.ActorRef
import scala.concurrent.duration._

object Replicator {
  case class Replicate(key: String, valueOption: Option[String], id: Long)
  case class Replicated(key: String, id: Long)
  case class Resend(r: Replicate)
  
  case class Snapshot(key: String, valueOption: Option[String], seq: Long)
  case class SnapshotAck(key: String, seq: Long)

  def props(replica: ActorRef): Props = Props(new Replicator(replica))
}

class Replicator(val replica: ActorRef) extends Actor {
  import Replicator._
  import Replica._
  import context.dispatcher
  
  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */
  var queued = List.empty[Any]
  
  var _seqCounter = 0L
  def nextSeq = {
    val ret = _seqCounter
    _seqCounter += 1
    ret
  }
  
  /* TODO Behavior for the Replicator. */
  def receive = normal
  
  def normal: Receive = {
    case r: Replicate => {
      context.become(sending)
      self ! Resend(r)
    }
  }
  
  def sending: Receive = {
    case r@Resend(Replicate(key, value, seq)) => {
      replica ! Snapshot(key, value, seq)
      Utils.delayFuture(50 milliseconds) onComplete { case _ => self ! r }
    }
    
    case s: SnapshotAck => {
      context.become(normal)
      queued.foreach(self ! _)
    }
    
    case a: Any => queued = queued :+ a
  }
}
