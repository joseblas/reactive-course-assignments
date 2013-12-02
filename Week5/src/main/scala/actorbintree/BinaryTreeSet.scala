/**
 * Copyright (C) 2009-2013 Typesafe Inc. <http://www.typesafe.com>
 */
package actorbintree

import akka.actor._

object BinaryTreeSet {

  trait Operation {
    def requester: ActorRef
    def id: Int
    def elem: Int
  }

  trait OperationReply {
    def id: Int
  }

  /** Request with identifier `id` to insert an element `elem` into the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Insert(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to check whether an element `elem` is present
    * in the tree. The actor at reference `requester` should be notified when
    * this operation is completed.
    */
  case class Contains(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request with identifier `id` to remove the element `elem` from the tree.
    * The actor at reference `requester` should be notified when this operation
    * is completed.
    */
  case class Remove(requester: ActorRef, id: Int, elem: Int) extends Operation

  /** Request to perform garbage collection*/
  case object GC

  /** Holds the answer to the Contains request with identifier `id`.
    * `result` is true if and only if the element is present in the tree.
    */
  case class ContainsResult(id: Int, result: Boolean) extends OperationReply
  
  /** Message to signal successful completion of an insert or remove operation. */
  case class OperationFinished(id: Int) extends OperationReply

}


class BinaryTreeSet extends Actor {
  import BinaryTreeSet._
  import BinaryTreeNode._

  def createRoot: ActorRef = context.actorOf(BinaryTreeNode.props(0, initiallyRemoved = true))

  var root = createRoot
  
  // CUSTOM CODE
  var senders: Map[Int, ActorRef] = Map()
  def received(id: Int): Boolean = senders.get(id) match {
    case Some(_) => true
    case None    => false
  }
  
  var ops: List[Operation] = List()
  var isgc: Boolean = false

  def receive: Receive = {
    case Insert  (req, id, elem) =>
      if (!received(id) || sender == self) {
        if (sender != self) senders = senders + (id -> req)
        
        if (isgc) ops = ops :+ Insert(req, id, elem)
        else root ! Insert(req, id, elem)
      }
    
    case Contains(req, id, elem) =>
      if (!received(id) || sender == self) {
        if (sender != self) senders = senders + (id -> req)
        
        if (isgc) ops = ops :+ Contains(req, id, elem)
        else root ! Contains(req, id, elem)
      }
    
    case ContainsResult(id, ise) =>
      senders(id) ! ContainsResult(id, ise)
    
    case Remove  (req, id, elem) =>
      if (!received(id) || sender == self) {
        if (sender != self) senders = senders + (id -> req)
        
        if (isgc) ops = ops :+ Remove(req, id, elem)
        else root ! Remove(req, id, elem)
      }
      
    case GC => root ! CopyTo(createRoot)
    case CopyFinished => root ! "StopRemoved"
    case "RemovedStopped" => {
      isgc = false
      
      ops.foreach(self ! _)
      ops = List()
    }
    
    case _ => Unit
  }
  
  def garbageCollection(newRoot: ActorRef): Unit = {
    root ! CopyTo(newRoot)
    root ! CleanReq
    root = newRoot
    
    isgc = false
    
    ops.foreach(self ! _)
    ops = List()
  }
}

object BinaryTreeNode {
  case object CleanReq
  
  trait Position

  case object Left extends Position
  case object Right extends Position

  case class CopyTo(treeNode: ActorRef)
  case object CopyFinished

  def props(elem: Int, initiallyRemoved: Boolean) = Props(classOf[BinaryTreeNode],  elem, initiallyRemoved)
}

class BinaryTreeNode(val elem: Int, initiallyRemoved: Boolean) extends Actor {
  import BinaryTreeNode._
  import BinaryTreeSet._

  var subtrees = Map[Position, ActorRef]()
  
  def l: Option[ActorRef] = subtrees.get(Left )
  def hasL: Boolean = l match { case Some(_) => true; case None => false }
  def lRaw: ActorRef = subtrees(Left )
  def r: Option[ActorRef] = subtrees.get(Right)
  def hasR: Boolean = r match { case Some(_) => true; case None => false }
  def rRaw: ActorRef = subtrees(Right)
  
  var removed = initiallyRemoved

  // optional
  def receive = normal

  // optional
  /** Handles `Operation` messages and `CopyTo` requests. */
  val normal: Receive = {
    case Insert  (req, id, sele) => {
      if (sele == elem && removed) {
        removed = false
        req ! OperationFinished(id)
      }
      else if (sele < elem)
        if (hasL) lRaw ! Insert(req, id, sele)
        else {
          subtrees = subtrees + (Left -> context.actorOf(props(sele, false)))
          req ! OperationFinished(id)
        }
      else if (sele > elem)
        if (hasR) rRaw ! Insert(req, id, sele)
        else {
          subtrees = subtrees + (Right -> context.actorOf(props(sele, false)))
          req ! OperationFinished(id)
        }
      else req ! OperationFinished(id)
    }
    
    case Contains(req, id, sele) =>
           if (sele == elem && !removed) req  ! ContainsResult(id, true     )
      else if (sele < elem  && hasL    ) lRaw ! Contains      (req, id, sele)
      else if (sele > elem  && hasR    ) rRaw ! Contains      (req, id, sele)
      else                               req  ! ContainsResult(id, false    )
    
    case Remove  (req, id, sele) =>
           if (sele == elem       ) { 
        removed = true
        req ! OperationFinished(id)
      }
      else if (sele < elem && hasL) lRaw ! Remove(req, id, sele)
      else if (sele > elem && hasR) rRaw ! Remove(req, id, sele)
      else                          req  ! OperationFinished(id)
    
    case CopyTo      (target) => {
      if (!removed) target ! Insert(self, 0, elem)
      
      if (hasL) lRaw ! CopyTo(target)
      if (hasR) rRaw ! CopyTo(target)
      
      context.parent ! CopyFinished
    }
    
    case CopyFinished         => context.parent ! CopyFinished
    
    case CleanReq => {
      l match {
        case Some(l) => l ! CleanReq
        case None    => Unit
      }
      
      r match {
        case Some(r) => r ! CleanReq
        case None    => Unit
      }
      
      if (removed) self ! PoisonPill
    }
    
    case _ => Unit
  }
}
