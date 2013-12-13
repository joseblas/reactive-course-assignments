package kvstore

import scala.concurrent.duration.Duration
import scala.concurrent.{ Promise, Future }
import scala.concurrent.ExecutionContext.Implicits.global

object Utils {
  // Creates a future that finishes after a duration
  // which performs a given function
  def delayFuture(d: Duration): Future[Unit] = {
    val p = Promise[Unit]
    
    Future { Thread.sleep(d.toMillis) } onComplete { _ => p.success(Unit) }
    
    p.future
  }
}