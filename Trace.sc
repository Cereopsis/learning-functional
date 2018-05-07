import cats.Eval
import scala.concurrent.{Future,ExecutionContext}
import scala.util.{Success,Failure}
import java.time.Instant

def networkOp[A](a: => A, delay: Long = 100L): Future[A] = Future {
  val result = a
  Thread.sleep(delay)
  result
}

object Trace {
  
  def start(id: String) = println(s"Start trace: $id at ${Instant.now}")
  def end(id: String)   = println(s"End trace: $id at ${Instant.now}")
  
  def apply[A](id: String, a: => Future[A]): Eval[Future[A]] = Eval.later{
      start(id)
      a map { elem =>
        end(id)
        elem
      } recover {
        case e: Throwable => 
          end(id)
          throw e
      }
    }
}

def run[A,B](
    eval: Eval[Future[A]]
  )(
    pf: PartialFunction[A,B]
  ): Future[B] = eval.value collect pf
  

val tr = Trace("123456", networkOp("GET localhost:8080 /"))

run(tr){
   case r if r.startsWith("GET") => "Hoo Ha!"
 } onComplete {
   case Success(s) => println(s)
   case Failure(e) => println("Sucks")
 }
