import scala.util.Either

// These function implementations are totally described by their type-signatures
def curry[A,B,C](f: (A,B) => C): A => B => C = a => { b => f(a,b) }

def uncurry[A,B,C](f: A => B => C): (A,B) => C = (a,b) => f(a)(b)

def compose[A,B,C](f: B => C, g: A => B): A => C = a => f(g(a))

def andThen[A,B,C](f: A => B, g: B => C): A => C = a => g(f(a))

def lift[A,B](fa: A => B): Option[A] => Option[B] = _ map fa

def lifted[A,B,C](fa: A => B)(f: => C): Option[A] => Either[C,B] = _.map(fa).toRight(f)
