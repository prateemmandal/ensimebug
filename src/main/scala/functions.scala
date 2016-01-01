object Functional {
  def foldr[A, B](list: DataStructures.List[A], acc: B)(f: (A, B) => B): B = list match {
    case DataStructures.Nil => acc
    case DataStructures.Cons(x, xs) => f(x, foldr(xs, acc)(f))
  }

  def foldl[A, B](list: DataStructures.List[A], acc: B)(f: (B, A) => B): B = list match {
    case DataStructures.Nil => acc
    case DataStructures.Cons(x, xs) => foldl(xs, f(acc, x))(f)
  }

}

object DataStructures {
  sealed abstract class List[+A]
  case object Nil extends List[Nothing]
  case class Cons[+A](head: A, tail: List[A]) extends List[A]
  object List {
    def length[A](list: List[A]): BigInt = list match {
      case Nil => 0
      case Cons(x, xs) => 1 + length(xs)
    }
  }
  val numlist = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
  val numlistfrzeroprimed = Functional.foldr(numlist, 0)_
  val numlistfroneprimed = Functional.foldr(numlist, 1)_
  val numlistflzeroprimed = Functional.foldl(numlist, 0)_
  val numlistfloneprimed = Functional.foldl(numlist, 1)_
  val frsum = numlistfrzeroprimed(_ + _)
  val flsum = numlistflzeroprimed(_ + _)
  val frprod = numlistfroneprimed(_ * _)
  val flprod = numlistfloneprimed(_ * _)
  }
}


