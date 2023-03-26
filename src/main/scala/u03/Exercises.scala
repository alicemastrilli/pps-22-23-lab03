package u03

import u02.Modules.*
import Lists.*
import u02.Optionals.*
object Exercises :
  import List.*
  import Option.*

  // Task 1 - svolto da solo
    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
      case (Nil(), _) => Nil()
      case (Cons(h, t), 0) => Cons(h, t)
      case (Cons(_, t), n) => drop(t, n - 1)

    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), l2) => l2
      case (Cons(h1, t1), l2) => Cons(h1, append(t1, l2))

    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = (l, f) match
      case (Nil(), _) => Nil()
      case (Cons(h, t), f) => append(f(h), flatMap(t)(f))

    def newMap[A, B](l: List[A])(mapper: A => B): List[B] =
      flatMap(l)(v => Cons(mapper(v), Nil()))

    def newFilter[A](l1: List[A])(pred: A => Boolean): List[A] =
      flatMap(l1)(v => l1 match
        case Cons(_, _) if pred(v) => Cons(v, Nil())
        case _ => Nil())
  
  // Task 2 - svolto da solo
    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h,t) => _max(t, h)
      @annotation.tailrec
      private def _max(l: List[Int], m: Int): Option[Int] = l match
        case Nil() => Some(m)
        case Cons(h,t) => _max(t, math.max(m,h))
  // Task 3 - svolto da solo
    def getCoursesOfTeachers(l: List[Person]): List[String]=
      flatMap(newFilter(l)(v => !isStudent(v)))(t => Cons(Person.course(t), Nil()))
  
  // Task 4 - svolto da solo
    def foldLeft[A, B](l: List[A])(acc :B)(f: (B,A)=>B) : B = l match
      case Nil() => acc
      case Cons(h,t) => foldLeft(t)(f(acc, h))(f)

    def foldRight[A, B](l: List[A])(acc :B)(f: (A,B)=>B) : B = l match
      case Cons(h,t) => f(h, foldRight(t)(acc)(f))
      case Nil() => acc

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])
  object Stream:
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    def empty[A](): Stream[A] = Empty()


    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()
    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    // Task 5 - svolto da solo   
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (s, 0) => s
      case (Cons(_, tail), n) => drop(tail())(n-1)
      case _ => empty()

    // Task 6 - svolto da solo
    def constant[A](value: A): Stream[A] =
      cons(value, constant(value))

    // Task 7 - svolto da solo
    def fibs(): Stream[Int] =
      def _fibs(val1: Int, val2: Int): Stream[Int] =
        cons(val2, _fibs(val2, val1 + val2))
      cons(0, _fibs(0,1))


