package u03

import u02.Modules.*

object Exercises :

  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()

  enum Option[A]:
    case Some(a: A)
    case None() // here parens are needed because of genericity

  object List:
    import Option.*

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

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



    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None()
      case Cons(h,t) => _max(t, h)
      @annotation.tailrec // checks only if optimisation is possible
      def _max(l: List[Int], m: Int): Option[Int] = l match
        case Nil() => Some(m)
        case Cons(h,t) => _max(t, math.max(m,h))

    def getCoursesOfTeachers(l: List[Person]): List[String]=
      flatMap(newFilter(l)(v => !isStudent(v)))(t => Cons(Person.course(t), Nil()))

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
    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (s, 0) => s
      case (Cons(_, tail), n) => drop(tail())(n-1)
      case _ => Empty()

