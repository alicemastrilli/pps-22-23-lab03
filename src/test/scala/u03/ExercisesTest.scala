package u03;

import org.junit.*
import org.junit.Assert.*
import Exercises.*
import u02.Modules.Person


class ExercisesTest:
  import List.*
  import Option.*
  import Person.*
  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val l2: List[Int] = Cons(40, Cons(50, Nil()))

  val p1: Student = Student("mario", 2015)
  val p2: Teacher = Teacher("luigi", "fisica")
  val p3: Teacher = Teacher("vittorio", "os")
  val p4: Student = Student("camilla", 2021)
  val p5: Teacher = Teacher("alessandro", "iot")

  val persons: List[Person] = Cons(p1, Cons(p2, Cons(p3, Cons(p4, Cons(p5, Nil())))))
  val students: List[Person] = Cons(p1, Cons(p4, Nil()))
  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 0))
    assertEquals(Cons(10, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(10, Cons(20, Nil())), drop(l, 2))
    assertEquals(l, drop(l, 3))
    assertEquals(l, drop(l, -2))
    assertEquals(Nil(), drop(Nil(), 2))

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), append(l, l2))
    assertEquals(l, append(l, Nil()))
    assertEquals(l, append(Nil(), l))
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil())))
    assertEquals(Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))), flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil()))))
    assertEquals(Nil(), flatMap(Nil())(_ => Cons(1, Nil())))

  @Test def testNewMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), newMap(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), newMap(l)(_ + ""))
    assertEquals(Nil(), newMap(Nil())(""))


  @Test def testNewFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), newFilter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), newFilter(l)(_ != 20))
    assertEquals(Nil(), newFilter(Nil())(_ != 20))


  @Test def testMax() =
    assertEquals(Some(30), max(l))
    assertEquals(None(), max(Nil()))
    assertEquals(Some(50), max(l2))
    assertEquals(Some(25),max(Cons(10, Cons(25, Cons(20, Nil())))))

  @Test def testGetCoursesOfTeachers() =
    assertEquals(Cons("fisica", Cons("os", Cons("iot", Nil()))), getCoursesOfTeachers(persons))
    assertEquals(Nil(), getCoursesOfTeachers(students))
    assertEquals(Nil(), getCoursesOfTeachers(Nil()))


  @Test def testFoldLeft() =
    assertEquals(60, foldLeft(l)(0)(_ + _))
    assertEquals(64, foldLeft(l)(4)(_ + _ ))
    assertEquals(-60, foldLeft(l)(0)(_ - _ ))
    assertEquals(-55, foldLeft(l)(5)(_ - _ ))
    assertEquals(0.0016, foldLeft(l)(10.0)(_ /_ ), 0.0001)

//assertEquals(1, foldLeft(Nil())(1)(_ + _))
