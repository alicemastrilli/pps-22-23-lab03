package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))
  val l2: List[Int] = Cons(40, Cons(50, Nil()))
  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_+1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_+""))

  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_>=20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_!=20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 0))
    assertEquals(Cons(10, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(10, Cons(20, Nil())), drop(l, 2))
    assertEquals(l, drop(l, 3))
    assertEquals(l, drop(l, -2))
    assertEquals(Nil(), drop(Nil(), 2))

  @Test def testAppend() =
    assertEquals( Cons(10, Cons(20, Cons(30, Cons(40, Cons(50, Nil()))))), append(l, l2))
    assertEquals(l, append(l, Nil()))
    assertEquals(l, append(Nil(), l))
    assertEquals(Nil(), append(Nil(), Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v=> Cons(v+1, Nil())))
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

