package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.*
import quickcheck.IntHeap
import scala.annotation.tailrec

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("findMin on two elements returns fewer") = forAll { (n: Int, m: Int) =>
    val heap = insert(n, insert(m, empty))

    val min = findMin(heap)

    if (n <= m) min == n
    else min == m
  }

  property("insert and deleteMin make heap become empty") = forAll { (n: Int) =>
    val heap = insert(n, empty)

    val emptyH = deleteMin(heap)

    isEmpty(emptyH)
  }

  property("insert and deleteMin n elements make heap become empty") = forAll { (n: List[Int]) =>
    @tailrec
    def insertElements(l: List[Int], h: H): H =
      if (l.isEmpty) h
      else insertElements(l.tail, insert(l.head, h))

    @tailrec
    def deleteCertainElements(h: H, number: Int): H =
      if (number == 0) h
      else deleteCertainElements(deleteMin(h), number - 1)

    val heap = insertElements(n, empty)

    val emptyH = deleteCertainElements(heap, n.length)

    isEmpty(emptyH)
  }

  property("isSorted always returns true") = forAll { (a: H) =>
    @tailrec
    def isSorted(min: Int, h: H): Boolean =
      if (isEmpty(h)) true
      else if ( min > findMin(h) ) false
      else isSorted(findMin(h), deleteMin(h))

    isSorted(findMin(a), deleteMin(a))
  }

  property("findMin on melded two heaps returns min element of them") = forAll { (a: H, b: H) =>
    val heap = meld(a, b)

    val minMelded = findMin(heap)

    findMin(a) == minMelded || findMin(b) == minMelded
  }

  property("!isEmpty then findMin and insert result to heap, findMin must be equal to previous findMin") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)

    findMin(insert(m, h)) == m
  }

  property("meld preserves all elements") = forAll { (h1: H, h2: H) =>
    @tailrec
    def heapsEquals(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (findMin(h1) == findMin(h2)) heapsEquals(deleteMin(h1), deleteMin(h2))
      else false

    val mc = if (findMin(h1) <= findMin(h2)) meld(deleteMin(h1), h2)
             else meld(h1, deleteMin(h2))

    heapsEquals(deleteMin( meld(h1, h2) ), mc)
  }

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    gH <- frequency( (20, const(empty)), (80, genHeap) )
  } yield insert(i, gH)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
