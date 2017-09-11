package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genEmptyHeap: Gen[H] = const(empty)

  lazy val genNonEmptyHeap: Gen[H] = for {
    v <- arbitrary[Int]
    m <- oneOf(const(empty), genHeap)
  } yield insert(v, m)

  lazy val genHeap: Gen[H] = oneOf(genEmptyHeap, genNonEmptyHeap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  def isSorted(i: H): Boolean = {
    if (isEmpty(i) || isEmpty(deleteMin(i)))
      true
    else
      findMin(i) <= findMin(deleteMin(i)) && isSorted(deleteMin(i))
  }

  def size(i: H): Integer = {
    if (isEmpty(i))
      0
    else if (isEmpty(deleteMin(i)))
      1
    else
      1 + size(deleteMin(i))
  }

  property("isEmptyShouldAlwaysBeTrueForEmptyHeaps") = forAll(genEmptyHeap) { (h: H) =>
    isEmpty(h)
  }

  property("findMinOfSingleElementHeap") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("findMinOfTwoElements") = forAll(genEmptyHeap) { (h: H) =>
    findMin(insert(1, insert(2, h))) == 1
  }

  property("findMinOfAnyHeap") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMinOfHeapWithOneElementShouldBeEmpty") = forAll(genEmptyHeap) { (h: H) =>
    isEmpty(deleteMin(insert(1, h)))
  }

  property("deleteMinShouldAlwaysBeMin") = forAll(genEmptyHeap) { (h: H) =>
    findMin(deleteMin(insert(3, insert(1, insert(2, h))))) == 2
  }

  property("findMinShouldReturnSortedElements") = forAll { (h: H) =>
    isSorted(h)
  }

  property("meldingOfAnyTwoEmptyHeapsShouldBeEmpty") = forAll(genEmptyHeap, genEmptyHeap) { (h: H, i: H) =>
    isEmpty(meld(h, i))
  }

  property("minOfMeldingOfAnyHeapAndAnEmptyHeap") = forAll(genNonEmptyHeap, genEmptyHeap) { (h: H, i: H) =>
    val x = findMin(h)

    val z = meld(h, i)

    findMin(z) == x
  }

  property("minOfMeldingOfAnyTwoHeaps") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h: H, i: H) =>
    val x = findMin(h)
    val y = findMin(i)

    val z = meld(h, i)

    findMin(z) == x || findMin(z) == y
  }

  property("meldingOfAnyTwoHeapsShouldPreserveAllElements") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h: H, i: H) =>
    val x = size(h)
    val y = size(i)

    val z = meld(h, i)

    size(z) == x + y
  }


  property("findMinOfMeldingOfAnyTwoHeapsShouldReturnSortedElements") = forAll(genNonEmptyHeap, genNonEmptyHeap) { (h: H, i: H) =>
    val z = meld(h, i)

    isSorted(z)
  }

}
