package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import scala.util.Sorting

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    // empty heap
    Gen.const(empty),
    // non-empty heap
    for {
      i <- arbitrary[A]
      h <- genHeap
    } yield insert(i, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("removing the added element should yield it") = forAll { (a: Int) =>
    val h = insert(a, empty)
    findMin(h) == a
  }
 
  property("of 2 els finding the min should get the smallest") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    val smallest = if (a < b) then a else b
    findMin(h) == smallest
  }

  property("deleting min of single el heap should result in empty") = forAll { (a: Int) =>
    val emptyH = deleteMin(insert(a, empty))
    emptyH == empty
  }

  // returning boolean
  // generate list from recursively finding min from h
  // list should eq list.ordered
  property("the result should be sorted seq when finding and deleting minina") = forAll { (h: H) =>
    def getSorted(h: H): List[A] = {
      if isEmpty(h) then Nil 
      else {
        findMin(h)::getSorted(deleteMin(h))
      }
    }

    val sorted = getSorted(h)
    sorted == sorted.sorted    
  }

  def getMinOrNil(h: H) = if isEmpty(h) then Nil else findMin(h)
  property("finding min of melded should be min of either") = forAll { (h1: H, h2: H) => 

    val minMelded = getMinOrNil(meld(h1, h2))
    minMelded == getMinOrNil(h1) || minMelded == getMinOrNil(h2)
  }