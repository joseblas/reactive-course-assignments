package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {
  // The minimum of a single-element heap will be the single element
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  // Finding the minimum value of a 2-element heap will be the smaller
  // of the two elements
  property("min2") = forAll { (a: Int, b: Int) =>
    val m = findMin(insert(a, insert(b, empty)))
    
    if (a >= b) m == b
    else        m == a
  }
  
  // Deleting the smallest element in a single-element heap will
  // result in an empty head
  property("min3") = forAll { a: Int => isEmpty(deleteMin(insert(a, empty))) }
  
  // Assembling a list from the minimums of a heap will result in
  // a sorted list
  property("min4") = forAll { h: H =>
    // Generating the list
    def genList: List[Int] = {
      def genListAcc(h: H, acc: List[Int]): List[Int] =
        if (isEmpty(h)) acc
        else            genListAcc(deleteMin(h), acc :+ findMin(h))
      
      genListAcc(h, List())
    }
    
    genList == genList.sorted(ord)
  }
  
  // The minimum of the union of two heaps will result in either
  // the minimum of the first heap or the second heap
  property("min5") = forAll { (h1: H, h2: H) =>
    val m = meld(h1, h2)
    try { findMin(m) == findMin(h1) || findMin(m) == findMin(h2) }
    catch { case e: NoSuchElementException => true }
  }

  // Generating a heap
  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    m <- oneOf(value(empty), genHeap)
  } yield insert(a, m)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)
}
