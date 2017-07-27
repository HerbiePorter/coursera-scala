package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("weight of an even larger tree") {
    new TestTrees {
      assert(weight(t2) === 9)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("chars of a t1") {
    new TestTrees {
      assert(chars(t1) === List('a', 'b'))
    }
  }

  test("counting the charectors in the string abcabcde") {
    assert(times("abcabcde".toList) === List(('e', 1), ('a', 2), ('b', 2), ('c', 2), ('d', 1)))
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("makeOrderedLeafList for some longer frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('f', 9), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3), Leaf('f', 9)))
  }


  test("combine some leafs") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 4)))
  }

  test("combine some leafs and check ordering is preservered") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3), Leaf('f', 9))
    assert(combine(leaflist) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('x', 3), Leaf('f', 9)))
  }

  test("combine some leafs and forks and check ordering is preservered") {
    val list = List(Leaf('x', 2), Leaf('y', 3), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Leaf('f', 9))
    assert(combine(list) === List(Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3), Fork(Leaf('x', 2), Leaf('y', 3), List('x', 'y'), 5), Leaf('f', 9)))
  }

//  test("create code tree from string 'cool'") {
//    assert(createCodeTree("cool".toList) === Fork(
//      Fork(
//        Leaf('l', 1),
//        Leaf('c', 1),
//        List('c', 'l'),
//        2),
//      Leaf('o', 2),
//      List('c', 'l', 'o'),
//      4))
//  }
//
//  test("create code tree from string 'someText' is optimal") {
//    assert(createCodeTree("someText".toList) ===
//      Fork(
//        Fork(
//          Fork(
//            Fork(
//              Fork(
//                Fork(
//                  Leaf('s', 1),
//                  Leaf('x', 1),
//                  List('s', 'x'),
//                  2),
//                Leaf('T', 1),
//                List('T','s', 'x'),
//                3),
//              Leaf('t', 1),
//              List('T', 's', 't', 'x'),
//              4),
//            Leaf('m', 1),
//            List('T', 'm', 's', 't', 'x'),
//            5),
//          Leaf('o', 1),
//          List('T', 'm', 'o', 's', 't', 'x'),
//          6),
//        Leaf('e', 2),
//        List('T', 'e', 'm', 'o', 's', 't', 'x'),
//        8)
//    )
//  }

  test("decode a simple bits '010' should be 'aba'") {
    new TestTrees {
      assert(decode(t1, List[Bit](0, 1, 0)).mkString("") === "aba")
    }
  }

  test("encode a simple string 'aba' should be '010'") {
    new TestTrees {
      assert(encode(t1)("aba".toList) === List(0, 1, 0))
    }
  }

  test("quick encode a simple string 'aba' should be '010'") {
    new TestTrees {
      assert(quickEncode(t1)("aba".toList) === List(0, 1, 0))
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode french secret") {
    assert(decodedSecret.fold("")((a, b) => s"$a$b") === "huffmanestcool")
  }

  test("convert codetree to codetable") {
    new TestTrees {
      assert(convert(t2) === List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))
    }
  }

}
