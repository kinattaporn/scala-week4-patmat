package patmat

import org.junit._
import org.junit.Assert.assertEquals

class HuffmanSuite {
  import Huffman._

  trait TestTrees {
    // t0        t1              t2
    //                           abd
    //           ab           ab     d
    //  a      a    b       a    b
    val t0 = Leaf('a', 2)
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  @Test def `weight of a larger tree (10pts)`: Unit =
    new TestTrees {
      println(weight(t1))
      println(chars(t1))
      assertEquals(5, weight(t1))
    }

  @Test def `chars of a larger tree (10pts)`: Unit =
    new TestTrees {
      println(weight(t2))
      println(chars(t2))
      assertEquals(List('a','b','d'), chars(t2))
    }

  @Test def `makeCodeTree_test`: Unit = {
    val sampleTree = makeCodeTree(
      makeCodeTree(Leaf('x', 1), Leaf('e', 1)),
      Leaf('t', 2))
    println(sampleTree)
  }

  @Test def `string2chars hello world`: Unit =
    println(string2Chars("hello, world"))
    assertEquals(List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'), string2Chars("hello, world"))

  @Test def `times_test`: Unit = {
    println(times(List('a', 'b', 'a')))
    println(times(List('a', 'b', 'a')).sortBy(x => x._2))
  }

  @Test def `make ordered leaf list for some frequency table (15pts)`: Unit =
    println(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))
    assertEquals(List(Leaf('e',1), Leaf('t',2), Leaf('x',3)), makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))))

  @Test def `singleton_test`: Unit = {
    new TestTrees {
      println(singleton(List(Leaf('a', 2))))
      println(singleton(List(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5))))
      println(singleton(List(t1)))
      println(singleton(List(Leaf('a', 2), t1)))
      println(singleton(List(Leaf('a', 2), Leaf('a', 2))))
    }
  }

  @Test def `combine of some leaf list (15pts)`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    println(combine(leaflist))
    assertEquals(List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)), combine(leaflist))

    val leaflist_2items = List(Leaf('e', 1), Leaf('t', 2))
    println(combine(leaflist_2items))

    val leaflist_order = List(Leaf('x', 4), Leaf('e', 1), Leaf('t', 2))
    println(combine(leaflist_order))
  }

  @Test def `until_test`: Unit = {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    println(until(singleton, combine)(leaflist))

    val leaflist_2items = List(Leaf('e', 1), Leaf('t', 2))
    println(until(singleton, combine)(leaflist_2items))

    val leaflist_order = List(Leaf('x', 4), Leaf('e', 1), Leaf('t', 2))
    println(until(singleton, combine)(leaflist_order))
  }

  @Test def `createCodeTree_test` = {
    val leaflist = List('e', 't', 'x', 't', 'x', 'x', 'x')
    println(createCodeTree(leaflist))

    val leaflist_2items = List('e', 't', 't')
    println(createCodeTree(leaflist_2items))

    val leaflist_order = List('x', 'x', 'e', 't', 'x', 't', 'x')
    println(createCodeTree(leaflist_order))
  }

  @Test def `decode_test`: Unit = {
    new TestTrees {
      println(decode(t0, List()))     // a
      println(decode(t1, List(0)))    // a
      println(decode(t1, List(1)))    // b
      println(decode(t1, List()))     // ab
      println(decode(t2, List(0, 0))) // a
      println(decode(t2, List(0, 1))) // b
      println(decode(t2, List(1)))    // d
      println(decode(t2, List(0)))    // ab
      println(decode(t2, List()))     // abd
      println(decode(frenchCode, secretH))
      println(decode(frenchCode, secretU))
      println(decode(frenchCode, secretF))
      println(decode(frenchCode, secretM))
      println(decode(frenchCode, secretA))
      println(decode(frenchCode, secretN))
      println(decode(frenchCode, secretE))
      println(decode(frenchCode, secretS))
      println(decode(frenchCode, secretT))
      println(decode(frenchCode, secretC))
      println(decode(frenchCode, secretO))
      println(decode(frenchCode, secretL))
      println(decode(frenchCode, secret))
    }
  }

  @Test def `encode_test`: Unit = {
    new TestTrees {
      println(encode(t0)(List('a')))            // List()
      println(encode(t1)(List('a')))            // List(0)
      println(encode(t1)(List('b')))            // List(1)
      println(encode(t1)(List('a', 'b')))       // List(0, 1)
      println(encode(t2)(List('a')))            // List(0, 0)
      println(encode(t2)(List('b')))            // List(0, 1)
      println(encode(t2)(List('d')))            // List(1)
      println(encode(t2)(List('a', 'b')))       // List(0, 0, 0, 1)
      println(encode(t2)(List('a', 'b', 'd')))  // List(0, 0, 0, 1, 1)
      println(encode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')))
    }
  }

  @Test def `decode and encode a very short text should be identity (10pts)`: Unit =
    new TestTrees {
      println(encode(t1)("ab".toList))
      println(decode(t1, encode(t1)("ab".toList)))
      assertEquals("ab".toList, decode(t1, encode(t1)("ab".toList)))
    }

  @Test def `codeBits_test`: Unit = {
    new TestTrees {
      println(codeBits(List(('a', List())))('a'))
      println(codeBits(List(('a', List(0)), ('b', List(1))))('a'))
      println(codeBits(List(('a', List(0)), ('b', List(1))))('b'))
      println(codeBits(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))('a'))
      println(codeBits(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))('b'))
      println(codeBits(List(('a', List(0, 0)), ('b', List(0, 1)), ('d', List(1))))('d'))
      println(codeBits(List(('h', List(0, 0, 1, 1, 1, 0, 1))))('h'))
    }
  }

  @Test def `convert_mergeCodeTables_test`: Unit = {
    new TestTrees {
      println(convert(t0))
      println(convert(t1))
      println(convert(t2))
      println(convert(frenchCode))
      println(mergeCodeTables(convert(t0), convert(t1)))
    }
  }

  @Test def `quickEncode_test`: Unit = {
    new TestTrees {
      println(quickEncode(t0)(List('a')))            // List()
      println(quickEncode(t1)(List('a')))            // List(0)
      println(quickEncode(t1)(List('b')))            // List(1)
      println(quickEncode(t1)(List('a', 'b')))       // List(0, 1)
      println(quickEncode(t2)(List('a')))            // List(0, 0)
      println(quickEncode(t2)(List('b')))            // List(0, 1)
      println(quickEncode(t2)(List('d')))            // List(1)
      println(quickEncode(t2)(List('a', 'b')))       // List(0, 0, 0, 1)
      println(quickEncode(t2)(List('a', 'b', 'd')))  // List(0, 0, 0, 1, 1)
      println(quickEncode(frenchCode)(List('h', 'u', 'f', 'f', 'm', 'a', 'n', 'e', 's', 't', 'c', 'o', 'o', 'l')))
    }
  }

  @Rule def individualTestTimeout = new org.junit.rules.Timeout(10 * 1000)
}
