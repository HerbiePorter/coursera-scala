package objsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {

  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
    val e = new Tweet("e", "e body", 15)
    val f = new Tweet("f", "f body", 2)
    val g = new Tweet("g", "g body", 4)
    val set6 = set5.incl(e).incl(f).incl(g)

    val set7 = set1.incl(c).incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 7 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 7)) === 1)
    }
  }

  test("filter: 134 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 134)) === 0)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("filter: 4 on set6") {
    new TestSets {
      assert(size(set6.filter(tw => tw.retweets == 4)) === 1)
    }
  }

  test("filter: contains body on set6") {
    new TestSets {
      assert(size(set6.filter(tweet => Seq("body").exists(tweet.text.contains))) === 7)
    }
  }

  test("filter: contains a or e on set6") {
    new TestSets {
      assert(size(set6.filter(tweet => Seq("a", "e").exists(tweet.text.contains))) === 2)
    }
  }

  test("union: set4c and set4c") {
    new TestSets {
      assert(size(set4c.union(set4c)) === 3)
      assert(set4c.union(set4c).head.text === "a body")
      assert(set4c.union(set4c).tail.text === "c body")
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
      assert(set4c.union(set4d).head.text === "a body")
      assert(set4c.union(set4d).tail.text === "d body")
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
      assert(set5.union(set1).head.text === "a body")
      assert(set5.union(set1).tail.text === "d body")
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
      assert(set1.union(set5).head.text === "a body")
      assert(set1.union(set5).tail.text === "d body")
    }
  }

  test("size: empty set") {
    new TestSets {
      assert(set1.size() === 0)
    }
  }

  test("size: set2") {
    new TestSets {
      assert(set2.size === 1)
    }
  }

  test("size: set3") {
    new TestSets {
      assert(set3.size === 2)
    }
  }

  test("head: set5") {
    new TestSets {
      assert(set5.head.text === "a body")
    }
  }

  test("tail: set5") {
    new TestSets {
      assert(set5.tail.text === "d body")
    }
  }

  test("mostRetweeted: set6") {
    new TestSets {
      assert(set7.mostRetweeted.text === "d body")
    }
  }

  test("mostRetweeted: set5") {
    new TestSets {
      assert(set5.mostRetweeted.text === "a body")
    }
  }

  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      assert(!trends.isEmpty)
      assert(trends.size === 4)
      assert(trends.head.user == "a" || trends.head.user == "b")
    }
  }

}
