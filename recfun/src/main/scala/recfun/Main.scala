package recfun

object Main {
  def main(args: Array[String]) {
    //    println("Pascal's Triangle")
    //    for (row <- 0 to 10) {
    //      for (col <- 0 to row)
    //        print(pascal(col, row) + " ")
    //      println()
    //    }

    //    println("false : " + balance("".toList))
    //    println("false : " + balance(")".toList))
    //    println("true : " + balance("()".toList))
    //    println("true : " + balance("()()".toList))
    //
    //    println("true : " + balance("(())".toList))
    //
    //    println("true : " + balance("(()())".toList))
    //
    //    println("false : " + balance(")))(((".toList))
    //
    //    println("true : " + balance("(1(2(3)()(4)5))".toList))

    println("3 : " + countChange(3, List(2, 3, 1)))

    println("0 : " + countChange(301, List(5, 10, 20, 50, 100, 200, 500)))
    println("1 : " + countChange(3, List(3)))
    println("1 : " + countChange(3, List(1)))
    println("2 : " + countChange(3, List(2, 1)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r || c == r + 1) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) {
      return false
    }

    def balance(chars: List[Char], openedCounter: Int): Boolean = {
      var charss = chars

      while (charss.nonEmpty && charss.head != '(' && charss.head != ')') {
        charss = charss.tail
      }

      if (charss.isEmpty) {
        openedCounter == 0
      } else if (charss.head == '(') {
        balance(charss.tail, openedCounter + 1)
      } else if (charss.head == ')' && openedCounter > 0) {
        balance(charss.tail, openedCounter - 1)
      } else {
        false
      }

    }

    balance(chars, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted.reverse

    def countChange(targetMoney: Int, runningTotal: Int, denominations: List[Int], coins: List[Int]): Int = {
      if (coins.sum == targetMoney) {
        return 1
      } else if (denominations.isEmpty) {
        return 0
      }

      val highestCoin = denominations.head

      if (highestCoin <= runningTotal) {
        countChange(targetMoney, runningTotal - highestCoin, denominations, highestCoin :: coins) +
          countChange(targetMoney, runningTotal, denominations.tail, coins)
      } else {
        countChange(targetMoney, runningTotal, denominations.tail, coins)
      }
    }

    countChange(money, money, sortedCoins, List[Int]())
  }
}
