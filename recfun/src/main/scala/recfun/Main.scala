package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || r < 0 || c > r) 0 else {
      if (c == 0) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }
  
  pascal(0, 0)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceFrom(acc: Int, i: Int, chars: List[Char]): Boolean = {
      if (i == chars.length)
        acc == 0
      else {
        if (chars(i) == '(')
          balanceFrom(acc + 1, i + 1, chars)
        else {
          if (chars(i) == ')') {
            if (acc == 0)
              false
            else
              balanceFrom(acc - 1, i + 1, chars)
          } else
            balanceFrom(acc, i + 1, chars)
        }
      }
    }
    balanceFrom(0, 0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange(start: Int, money: Int, coins: List[Int]): Int = {
      if (start >= coins.length) 0
      else {
        if (money == 0) 1
        else if (money < 0) 0
        else {
          var count = 0
          for (i <- start until coins.length) {
            count = count + countChange(i, money - coins(i), coins)
          }
          count
        }
      }
    }
    countChange(0, money, coins)
  }
}
