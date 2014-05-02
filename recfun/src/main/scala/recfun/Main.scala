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
    if (r == 0 || c == 0 || c == r) {
      1
    } else {
      pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceWithState(chars: List[Char], numUnmatched: Int, isOpen: Boolean): Boolean = {
      if (chars.isEmpty) {
        if (chars.head == '(') {
          balanceWithState(chars.tail, numUnmatched + 1, true)
        } else if (chars.head == ')') {
          if (isOpen) {
            balanceWithState(chars.tail, numUnmatched - 1, numUnmatched > 1)
          } else {
            false
          }
        } else {
          balanceWithState(chars.tail, numUnmatched, isOpen)
        }
      } else {
        !isOpen && numUnmatched == 0
      }
    }

    balanceWithState(chars, 0, false)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money < 0) {
      0
    } else if (money >=1 && coins.isEmpty) {
      0
    } else {
      countChange(money, coins.tail) + countChange(money - coins.head, coins)
    }
  }
}
