package recfun

import scala.annotation.tailrec

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    println("Balanced Parenthesis")
    print(s"${balance("(if (zero? x) max (/ 1 x))".toList)}")
    println()
    print(s"${balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList)}")
    println()
    print(s"${balance(":-)".toList)}")
    println()
    print(s"${balance("())(".toList)}")

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {if (c==0 ||c==r) 1 else pascal(c, r-1)+pascal(c-1, r-1)}

  /** 
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def balanced(chars: List[Char], numOpens: Int) : Boolean = {
      if (chars.isEmpty) numOpens == 0
      else if (chars.head == '(') balanced(chars.tail, numOpens+1)
      else if (chars.head == ')') numOpens>0 && balanced(chars.tail, numOpens-1)
      else balanced(chars.tail, numOpens)
    }
    balanced(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty) 0
      else if (money == 0) 1
      else loop(money, coins.tail) + loop(money - coins.head, coins)
    }
    loop(money, coins)
  }
}
