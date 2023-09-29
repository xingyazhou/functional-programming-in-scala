package recfun

import scala.language.postfixOps
import scala.util.control.Breaks._


object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0) then 1
    else if (c == r) then 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    var i = 0
    breakable{
      for (c <- chars) {
        if (c == '(') then i = i + 1
        else if (c == ')') i = i - 1
        if i < 0 then break()
      }
    }
    i == 0

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) {
      1
    } else if (money > 0 && !coins.isEmpty) {
      countChange(money - coins.head, coins) + countChange(money, coins.tail)
    } else {
      0
    }
  }
