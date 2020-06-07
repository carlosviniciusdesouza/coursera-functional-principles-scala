package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(r == 0 || c == 0 || c == r) 1
    else pascal(c, r-1) + pascal(c-1, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def temp(i: Int, chars: List[Char]): Int =
      if (chars.isEmpty) i
      else if (chars.head == '(') temp(i + 1, chars.tail)
      else if (chars.head == ')' && i > 0) temp(i - 1, chars.tail)
      else if (chars.head == ')' && i <= 0) -1
      else temp(i, chars.tail)
    temp(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def coinsSort = coins.sorted(Ordering.Int.reverse)
    def temp(money: Int, coins: List[Int]): Int = {
      if (money < 0 || coins.isEmpty ) 0
      else if (money == 0 ) 1
      else temp(money, coins.tail) + temp(money - coins.head, coins)
    }
    temp(money, coinsSort)
  }
}
