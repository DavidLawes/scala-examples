package recfun

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
    if c > r then 0
    else if c == r || c == 0 then 1
    else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean =
    def loop(chars: List[Char], openParens: Int): Boolean =
      if (chars.isEmpty) {
        openParens == 0 
      }
      else {
        val n = chars.head.toString match {
          case "(" => openParens + 1
          case ")" => openParens - 1
          case _ => openParens
        }
        
        if n < 0 then false else loop(chars.tail, n)
      } 
    loop(chars, 0)

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int =
    if (money == 0) 1
    else if (coins.isEmpty || money < 0) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
