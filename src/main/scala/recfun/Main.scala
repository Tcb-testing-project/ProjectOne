package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println()
    println("Balance :")
    println(balance("(())".toList))
    println(balance("))()".toList))


    println()
    println("Change :")
    print(countChange(300,List(5,10,20,50,100,200,500)))

  }

  /**
   * Exercise 1
   */


  /*

  My solution

   */

  def factorial(n: Int): Int = {
    if (n == 0)
      1
    else
      n * factorial(n-1)
  }

  def pascal(c: Int, r: Int): Int = {

    factorial(r)/( factorial(r-c) * factorial(c))

  }


  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def iterate(chars: List[Char], acc: Int): Int = {
        if (chars.isEmpty || acc < 0) acc
        else if (chars.head == '(') iterate(chars.tail, acc + 1)
        else if (chars.head == ')') iterate(chars.tail, acc - 1)
        else iterate(chars.tail, acc)
      }
      iterate(chars, 0) == 0


    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else if (money <= 0 && coins.nonEmpty) 0
      else countChange(money, coins.tail) + countChange(money - coins.head, coins)

    }
  }
