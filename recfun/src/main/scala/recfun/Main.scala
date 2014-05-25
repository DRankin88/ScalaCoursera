package recfun
import common._

object Main {
    def main(args: Array[String])
    {
        // println("Pascal's Triangle")
        // for (row <- 0 to 10) {
        //     for (col <- 0 to row)
        //         print(pascal(col, row) + " ")
        //     println()

        println("balance")
        val input = "((()))((())())  true"
        println(input)
        println(balance(input.toList))
        val input1 = "Th( ) () ( ( ( ) ) )) false"
        println(input1)
        println(balance(input1.toList))
        val input2 = "()( false"
        println(input2)
        println(balance(input2.toList))
        val input3 = "()((( false"
        println(input3)
        println(balance(input3.toList))
        val input4 = "())) false"
        println(input4)
        println(balance(input4.toList))
        val input5 = ") false"
        println(input5)
        println(balance(input5.toList))
        val input6 = "( false"
        println(input6)
        println(balance(input6.toList))
//        }
    }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
    {

        if ((c == 0 || r == 0) || (c == r))
        {
            1
        }
        else
        {
            pascal(c - 1, r - 1) + pascal(c, r - 1)
        }
    }

  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean =
    {

        def balanceHelper(chars: List[Char], incriment: Int): Int =
        {
            if(chars.isEmpty) incriment
            else
            {
                if(chars.head == '(')
                {
                    val result = balanceHelper(chars.tail, incriment + 1)
                    result
                }
                else if (chars.head == ')')
                {
                    if (incriment - 1 < 0)
                    {
                        val result = - 1
                        result
                    }
                    else
                    {
                        val result = balanceHelper(chars.tail, incriment - 1)
                        result
                    }
                }
                else balanceHelper(chars.tail, incriment)
            }
        }

        val result = balanceHelper(chars, 0)
        println(result)

        if (result != 0) false
        else true

    }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
