package recfun
import common._

object Main {

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

        result == 0

    }

  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
    {

        def count(subMoney: Int, subCoins: List[Int]): Int = {

            if (subMoney == 0) 1
            else if (subCoins.isEmpty && subMoney >= 1 || subMoney < 0 ) 0
            else
            {

                count (subMoney, subCoins.tail) + count(
                    subMoney - subCoins.head, subCoins)

            }
        }

        count(money, coins.sortWith(_.compareTo(_) < 0))

    }
}
