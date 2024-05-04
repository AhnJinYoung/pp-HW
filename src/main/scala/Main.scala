package pp202302.assign1 

import scala.annotation.tailrec
import scala.util.control.TailCalls._

///////////////////////////////////////////
/////// DO NOT FIX THE ABOVE LINES ////////
///////////////////////////////////////////

/** Principles of Programming: Assignment 01.
  *
  * Implement given functions, which are currently left blank. (???) **WARNING:
  * Please read the restrictions below carefully.**
  *
  * If you do not follow these, **your submission will not be graded.**
  *
  *   - Do not use the keyword `var`. Use `val` and `def` instead.
  *   - Do not use any library functions or data structures like `List`,
  *     `Array`, `Range` (`1 to n`, `1 until n` ...), `fold`, `map`, `reduce` or
  *     etc.
  *   - You can only use tuples, `scala.annotation.tailrec`, and
  *     `scala.util.control.TailCalls._`, `Math._` from the library.
  *   - Do not use any looping syntax of Scala (`for`, `while`, `do-while`,
  *     `yield`, ...)
  *
  * Again, your score will be zero if you do not follow these rules.
  *
  * Note that these rules will be gradually relaxed through the next
  * assignments.
  *
  * For three problems, 50% of the test cases will require tail call
  * optimizations (i.e., large inputs) and the other 50% will not (i.e., small
  * inputs).
  *
  * So, you will get at least 50% of the score if you submit a correct program
  * without tail call optimization. (30sec timeout)
  */
object Assignment1:
  /** Problem 1.
    *
    * Get i_1 + i_2 + ... + i_k which satisfies the following conditions:
    *
    *   - 0 < i_1 < i_2 < ... < i_k <= n <= 100000
    *   - Inductive definition
    *     - i_1 is the smallest value s.t. prop(i_1) = true
    *     - Given the previous i_1 ... i_(j-1), i_j is the smallest value s.t.
    *       prop(i_1 + .. + i_j) = true
    *   - If there is no such value, return 0.
    */
  def sumProp(prop: Long => Boolean, n: Long): Long = {
    @tailrec
    def SumPropHelper(currentSum: Long, Index: Long): Long = {
      if (Index > n) {
        currentSum
      } else if (prop(currentSum + Index)) {
        SumPropHelper(currentSum + Index, Index + 1)
      } else {
        SumPropHelper(currentSum, Index + 1)
      }
    }
    SumPropHelper(0, 1)
  }

  //TODO :

  /** Problem 2: Finding Gaussian Primes
    *
    * A Gaussian integer is a complex number whose real and imaginary parts are
    * both integers. A Gaussian integer is said to be a Gaussian prime if and
    * only if it is a Gaussian integer that cannot be factored into the product
    * of two Gaussian integers with smaller absolute value.
    *
    * Note) Absolute value of a complex number |a + bi| = sqrt(a^2 + b^2)
    *
    * e.g.) 1 + 2i is a Gaussian prime because it cannot be factored into the
    * product of two Gaussian integers with smaller absolute value.
    *
    * e.g.2) 3 + i is not a Gaussian prime because 3 + i = (1 + i)(2 - i) .
    *
    * See the below websites for more information.
    *   - ko) https://ko.wikipedia.org/wiki/가우스_정수
    *   - en) https://en.wikipedia.org/wiki/Gaussian_integer
    *
    * Note) You can use Math._ functions for this problem.
    */

  /** Problem 2-1.
    *
    * Find out whether a gaussian integer a + bi is divisible by a gaussian
    * integer c + di.
    *
    * a + bi is divisible by c + di iff there is a gaussian integer e + fi which
    * satisfies (a + bi) = (c + di)(e + fi)
    *
    * We guarantee that either c or d is non-zero.
   *
   *Note That :
   *
   * A Gaussian integer a + bi is a Gaussian prime if and only if either:
   *
   * one of a, b is zero and the absolute value of the other is a prime number of the form 4n + 3
   * (with n a nonnegative integer), or
   * both are nonzero and a^2 + b^2 is a prime number (which will not be of the form 4n + 3).
   */

  def isDivisible(a: Long, b: Long, c: Long, d: Long): Boolean = {
    if (c == 0 && d == 0) {
      false
    } else {

      val e = (a * c + b * d) / (c * c + d * d)
      val f = (b * c - a * d) / (c * c + d * d)  // e + fi s.t. a + bi = (c + di)(e + fi)

      // e와 f가 정수인 경우 a + bi가 c + di로 나눌 수 있음
      (e * c == a + f * d) && (f * c == b - e * d)
    }
  }

  /** Problem 2-2.
    *
    * Find out the n-th gaussian prime in a squared area.
    *
    * Detailed description:
    *
    *   - For all gaussian primes (a + bi) which 0 <= a <= r and 0 <= b <= r,
    *     Sort those gaussian primes in a lexicographical order.
    *     - e.g.) a + bi < c + di iff a < c or (a == c and b < d)
    *   - Find the n-th value in the sorted list. If that prime is a + bi,
    *     return (a, b)
    *   - If there is no such value, return (0, 0)
    *
    * Note)
    *   - 0, +-1, +-i are not a gaussian prime
    *   - 1 <= n <= 10^5, 1 <= r <= 10^4
    *   - We guarantee that the n-th prime is always in the sorted list.
    *
    * Hint) https://mathworld.wolfram.com/GaussianPrime.html
    */

  def isGaussianPrime(a: Long, b: Long): Boolean = {

    (((a * b == 0) && is_3_mod_4(a, b)) || ((a * b != 0) && isPrime(a*a + b*b)))
  }

  def is_3_mod_4(a: Long, b: Long): Boolean = {
    (((a - 3) % 4 == 0) && ((a - 3) / 4 >= 0) && isPrime(a))
      || (((b - 3) % 4 == 0) && ((b - 3) / 4 >= 0) && isPrime(b))
  }

  def isPrime(n: Long): Boolean = {
    @tailrec
    def isPrimeTailHelper(divisor: Long, isDivisible: Boolean): Boolean = {
      if (divisor <= 1) {
        isDivisible
      } else if (n % divisor == 0) {
        false
      } else {
        isPrimeTailHelper(divisor - 1, isDivisible)
      }
    }
    if (n <= 1) {
      false
    } else {
      isPrimeTailHelper(Math.sqrt(n).toInt, true)
    }
  }

  def nthPrime(n: Long, r: Long): (Long, Long) = {
      @tailrec
      def nthPrimeTailHelpler(N : Long, a : Long, b : Long,  R : Long): (Long, Long) = {
        if (isGaussianPrime(a,b)) {
          if (N == 1) (a,b)
          else {
            if (b + 1 > r) {
              if (a >= r) {
                (0, 0)
              }
              else {
                //next a
                nthPrimeTailHelpler(N-1, a + 1, 0, r)
              }
            } else {
              nthPrimeTailHelpler(N-1, a, b + 1, r)
            }
          }
        }
        else { //not Prime
          if (b+1 > r) {
            if (a >= r) {
              (0,0)
            }
            else {
              //next a
              nthPrimeTailHelpler(N,a+1,0,r)
            }
          } else {
            nthPrimeTailHelpler(N,a,b+1,r)
          }
        }
      }

    nthPrimeTailHelpler(n,0,0,r)
  }

  /** Problem 3: Implement Branching Ackermann Function
    *
    * Given a function p and positive integers a and b, implement function `baf`
    * s.t. (0 <= a, b <= 10^6):
    *
    * ```
    * baf(p, a, b) = p(a, b)                            (a <= 0 or b <= 0)
    * baf(p, a, b) = p(baf(p, a-1, b)), baf(p, a, b-1)) (else if a + b is a power of 2)
    * baf(p, a, b) = p(a, baf(p, a, b-1))               (else if p(a, b) % 2 == 0)
    * baf(p, a, b) = p(baf(p, a-1, b)), b)              (otherwise)
    * ```
    *
    * a + b is a power of 2 iff there is an integer k s.t. a + b = 2^k
    *
    * Hint 1: If a, b > 0, wrap remained calculations, make it as an anonymous
    * function and pass it to the parameter recursively.
    *   - That anonymous function is called 'Continuation'. You'd better search
    *     'Continuation Passing Style'.
    *
    * Hint 2: Due to the limits of Scala's platform (JVM), tail call (not tail
    * recursion) is not optimized generally, especially when using continuation
    * passing style.
    *   - To optimize tail call properly, use `tailcall`, `done`, `result`
    *     functions from scala.util.control.TailCalls._ (See
    *     https://stackoverflow.com/questions/16539488/why-scala-doesnt-make-tail-call-optimization)
    */

  def baf(p: (Long, Long) => Long, a: Long, b: Long): Long = {

    def bafCPS(p: (Long, Long) => Long, a: Long, b: Long, cont: Long => TailRec[Long]): TailRec[Long] = {

      if (a <= 0 || b <= 0) {
        done(p(a, b))
      } else if (isPowerOfTwo(a + b)) {
        val result = tailcall(bafCPS(p, a - 1, b, resultA => tailcall(bafCPS(p, a, b - 1, resultB => done(p(resultA, resultB))))))
        result
      } else if (p(a, b) % 2 == 0) {
        val result = tailcall(bafCPS(p, a, b - 1, result => done(p(a, result))))
        result
      } else {
        val result = tailcall(bafCPS(p, a - 1, b, result => done(p(result, b))))
        result
      }
    }


    val answer: Long = tailcall(bafCPS(p, a, b, done)).result
    answer
  }
def isPowerOfTwo(n : Long) : Boolean = {
  (n > 0) && (n & (n-1)) == 0  //idea from chatGPT
  }