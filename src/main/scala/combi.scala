import scalaz.Memo

object Count {
  def main(args: Array[String]) {

    lazy val limit = BigInt(10).pow(6)

    // conditions:
    // n <- 1 to 100
    // r <= n

    val vals = for {
      n <- 1 to 100
      r <- 1 to n
    } yield selectionsMem(n, r)

    println("All vals: " + vals.size)
    println("Above limit: " + vals.filter{ _ > limit}.size)
  }

  def selectionsPure(n: BigInt, r: BigInt): BigInt = {
    factorial(n) / ( factorial(r) * factorial(n -r))
  }

  def selectionsMem(n: Int, r: Int): BigInt = {
    memoFact(n) / ( memoFact(r) * memoFact(n -r))
  }

  def factorial(n: BigInt): BigInt = {
    if (n <= 1)
      1
    else
      n * factorial(n - 1)
  }

  val memoFact: BigInt => BigInt = Memo.mutableHashMapMemo {
    case n if n <= 1 => 1
    case n => n * memoFact(n - 1)
  }

}

