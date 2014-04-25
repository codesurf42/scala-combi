import scalaz.Memo

object Count {
  def main(args: Array[String]) {

    val n = 5
    val r = 10
    lazy val limit = BigInt(10).pow(6)

    // purely computational
    val val1 = (1 to 100)
      .map{ selectionsPure(_, r) }
      .filter(_ > limit)
      .size

    // memoized, faster
    val val2 = (1 to 100)
      .map{ selectionsMem(_, r) }
      .filter(_ > limit)
      .size

    // too small to be faster
    val val3 = (1 to 100)
      .par.map{ selectionsMem(_, r) }
      .filter(_ > limit)
      .size

    println(s"There are $val1 ($val2, $val3) selections greater than $limit")

  }

  def selectionsPure(n: BigInt, r: BigInt): BigInt = {
    factorial(n) / ( factorial(r) * factorial(n -r))
  }

  def selectionsMem(n: BigInt, r: BigInt): BigInt = {
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

