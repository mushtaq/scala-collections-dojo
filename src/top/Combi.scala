package top

object Combi extends App {

  def comb(xs: List[Int], r: Int): List[List[Int]] = r match {
    case 0 => List(List())
    case _ => for {
      head :: tail <- xs.tails.toList
      ys <- comb(tail, r - 1)
    } yield head :: ys
  }
  
  println("testing github's edit online feature.. looks very cool")

  def perm(xs: List[Int], r: Int): List[List[Int]] = r match {
    case 0 => List(List())
    case _ => for {
      (x, i) <- xs.zipWithIndex
      rest = remove(xs, i)
      ys <- perm(rest, r - 1)
    } yield x :: ys
  }

  def subStrings(str: String): List[String] = for {
    tail <- str.tails.toList
    init <- tail.inits
    if init != ""
  } yield init

  def multiSplits(str: String): List[List[String]] = str match {
    case "" => List(List())
    case _ => for {
      i <- (1 to str.length).toList
      (pre, post) = str splitAt i
      rest <- multiSplits(post)
    } yield pre :: rest
  }
  
  def remove(xs: List[Int], i: Int): List[Int] = (xs take i) ::: (xs drop (i + 1))

  val res = perm(List(1,2,3,4,5), 3)
  
  res foreach println
  
  println(res.size)
  
  val mr = multiSplits("abcdef")
  mr foreach println
  println(mr.size)
}

