package top

object Main extends App {

  val keys = 1 to 10 toList
  
  
  keys map (x => x + 1)
  
  keys map (_ + 1)

  keys map (x => incr(x))
  
  keys map incr
  
  for(key <- keys) yield (key + 1)
  
  keys map (x => x + 1) filter (x => x < 10)

  keys map (_ + 1) filter (_ < 10)
  
  keys map incr filter lessThanTen
  
  for(key <- keys; if key < 10) yield (key + 1)

  
  keys reduce ((result, item) => result + item)
  
  keys reduce (_ + _)
  
  keys reduce add
  
  
  keys map incr filter lessThanTen reduceLeft add
  
  (for(key <- keys; if key < 10) yield (key + 1)) reduce (_ + _)
  
  val values = 'a' to 'z' take 10 toArray

  values map (_.toString)
  
  keys map {x => 
  	values map {y =>
  	  (x, y)
  	}
  }
  
  for {
    x <- keys
    y <- values
  } yield (x, y)
  
  val chars = List("ABC", "DEF", "GHI", "JKL", "MNO", "PQRS", "TUV", "WXYZ")

  val digits = 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil
  
  def appendLists(xs: List[String], ys: List[String]): List[String] =
  if (xs == List()) ys
  else xs.head :: appendLists(xs.tail, ys)

  
  val digitCode = digits zip chars toMap;
  
  
  val charCode = for((digit, str) <- digitCode; letter <- str) yield letter -> digit
  
  def wordCode(word: String) = word map (x => charCode(x))
  
  val words = io.Source.fromFile("corncob_lowercase.txt").getLines.toSeq
  
  words groupBy (_.length)

  words groupBy (_.head)

  words groupBy (x => x.head)

  words groupBy (x => wordCode(x))
  
  def add(a: Int, b: Int) = a + b
  
  val pairs = keys zip values toMap
  
  def incr(a: Int) = a + 1
  
  def lessThanTen(a: Int) = a < 10
  
  def prefixes(x: String) =
    for {
      splitPoint <- 1 to x.length
    } yield x.take(splitPoint)


  def possibleSplits(s: String): List[List[String]] = s match {
    case "" => List(List())
    case _ => for {
      i <- (1 to s.length).toList
      post <- possibleSplits(s.drop(i))
    } yield (s.take(i)) :: post
  }
    
  
    

  
}

object A {

  val xs = Seq(1, 2, 3)

//  xs.foreach()
//  xs.map()
//  xs.flatMap()

  xs.exists(_ > 4)

  xs.forall(_ < 4)

  Seq.empty[Int].forall(_ > 100)
  Seq.empty[Int].exists(_ > 100)
  Seq.empty[Int].map(_ + 1)

  val ds = Seq.empty[Int]

  val o1 = Option(10)
  val o2 = Option.empty[Int]

  o1.map(_ + 1)
  o2.map(_ + 1).getOrElse(0)

  xs.isEmpty
  xs.nonEmpty

  o1.isEmpty
  o1.nonEmpty
  o1.isDefined

  o1.forall(_ > 100)
  o1.exists(_ > 100)

  def m: Seq[Int] = {
    xs
    o1

    xs ++ o1
  }


  for {
    x <- xs
    o <- o1.toSeq
  } yield x + o




}