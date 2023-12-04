import scala.io.Source

def loadLines(filename: String): List[String] = Source.fromFile(filename).getLines().toList

def part1(filename: String) = {
  val lines = loadLines(filename)
  var scores = List[Int]()

  for (line <- lines) {
    val input = line.split(": ")
    val numbers = input(1).split(" \\| ").toList
    val winning = getNumbers(numbers(0))
    val selected = getNumbers(numbers(1))
    
    var score = selected.foldLeft(0)(
      (acc, s) => 
        if (winning.contains(s)) { if (acc == 0) { 1 } else { acc * 2 } }
        else { acc }
    )

    scores = scores :+ score
  }

  scores.sum
}

def getNumbers(s: String): List[Int] = {
  var nums = List[Int]()
  for (i <- 0 to s.length by 3) {
    nums = nums :+ s.substring(i, i+2).trim().toInt
  }
  nums
}

def part2(filename: String): Int = {
  var lines = loadLines(filename)
  var stack = Map[Int, Int]()

  for (line <- lines) {
    val input = line.split(": ")
    val numbers = input(1).split(" \\| ").toList
    val winning = getNumbers(numbers(0))
    val selected = getNumbers(numbers(1))
    val cardNo = lines.indexOf(line)

    stack.get(cardNo) match {
      case Some(c) => stack = stack + (cardNo -> (c + 1))
      case None => stack = stack + (cardNo -> 1)
    }
    
    var score = selected.foldLeft(0)(
      (acc, s) => if (winning.contains(s)) { acc + 1 } else { acc }
    )

    for (i <- (cardNo + 1) until (cardNo + score + 1)) {
      stack.get(i) match {
        case Some(c) => stack = stack + (i -> (c + stack.getOrElse(cardNo, 0)))
        case None => stack = stack + (i -> stack.getOrElse(cardNo, 0))
      }
    }
  }

  stack.map(
    entry => entry._2
  ).sum
}
