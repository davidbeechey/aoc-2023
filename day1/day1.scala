import scala.io.Source

def part1(filename: String): Int = {
  var total = 0;
  val lines = Source.fromFile(filename).getLines();
  for (line <- lines) {
    line.find((p) => p.isDigit) match {
      case Some(c) => total += c.asDigit * 10
      case None => sys.error("No digit found in line")
    }
    line.reverse.find((p) => p.isDigit) match {
      case Some(c) => total += c.asDigit
      case None => sys.error("No digit found in line")
    }
  }
  total
}

val options = List[String](
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9"
)

def part2(filename: String): Int = {
  var total = 0;
  val lines = Source.fromFile(filename).getLines();
  for (line <- lines) {
    var matches: List[Int] = Nil
    for (i <- 0 until line.length) {
      for (option <- options) {
        if (line.substring(i, line.length).startsWith(option)) {
          matches = convert(option) :: matches
        }
      }
    }
    total += matches.last * 10 + matches(0)
  }
  total
}

val numbersMap = Map[String, Int](
  "one" -> 1,
  "two" -> 2,
  "three" -> 3,
  "four" -> 4,
  "five" -> 5,
  "six" -> 6,
  "seven" -> 7,
  "eight" -> 8,
  "nine" -> 9,
)

def convert(s: String): Int = if (numbersMap.contains(s)) numbersMap(s) else s.toInt
