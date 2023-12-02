import scala.io.Source

def part1(filename: String): Int = {
  var ids: List[Int] = Nil;
  val lines = Source.fromFile(filename).getLines();

  val maxRed = 12;
  val maxGreen = 13;
  val maxBlue = 14;

  for (line <- lines) {
    var valid = true;
    var game = line.substring(5, line.length).split(": ")
    var id = game(0).toInt
    val sets = game(1).split("; ");
    for (set <- sets) {
      val cubes = set.split(", ");
      for (cube <- cubes) {
        cube.split(" ") match {
          case Array(n, "red") => if (n.toInt > maxRed) then valid = false
          case Array(n, "green") => if (n.toInt > maxGreen) then valid = false
          case Array(n, "blue") => if (n.toInt > maxBlue) then valid = false
        }
      }
    }
    if (valid) { ids = ids :+ id }
  }

  ids.sum
}

def part2(filename: String): Int = {
  var powers: List[Int] = Nil;
  val lines = Source.fromFile(filename).getLines();

  for (line <- lines) {
    val sets = line.substring(5, line.length).split(": ")(1).split("; ");
    var minRed = 0
    var minGreen = 0
    var minBlue = 0
    for (set <- sets) {
      val cubes = set.split(", ");
      for (cube <- cubes) {
        cube.split(" ") match {
          case Array(n, "red") => if (n.toInt > minRed) then minRed = n.toInt
          case Array(n, "green") => if (n.toInt > minGreen) then minGreen = n.toInt
          case Array(n, "blue") => if (n.toInt > minBlue) then minBlue = n.toInt
        }
      }
    }
    powers = (minRed * minGreen * minBlue) :: powers
  }

  powers.sum
}
