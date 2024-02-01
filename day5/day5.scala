import scala.io.Source
import scala.util.control.Breaks._

def loadLines(filename: String): List[String] = Source.fromFile(filename).getLines().toList

type Seed = Long
type Destination = Long
type Source = Long
type Range = Long
case class Almanac(destination: Destination, source: Source, range: Range)

def part1(filename: String) = {
  val lines = loadLines(filename)
  val seeds: List[Seed] = lines(0).substring(7, lines(0).length).split(" ").map((s) => s.toLong).toList
  val almanac = parseAlmanac(lines.slice(1, lines.length))

  var locationNums = List[Seed]()

  for (seed <- seeds) {
    var current: Seed = seed
    for (maps <- almanac) {
      breakable(
        for (map <- maps) {
          val range = map.source until map.source + map.range
          if (range.contains(current)) {
            current = map.destination + (current - map.source)
            break
          }
        }
      )
    }
    locationNums = locationNums :+ current
  }

  locationNums.min
}

def parseAlmanac(lines: List[String]): List[List[Almanac]] = lines.foldLeft(List[List[Almanac]]())(
  (acc, s) => s match {
    case s if s.length > 0 && s(0).isDigit => acc.updated(acc.length - 1, acc.last :+ parseAlmanacLine(s))
    case _ if s.length > 0 && s(0).isLetter => acc :+ List[Almanac]()
    case _ => acc
  }
)

def parseAlmanacLine(line: String): Almanac = {
  val parts = line.split(" ").map((s) => s.toLong).toList
  Almanac(parts(0), parts(1), parts(2))
}

type SeedRange = List[(Seed, Seed)]

def part2(filename: String) = {
  val lines = loadLines(filename)
  val originalSeeds: List[Seed] = lines(0).substring(7, lines(0).length).split(" ").map((s) => s.toLong).toList
  // val seeds = originalSeeds.foldLeft(List[Seed]())(
  //   (acc, s) => s match {
  //     case s if originalSeeds.indexOf(s) % 2 == 1 => {
  //       acc ++ ((originalSeeds(originalSeeds.indexOf(s) - 1) until originalSeeds(originalSeeds.indexOf(s) - 1) + s).toList)
  //     }
  //     case _ => acc
  //   }
  // )
  // println(seeds)
  val seedRanges = originalSeeds.foldLeft(List[(Seed, Seed)]())(
    (acc, s) => s match {
      case s if originalSeeds.indexOf(s) % 2 == 1 => {
        acc :+ ((originalSeeds(originalSeeds.indexOf(s) - 1), originalSeeds(originalSeeds.indexOf(s) - 1) + s))
      }
      case _ => acc
    }
  )
  println(seedRanges)

  val almanac = parseAlmanac(lines.slice(1, lines.length))

  var minLocationNum: Seed = 1000000000000000000L

  for (seedRange <- seedRanges) {
    var current: Seed = seed
    for (maps <- almanac) {
      breakable(
        for (map <- maps) {
          val range = map.source until map.source + map.range
          if (range.contains(current)) {
            current = map.destination + (current - map.source)
            break
          }
        }
      )
    }
    if (current < minLocationNum) minLocationNum = current
  }

  minLocationNum
}
