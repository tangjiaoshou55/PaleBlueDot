package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task3 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("1 - Country names that have proper capitalization for cityPopulations") {
    val testCases: Map[String, Map[String, Int]] = Map(
      "Andorra" -> Map("la massana" -> 7211, "les escaldes" -> 15854, "ordino" -> 2553, "sant julia de loria" -> 8020),
      "United Arab Emirates" -> Map("abu dhabi" -> 603687, "dubai" -> 1137376, "sharjah" -> 543942),
      "Anguilla" -> Map("the valley" -> 1379)
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Map[String, Int] = PaleBlueDot.cityPopulations(countriesFile, citiesFilename, input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }

  test("2 - Country names that have proper capitalization for aboveAverageCities") {
    val testCases: Map[String, List[String]] = Map(
      "Andorra" -> List("les escaldes"),
      "United Arab Emirates" -> List("dubai"),
      "Albania" -> List("durres", "shkoder", "gjirokaster", "elbasan", "lushnje", "pogradec", "vlore", "berat", "tirana", "fier", "kavaje", "lac", "korce")
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted, input + " -> " + computedOutput)
    }
  }

  test("3 - Test cases that are not countries in the data file for cityPopulations") {
    val testCases: Map[String, Map[String, Int]] = Map(
      "" -> Map(),
      "Not a real country" -> Map(),
      "j a p a n" -> Map()
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: Map[String, Int] = PaleBlueDot.cityPopulations(countriesFile, citiesFilename, input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }

  test("4 - Test cases that are not countries in the data file for aboveAverageCities") {
    val testCases: Map[String, List[String]] = Map(
      "" -> List(),
      "Not a real country" -> List(),
      "j a p a n" -> List(),
      "Aruba" -> List()
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.aboveAverageCities(countriesFile, citiesFilename, input)
      assert(computedOutput.sorted == expectedOutput.sorted, input + " -> " + computedOutput)
    }
  }
}
