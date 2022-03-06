package tests

import org.scalatest._
import pbd.PaleBlueDot

class Task2 extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("1 - Country names that have proper capitalization") {
    val testCases: Map[String, Double] = Map(
      "UGANDA" -> 44471.1975308642,
      "SOUTH africa" -> 142352.0202020202,
      "Japan" -> 134539.0834437086
    )

    for ((input, expectedOutput) <- testCases) {
      val computed0utput: Double = PaleBlueDot.averagePopulation(countriesFile, citiesFilename, input)
      assert(Math.abs(computed0utput - expectedOutput) < 0.001, input + "->" + computed0utput)
    }
  }

  test("2 - Test cases that are not countries in the data file") {
    val testCases: Map[String, Double] = Map(
      "" -> 0.0,
      "Not a real country" -> 0.0,
      "j a p a n" -> 0.0
    )

    for ((input, expectedOutput) <- testCases) {
      val computed0utput: Double = PaleBlueDot.averagePopulation(countriesFile, citiesFilename, input)
      assert(Math.abs(computed0utput - expectedOutput) < 0.001, input + "->" + computed0utput)
    }
  }
}