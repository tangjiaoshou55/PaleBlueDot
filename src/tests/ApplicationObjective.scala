package tests

import org.scalatest._
import pbd.PaleBlueDot

class ApplicationObjective extends FunSuite {

  val countriesFile: String = "data/countries.txt"
  val citiesFilename: String = "data/cities.csv"

  test("test 1"){
    val testCases: Map[List[Double], List[String]] = Map(
      List(0.0,0.0)->List("gh", "takoradi", "09").sorted,
      List(50.0,120.0)-> List("ru", "priargunsk", "93").sorted,
      List(5000.0,1200.0)-> List("au", "albany", "08").sorted,
    )

    for ((input, expectedOutput) <- testCases) {
      val computedOutput: List[String] = PaleBlueDot.closestCity(citiesFilename, input)
      assert(computedOutput == expectedOutput, input + " -> " + computedOutput)
    }
  }
}
