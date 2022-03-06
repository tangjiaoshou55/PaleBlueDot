package pbd

import java.awt.Desktop
import java.net.URI
import scala.io.{BufferedSource, Source}

object PaleBlueDot {


  /**
   * Task 1
   *
   * Given a country name using a mix of case (upper/lower), return the country code in all lowercase letters
   *
   * Ex. If "Heard Island and McDonald Islands#HM" is a line countriesFilename and the countryName input
   * of your method is "hEaRd IsLaNd AnD mCdOnAlD iSlAnDs" the returned value is "hm"
   *
   * If countryName is not in the file, return the empty String: ""
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param countryName       The name of the country to lookup in the file with any mix of upper/lower-case
   * @return The two letter country code for countryName in lowercase letters
   */
  def getCountryCode(countriesFilename: String, countryName: String): String = {
    val inputName = countryName.toLowerCase()

    val countriesFile: BufferedSource = Source.fromFile(countriesFilename)
    for (line <- countriesFile.getLines()){
      val splits: Array[String] = line.split("#")
      val countryNameInData: String = splits(0).toLowerCase()

      if (inputName == countryNameInData) {
        return splits(1).toLowerCase()
      }
    }
    ""
  }


  /**
   * Task 2
   *
   * Find the average population of cities in a country
   * regardless.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return The average population of cities in the given country
   */
  def averagePopulation(countriesFilename: String, citiesFilename: String, countryName: String): Double = {
    val countryCode = getCountryCode(countriesFilename, countryName)

    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)

    var sumPopulation: Double = 0.0
    var sumCities: Double = 0.0

    for (line <- citiesFile.getLines()){
      val splits: Array[String] = line.split(",")
      val countryCodeInData: String = splits(0)
      
      if (countryCode == countryCodeInData) {
        sumPopulation += splits(3).toDouble
        sumCities += 1
      }
    }

    if (sumCities == 0.0){
      return 0.0
    }

    return sumPopulation/sumCities
  }


  /**
   * Task 3
   */

  /**
   * Returns a Map[cityName -> population] for all cities in the given county. The name of each
   * city should match exactly how it appears in citiesFilename and the population is read from the file
   * and converted to an Int. The country name may contain any mix of upper/lower-case letters.
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return A Map containing the name and population of every city in the given country
   */
  def cityPopulations(countriesFilename: String, citiesFilename: String, countryName: String): Map[String, Int] = {
    val countryCode = getCountryCode(countriesFilename, countryName)
    var mapOfPopulation = Map[String, Int]()

    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)

    for (line <- citiesFile.getLines()) {
      val splits: Array[String] = line.split(",")
      val countryCodeInData: String = splits(0)

      if (countryCode == countryCodeInData) {
        mapOfPopulation = mapOfPopulation + (splits(1) -> splits(3).toInt)
      }
    }

    return mapOfPopulation
  }

  /**
   * Returns a List of city names in the given county and with above average population for that country
   *
   * @param countriesFilename Name of the file containing country names and codes
   * @param citiesFilename    Name of the file containing city name, population, and location data
   * @param countryName       The name of the country with any mix of upper/lower-case
   * @return All city names in given country with a population > the average populations of cities in that country
   */
  def aboveAverageCities(countriesFilename: String, citiesFilename: String, countryName: String): List[String] = {
    val avePopulation = averagePopulation(countriesFilename, citiesFilename, countryName)
    var listOfAboveAve = List[String]()
    val mapOfAvePopulation = cityPopulations(countriesFilename, citiesFilename, countryName)

    for ((key, value) <- mapOfAvePopulation){
      if (value > avePopulation){
        listOfAboveAve = listOfAboveAve :+ key
      }
    }

    return listOfAboveAve
  }


  /**
   * Application Objective
   *
   * You find yourself stranded in an unfamiliar place with no signs of civilization. You don't have much with you,
   * but you do have a locator that gives your current latitude/longitude, a csv file of cities, and your final
   * submission to the PaleBlueDot assignment from CSE116 (What luck!). You decide that finding and walking
   * directly to the closest city will give you the best chance to survive.
   *
   * Return the closest city to the given location in terms of greater circle distance which is the shortest distance
   * needed to walk along the surface of the Earth to reach a city.
   *
   * @param citiesFilename Name of the file containing city name, population, and location data
   * @param location       A location on Earth given as a List containing latitude and longitude coordinates
   * @return The city closest to the given location as a List containing country code, city name, and region
   *         exactly as they appear in the cities file (ie. the List should have exactly 3 values to return
   *         a single city
   */
  def closestCity(citiesFilename: String, location: List[Double]): List[String] = {

    val citiesFile: BufferedSource = Source.fromFile(citiesFilename)

    val R = 6371e3; // metres
    val φ1 = location(0) * math.Pi/180; // φ, λ in radians
    var min = 0.0
    var cityList: List[String] = List()

    for (line <- citiesFile.getLines()) {
      val splits: Array[String] = line.split(",")

      if (splits(0) != "Country"){
        val φ2 = splits(4).toDouble * math.Pi/180;
        val Δφ = (splits(4).toDouble-location(0)) * math.Pi/180;
        val Δλ = (splits(5).toDouble-location(1)) * math.Pi/180;

        val a = math.sin(Δφ/2) * math.sin(Δφ/2) +
          math.cos(φ1) * math.cos(φ2) *
            math.sin(Δλ/2) * math.sin(Δλ/2);
        val c = 2 * math.atan2(math.sqrt(a), math.sqrt(1-a));

        val d = R * c; // in metres

        if (cityList == List()) {
          min = d
          cityList = cityList :+ splits(0)
          cityList = cityList :+ splits(1)
          cityList = cityList :+ splits(2)
        }
        else if (min > d) {
          min = d
          cityList.updated(0, splits(0))
          cityList.updated(1, splits(1))
          cityList.updated(2, splits(2))
        }
      }

    }

    return cityList
  }


  /**
   * Helper Method
   *
   * Opens Google Maps at a specific location. The location is a List containing the latitude then longitude as Doubles
   *
   * @param location The location to open in the format List(Latitude, Longitude)
   */
  def openMap(location: List[Double]): Unit = {
    if (Desktop.isDesktopSupported && Desktop.getDesktop.isSupported(Desktop.Action.BROWSE)) {
      val url: String = "http://maps.google.com/maps?t=m&q=loc:" + location.head.toString + "+" + location(1).toString
      Desktop.getDesktop.browse(new URI(url))
    } else {
      println("Opening the browser not supported")
    }
  }


  def main(args: Array[String]): Unit = {
    print(closestCity("data/cities.csv", List(0.0,0.0)))
  }

}
