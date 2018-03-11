# install.packages("tidyverse")
# library(tidyverse)
#
# install.packages("lubridate")
# library(lubridate)
#
# install.packages("lettercase")
# library(lettercase)
#
# eq_raw <- read_delim("inst/extdata/signif.txt", "\t")


#' @title eq_location_clean
#'
#' @description Function that removes the country name from the location data
#'
#' @param countryName A character string with the country name to be removed
#' @param location A character string with the location to be cleaned
#'
#' @return a string with the country and any folowing colons or semi-colons
#'     removed from the front
#'
#' @details
#' The function first does sanity chaecks on countryName and location
#'     then checks to see if the location starts with countryName
#'     if yes, the countryName and trailing colons are removed from the start of location
#'     str_ucfirst is then used to convert the location to initial upper case
#'
#' @examples
#' \dontrun{
#' eq_location_clean(eq_raw)
#' }
#'
#' @importFrom stringr str_replace str_trim str_to_title
#'
#' @export
eq_location_clean <- function(data) {
  data <- data %>%
    dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                     stringr::str_replace(paste0(COUNTRY, ":"), "") %>%
                     stringr::str_replace(paste0(COUNTRY, ";"), "") %>%
                     stringr::str_trim("both") %>%
                     stringr::str_to_title())
  data
}


#' @title eq_clean_data
#'
#' @description Function that tidies up the raw earthquake data
#'
#' @param eq_raw a raw dataframe from the NOAA Significant Earthquakes data set
#'
#' @return a dataframe with the cleaned location names, numeric lat/long and DATE column
#'
#' @details
#' The function first replaces NA values in month and day with 1 to give a complete date
#'     it then mutates the column like this
#'     1. creates a new Date class column called "DATE" by combining the YEAR, MONTH and DAY field
#'     2. Converts the LATTITUDE and LONGITUDE columns to numeric
#'     3. Calls eq_location_clean to strip the country name from the location column
#'
#' @examples
#' \dontrun{
#' eq_clean_data(eq_raw)
#' }
#'
#' @importFrom tidyr replace_na
#' @importFrom dplyr mutate
#' @importFrom lubridate make_date
#'
#'
#' @export
eq_clean_data <- function(eq_raw) {
                            tidyr::replace_na(eq_raw, list(MONTH = 1, DAY = 1)) %>%
                            dplyr::mutate(DATE = make_date(year = YEAR, month = MONTH, day = DAY), LATITUDE = as.numeric(LATITUDE), LONGITUDE = as.numeric(LONGITUDE)) %>% ##, LOCATION_NAME = eq_location_clean(COUNTRY, LOCATION_NAME))
                            eq_location_clean()
                                            }

#eq_clean <- eq_clean_data(eq_raw)

