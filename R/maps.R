#install.packages("leaflet")
#library(leaflet)


#' @title eq_map
#'
#' @description Uses Leaflet to plot a map the earthquakes in region
#'
#' @param data a clean(with eq_clean_data) dataframe from the NOAA Significant Earthquakes data set
#' @param annot_col a string that contains the name of the column that holds the annotation information
#'
#' @return a leaflet object that can be plotted
#'
#' @details
#' The function add circle markers with following attributes
#'     1. LONGITUDE and LATITUDE from data are mapped to lng and lat paramters in Leaflet
#'     2. EQ_Primary is mapped to the radius of the marker
#'     3. the string from the "annot_col" param is mapped to the popup parameter
#'
#' @examples
#' \dontrun{
#' eq_Mexico <- dplyr::filter(eq_clean,COUNTRY == "MEXICO" & lubridate::year(DATE) >2000) %>%
#'    dplyr::mutate(popup_text = eq_create_label(.))
#' eq_map(eq_Mexico, annot_col = "popup_text")
#' }
#'
#' @importFrom leaflet leaflet addTiles AddCircleMarkers
#'
#' @export
eq_map <- function(data, annot_col) {

  out <- leaflet::leaflet() %>%
         leaflet::addTiles() %>%
         leaflet::addCircleMarkers(lng = data$LONGITUDE, lat = data$LATITUDE,
                                   radius = data$EQ_PRIMARY, weight = 1,
                                   popup = data[[annot_col]])

  out
}


#' @title eq_create_label
#'
#' @description creates the HTML to create the popup used in Leaflet
#'
#' @param data a clean(with eq_clean_data) dataframe from the NOAA Significant Earthquakes data set
#'
#' @return a string containing the html code
#'
#' @details
#' The function creates an html chunk wiht labels and values to represent the Location, Total Deaths and
#'    Magnitude.  If any of these three is NA, the value and it's corresponding label
#'    is omitted from the returned string
#'
#' @examples
#' \dontrun{
#' eq_Mexico <- dplyr::filter(eq_clean,COUNTRY == "MEXICO" & lubridate::year(DATE) >2000) %>%
#'   dplyr::mutate(popup_text = eq_create_label(.))
#' }
#'
#'
#' @export
eq_create_label <- function(data){
                  locn <- if_else(!is.na(data$LOCATION_NAME),paste0("<strong>Location: </strong>",data$LOCATION_NAME),"")
                  deaths <- if_else(!is.na(data$TOTAL_DEATHS),paste0("<br/><strong>Total Deaths: </strong>",data$TOTAL_DEATHS),"")
                  mag <- if_else(!is.na(data$EQ_PRIMARY),paste0("<br/><strong>Magnitude: </strong>",data$EQ_PRIMARY),"")
                  paste(locn,deaths,mag)
}




