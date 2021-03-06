% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{eq_map}
\alias{eq_map}
\title{eq_map}
\usage{
eq_map(data, annot_col)
}
\arguments{
\item{data}{a clean(with eq_clean_data) dataframe from the NOAA Significant Earthquakes data set}

\item{annot_col}{a string that contains the name of the column that holds the annotation information}
}
\value{
a leaflet object that can be plotted
}
\description{
Uses Leaflet to plot a map the earthquakes in region
}
\details{
The function add circle markers with following attributes
    1. LONGITUDE and LATITUDE from data are mapped to lng and lat paramters in Leaflet
    2. EQ_Primary is mapped to the radius of the marker
    3. the string from the "annot_col" param is mapped to the popup parameter
}
\examples{
\dontrun{
eq_Mexico <- dplyr::filter(eq_clean,COUNTRY == "MEXICO" & lubridate::year(DATE) >2000) \%>\%
   dplyr::mutate(popup_text = eq_create_label(.))
eq_map(eq_Mexico, annot_col = "popup_text")
}

}
