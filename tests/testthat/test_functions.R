library(Capstone)
library(lubridate)
library(ggplot2)
library(dplyr)


context("CApstone tests")

filename <- system.file("inst/extdata/signif.txt", package = "Capstone")
data <- readr::read_delim(filename, delim = "\t")


test_that("The data type of returned by eq_clean_data is a data frame", {
  expect_is(eq_clean_data(data), "data.frame")
})

test_that("The data type of returned by eq_clean_data$DATE is Date", {
  expect_is(eq_clean_data(data)$DATE, "Date")
})

test_that("The data type of LATITUDE and LONGITUDE returned by eq_clean_data are numeric", {
  expect_is(eq_clean_data(data)$LATITUDE, "numeric")
  expect_is(eq_clean_data(data)$LONGITUDE, "numeric")
})

test_that("The data type of returned by eq_location_clean is a data frame", {
  expect_is(eq_location_clean(data), "data.frame")
})

test_that("The data type of returned by geom_timeline is a ggplot object", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline()
  expect_is(g, "ggplot")
})

test_that("The data type of returned by geom_timeline_label is a ggplot object", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    geom_timeline_label(aes(label = LOCATION_NAME))
  expect_is(g, "ggplot")
})

test_that("The data type of returned by theme_timeline is a ggplot object", {
  g <- data %>% eq_clean_data() %>%
    dplyr::filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
    ggplot2::ggplot(ggplot2::aes(x = DATE,
                                 y = COUNTRY,
                                 color = as.numeric(TOTAL_DEATHS),
                                 size = as.numeric(EQ_PRIMARY)
    )) +
    theme_timeline()
  expect_is(g, "ggplot")
})

test_that("The data type of returned by eq_map is a leaflet object", {
  l <- data %>%
    eq_clean_data() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    dplyr::mutate(popup_text = eq_create_label(.)) %>%
    eq_map(annot_col = "popup_text")
  expect_is(l, "leaflet")
})

test_that("The data type of returned by eq_create_label is character vector", {
  expect_is(eq_create_label(data), "character")
})


