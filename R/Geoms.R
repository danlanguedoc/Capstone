

#install.packages("scales")
#library(scales)


#' @title geom_timeline
#'
#' @description Function that returns the timeline layer to display earthquake data
#'
#' @inheritParams ggplot2::geom_point
#'
#' @return a layer that can be passed ot ggplot containing the time line data
#'
#' @details
#' The function provides a timeline plot of earthquake data.  The size of the plot markers is related
#'     to the magnitude of the earthquake and the color to the number of reported deaths
#'     the "x" aesthetic is required and maps to the x-axis of the timeline.  The "y" easthetic is
#'     optional and can be used to group data by a variable, i.e. country
#'
#'
#'
#' @examples
#' \dontrun{
#' eq_clean %>% filter(COUNTRY %in% c("JAPAN"), YEAR > 1990) %>%
#'  ggplot(aes(x = DATE,
#'             y = COUNTRY,
#'             color = as.numeric(TOTAL_DEATHS),
#'             size = as.numeric(EQ_PRIMARY)
#'  )) +
#'  geom_timeline() +
#'  theme_timeline() +
#'  labs(title = "Earthquake Fatalities", color = "# deaths", size = "Richter scale value") +
#'  scale_color_gradient(low = "black", high = "red")
#' }
#'
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {

    ggplot2::layer(
          geom = GeomTimeline, mapping = mapping,
          data = data, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, ...)
     )
}



#' @title GeomTimeline
#'
#' @description Implementation of the geom_timeline class
#'
#' @importFrom ggplot2 aes draw_key_point
#' @importFrom grid pointsGrob linesGrob gList gpar
#' @importFrom scales alpha
GeomTimeline <-
     ggplot2::ggproto(
          "GeomTimeline", ggplot2::Geom,
          required_aes = c("x"),
          default_aes = ggplot2::aes(colour = "grey", size = 1.5, alpha = 0.5,
                                     shape = 21, fill = "grey", stroke = 0.5),
          draw_key = ggplot2::draw_key_point,
          draw_panel = function(data, panel_scales, coord) {

              if (!("y" %in% colnames(data))) {
                  data$y <- 0.15
              }

              coords <- coord$transform(data, panel_scales)

              points <- grid::pointsGrob(
                    coords$x, coords$y,
                    pch = coords$shape, size = unit(coords$size / 4, "char"),
                    gp = grid::gpar(
                         col = scales::alpha(coords$colour, coords$alpha),
                         fill = scales::alpha(coords$colour, coords$alpha)
                    )
               )
              y_lines <- unique(coords$y)

              lines <- grid::polylineGrob(
                    x = unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                    y = unit(c(y_lines, y_lines), "npc"),
                    id = rep(seq_along(y_lines), 2),
                    gp = grid::gpar(col = "grey",
                                    lwd = .pt)
               )

              grid::gList(points, lines)
          }
     )


#' @title Theme for timeline in ggplot2
#'
#' @description  This is a simple theme that moves the legend to the
#'    rights so that the deaths labels don't overlap
#'
#' @examples
#' \dontrun{
#' eq_clean %>% filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
#'  ggplot(aes(x = DATE,
#'             y = COUNTRY,
#'             color = as.numeric(TOTAL_DEATHS),
#'             size = as.numeric(EQ_PRIMARY)
#'  )) +
#'  geom_timeline() +
#'  geom_timeline_label(aes(label = LOCATION_NAME), maxCountries = 5) +
#'  theme_timeline() +
#'  labs(title = "Earthquake Fatalities", color = "# deaths", size = "Richter scale value") +
#'  scale_color_gradient(low = "black", high = "red")
#' }
#'
#' @importFrom ggplot2 theme element_blank element_line
#'
#' @export
theme_timeline <- function() {
    ggplot2::theme(
          plot.background = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          legend.key = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          axis.line.x = ggplot2::element_line(size = 1),
          axis.ticks.y = ggplot2::element_blank(),
          legend.position = "right"
     )
}

#' @title geom_timeline_label
#'
#' @description Function that returns the labels for the data markers returned by geom_timeline
#'
#' @inheritParams ggplot2::geom_text
#'
#' @param maxQuakes Optional: it only plots the labels for the
#' largest earthquakes in the selected group in the timeline
#'
#' @return a layer that can be passed to ggplot containing the time line labels
#'
#' @details
#' The function provides a set of labels the can be used with the timeline data
#'     returned by geom_timeline.  The labels are tilted to 45 degrees.
#'     The "label" aesthic is required and should contain the name of a column
#'     that contains the information to display.
#'
#'
#' @examples
#' \dontrun{
#' eq_clean %>% filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
#'  ggplot(aes(x = DATE,
#'             y = COUNTRY,
#'             color = as.numeric(TOTAL_DEATHS),
#'             size = as.numeric(EQ_PRIMARY)
#'  )) +
#'  geom_timeline() +
#'  geom_timeline_label(aes(label = LOCATION_NAME), maxCountries = 5) +
#'  theme_timeline() +
#'  labs(title = "Earthquake Fatalities", color = "# deaths", size = "Richter scale value") +
#'  scale_color_gradient(low = "black", high = "red")
#' }
#' @importFrom ggplot2 layer
#'
#' @export
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", ..., na.rm = FALSE,
                                maxQuakes = NULL, show.legend = NA,
                                inherit.aes = TRUE) {

    ggplot2::layer(
          geom = GeomTimelineLabel, mapping = mapping,
          data = data, stat = stat, position = position,
          show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(na.rm = na.rm, maxQuakes = maxQuakes, ...)
     )
}

#' @title GeomTimelineLabel
#'
#' @description Implementation of the geom_timeline_label class
#'
#' @importFrom ggplot2 draw_key_blank
#' @importFrom grid gList textGrob linesGrob gList gpar
#' @importFrom dplyr %>% group_by top_n ungroup
 GeomTimelineLabel <-
     ggplot2::ggproto(
          "GeomTimelineLabel", ggplot2::Geom,
          required_aes = c("x", "label"),
          draw_key = ggplot2::draw_key_blank,
          setup_data = function(data, params) {
              if (!is.null(params$maxQuakes)) {
                  if (!("size" %in% colnames(data))) {
                      stop(paste("'size' aesthetic needs to be",
                                    "provided when 'maxQuakes' is defined."))
                  }
                  data <- data %>%
                         dplyr::group_by_("group") %>%
                         dplyr::top_n(params$maxQuakes, size) %>%
                         dplyr::ungroup()
              }
              data
          },
          draw_panel = function(data, panel_scales, coord, maxQuakes) {

              if (!("y" %in% colnames(data))) {
                  data$y <- 0.15
              }

              coords <- coord$transform(data, panel_scales)
              n_grp <- length(unique(data$group))
              offset <- 0.2 / n_grp

              lines <- grid::polylineGrob(
                    x = unit(c(coords$x, coords$x), "npc"),
                    y = unit(c(coords$y, coords$y + offset), "npc"),
                    id = rep(1:dim(coords)[1], 2),
                    gp = grid::gpar(
                         col = "grey"
                    )
               )

              names <- grid::textGrob(
                    label = coords$label,
                    x = unit(coords$x, "npc"),
                    y = unit(coords$y + offset, "npc"),
                    just = c("left", "bottom"),
                    rot = 45
               )

              grid::gList(lines, names)
          }
     )


