#' @title ONDRI Palettes
#' @name ondricolors
#' @docType package
#' @details ONDRI's color palettes
#' @description A lightweight package for the Ontario Neurodegenerative Disease Research Initiative (ONDRI) color palettes.
#' Returns a palette object (vector of colors) similar to Wes Anderson Palettes (https://github.com/karthik/wesanderson).
#' The palette attributes certain colors to cohort labels.
#' @examples
#' \dontrun{
#' ## quick visualization of palettes
#' ## continuous example
#'  pal1 <- ondri_palette(name = "all_colours", start = 3, n = 20, is_discrete = FALSE)
#' image(volcano, col = pal1)
#'
#' ## discrete example
#' pal2 <- ondri_palette(name = "cohorts")
#' image(volcano, col = pal2)
#'
#' ## three toy examples
#' library(ondricolors)
#' data("toy_ONDRI_data")
#' ## first in base R
#' ## for the filled pchs
#' col2use <- ondri_palette("cohorts")[as.character(toy_ONDRI_data$COHORT)]
#' plot(toy_ONDRI_data$X, toy_ONDRI_data$Y, col = col2use, pch = 20)
#'
## for the pchs with bgs
#' plot(toy_ONDRI_data$X, toy_ONDRI_data$Y, bg = col2use, pch = 21)
#'
#' ## now with ggplot2
#' library(ggplot2)
#' ggp_color <- ggplot(toy_ONDRI_data, aes(x = X, y = Y, color = COHORT)) +
#'   geom_point() +
#'   scale_color_manual(values = ondri_palette("cohorts"))

#' ggp_color
#' }
NULL


#' Toy ONDRI data
#'
#' A small toy data set to help illustrate how to use the ondricolors package
#'
#' @format A data.frame with four columns:
#' \describe{
#' \item{SUBJECT}{A vector of SUBJECT IDs}
#' \item{COHORT}{A vector of COHORT values; one of five ("ADMCI","ALS","FTD","PD","VCI")}
#' \item{X}{Some arbitrary values for the x-axis in a plot}
#' \item{Y}{Some arbitrary values for the y-axis in a plot}
#' }
"toy_ONDRI_data"
