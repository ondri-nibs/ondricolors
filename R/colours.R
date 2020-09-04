#' Complete list of ONDRI palettes
#'
#'
#' @export
#'

# list of all palettes
ondri_palettes <- list(
  Cohort1 = c("#818385", # Dark
              "#F0F2F4", # Light
              "#F9423A", # VCI/CVD Red
              "#F9423A", # VCI/CVD Red
              "#ED8B00", # FTD Orange
              "#62B5E5", # ADMCI Blue
              "#6BA539", # PD Green
              "#A77BCA"  # ALS Purple
              ),
  Cohort2 = c("#F9423A", # VCI/CVD Red
              "#F9423A", # VCI/CVD Red
              "#ED8B00", # FTD Orange
              "#62B5E5", # ADMCI Blue
              "#6BA539", # PD Green
              "#A77BCA"  # ALS Purple
              )
)

# renaming columns to match hex values
names(ondri_palettes$Cohort1) <- c("Dark", "Light", "VCI", "CVD", "FTD", "ADMCI", "PD", "ALS")
names(ondri_palettes$Cohort2) <- c("CVD", "VCI", "FTD", "ADMCI", "PD", "ALS")



#' An ONDRI palette generator
#'
#' These are a handful of colour palettes standard within ONDRI.
#'
#' @param name Name of desired palette. Choices are:
#'   \code{Cohort1}, \code{Cohort2}
#' @param start Starting index of colours desired.
#'   If omitted, starts at 1.
#' @param end Ending index of colours desired.
#'   If omitted, ends at last colour in palette list.
#' @param n Number of colours desired for type="continuous".
#'   If omitted, uses amount of discrete colours in range [start:end].
#' @param is_discrete Either TRUE or FALSE. Set to FALSE if you want
#'   to automatically interpolate between colours.
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#' ondri_palette("Cohort1")
#' ondri_palette("Cohort1", start = 3, end = 7) # Returns only the VCI, FTD, ADMCI, PD, and ALS colours
#' ondri_palette("Cohort2", n = 20, is_discrete = FALSE)
#'
#' # If you need more colours than normally found in a palette, you
#' # can use a continuous palette to interpolate between existing
#' # colours
#' pal <- ondri_palette(name = "Cohort1", start = 3, n = 20, is_discrete = FALSE)
#' image(volcano, col = pal)
#'
#' @details
#' This function heavily borrows from Karthik Ram's wesanderson package, and specifically
#' \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
ondri_palette <- function(name, start = 1, end, n, is_discrete = TRUE) {

  pal <- ondri_palettes[[name]]
  if (is.null(pal))
    stop("Palette not found.")

  if (missing(end)) {
    end <- length(pal)
  }

  if (missing(n)) {
    n <- end - start + 1
  }

  if (end > length(pal) || start > end) {
    stop("Range of requested colors outside of palette range")
  }

  if (is_discrete) {
    out <- pal[start:end]
  } else {
    out <- grDevices::colorRampPalette(pal[start:end])(n)
  }

  structure(out, class = "palette", name = name)
}


#' @export
#' @title print.palette
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
#' @details
#' This function heavily borrows from Karthik Ram's wesanderson package, and specifically
#' \url{https://github.com/karthik/wesanderson/blob/master/R/colors.R}
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}
