README
================
Derek Beaton & Jeremy Tanuan
November 25, 2019

# **ONDRI Themes**

Standardized themes, colors, and templates for the ONDRI project’s
colors and presentations. Developed by Jeremy Tanuan and Derek Beaton.
The package is heavily influenced by and copies key parts of [Karthik
Ram’s `wesanderson` palette
package](https://github.com/karthik/wesanderson).

## **Installation**

Make sure you have [R](https://cran.r-project.org/) and
[RStudio](https://www.rstudio.com/products/rstudio/download/). Open
RStudio and run the following commands in the RStudio console (the
bottom left pane) to install the necessary packages if you have not
already done so.

First install the `ondricolors` package

``` r
if(!require('devtools')){
  devtools::install_github(repo = "ondri-nibs/ondricolors")
}
```

    ## Loading required package: devtools

    ## Loading required package: usethis

Then load the package as

``` r
library(ondricolors)
```

You will know that the packages are loaded if the checkbox beside each
in the *Packages* tab (the bottom right pane) are checked. Add the
checkmark to each for any that are missing from the necessary packages.

## **Palettes**

#### **Cohort1**

``` r
ondri_palette("Cohort1")
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### **Cohort2**

``` r
ondri_palette("Cohort2")
```

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## **Plots**

``` r
## continuous example
pal <- ondri_palette("Cohort1", start = 3, n = 20, is_discrete = F)
image(volcano, col = pal)
```

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
## discrete example
pal <- ondri_palette("Cohort1")
image(volcano, col = pal)
```

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

## **Usage and examples**

To start let’s see the palette colors

``` r
# See all ONDRI palettes
names(ondri_palettes)
```

    ## [1] "Cohort1" "Cohort2"

### A tidy & ggplot2 example

Here we provide an example with `ggplot2`.

``` r
library(ggplot2)
library(ondricolors)

data('toy_ONDRI_data')

ggp_color <- ggplot(toy_ONDRI_data, aes(x = X, y = Y, color = COHORT)) +
geom_point() +
scale_color_manual(values = ondri_palette("Cohort2"))

ggp_color
```

![](README_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### A base R example

And here we provide an example with base `R`

``` r
library(ondricolors)
data('toy_ONDRI_data')

## for the filled pchs
plot(toy_ONDRI_data$X, toy_ONDRI_data$Y, col = ondri_palette("Cohort2")[as.character(toy_ONDRI_data$COHORT)], pch = 20)
```

![](README_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
## for the pchs with bgs
plot(toy_ONDRI_data$X, toy_ONDRI_data$Y, bg = ondri_palette("Cohort2")[as.character(toy_ONDRI_data$COHORT)], pch = 21)
```

![](README_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->
