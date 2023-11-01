20231030_AssignmentB1
================
Torin
2023-10-31

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.4.2     ✔ purrr   1.0.1
    ## ✔ tibble  3.2.1     ✔ dplyr   1.1.1
    ## ✔ tidyr   1.3.0     ✔ stringr 1.5.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## Warning: package 'ggplot2' was built under R version 4.2.3

    ## Warning: package 'tibble' was built under R version 4.2.3

    ## Warning: package 'tidyr' was built under R version 4.2.3

    ## Warning: package 'purrr' was built under R version 4.2.3

    ## Warning: package 'dplyr' was built under R version 4.2.3

    ## Warning: package 'stringr' was built under R version 4.2.3

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(testthat)
```

    ## Warning: package 'testthat' was built under R version 4.2.3

    ## 
    ## Attaching package: 'testthat'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null

    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches

``` r
library(gapminder)
```

    ## Warning: package 'gapminder' was built under R version 4.2.3

# Creating a Function

``` r
#Create a function that calculates the standard deviation of a vector or list of numerics representing a sample
#' Title: stdev
#'
#' @description This function calculates the standard deviation of a vector of numbers.
#' @param w a numeric vector, named as per conventions for vectors
#' @param na.rm a logical operator that determines whether missing values are kept (if FALSE) or removed (if TRUE)
#'
#' @return The function returns the standard deviation of the vector of numerics. The mean and length of the vector are also printed as the function runs, to double-check that the values used for calculating the standard deviation are correct.

stdev <- function(w) {
  stopifnot(is.numeric(w))
  w = w[!is.na(w)]
  mu = mean(w)
  print(paste("Mean:", mu))
  n = length(w)
  print(paste("Length:", n))
  sum_sq = 0
  for (i in 1:length(w)) {
    sum_sq = sum_sq + (w[i] - mu)^2
  }
  sd_sq = sum_sq/(n - 1)
  stdeviation = sqrt(sd_sq)
  print("Standard deviation:")
  return(stdeviation)
}
```

# Examples

``` r
#Example 1: calculate the standard deviation of a vector
vec <- c(2,3,4,5,10,12)
stdev(w = vec)
```

    ## [1] "Mean: 6"
    ## [1] "Length: 6"
    ## [1] "Standard deviation:"

    ## [1] 4.049691

``` r
#Example 2: calculate the standard deviation of a vector with missing values
vec1 <- c(2,3,4,5,NA,NA,10,12)
stdev(w = vec1)
```

    ## [1] "Mean: 6"
    ## [1] "Length: 6"
    ## [1] "Standard deviation:"

    ## [1] 4.049691

``` r
#Example 3: Calculate the standard deviation of a numeric column in the gapminder dataset
stdev(gapminder$lifeExp)
```

    ## [1] "Mean: 59.4744393661972"
    ## [1] "Length: 1704"
    ## [1] "Standard deviation:"

    ## [1] 12.91711

# Testing

``` r
# Test that the function output is the same as the built-in dplyr function sd()
test_that("vec", {expect_equal(sd(vec), stdev(vec))})
```

    ## [1] "Mean: 6"
    ## [1] "Length: 6"
    ## [1] "Standard deviation:"
    ## Test passed 🥳

``` r
#Test that the function returns an error if w is not numeric
vec2 <- c("apple", "banana", "orange")
test_that("vec", {expect_error(stdev(vec2))})
```

    ## Test passed 🌈

``` r
#Test that the function returns the same output regardless of missing values in the vector
test_that("vec", {expect_equal(stdev(vec), stdev(vec1))})
```

    ## [1] "Mean: 6"
    ## [1] "Length: 6"
    ## [1] "Standard deviation:"
    ## [1] "Mean: 6"
    ## [1] "Length: 6"
    ## [1] "Standard deviation:"
    ## Test passed 😸
