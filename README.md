
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tracerr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of tracerr is to …

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jspaezp/tracerr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tracerr)
## basic example code
## 
r_str <- "
    b <- 1:2
    a <- sum(b)
    a <- sum(a)
    c <- sum(a)
    print(c)
    "

parsed_expr <- parse(text = r_str)
get_dependencies(parsed_expr, unique_names = FALSE)
#> # A tibble: 3 x 2
#>   From  To   
#>   <chr> <chr>
#> 1 b     a    
#> 2 a     a    
#> 3 a     c
```

``` r
get_dependencies(parsed_expr, unique_names = TRUE)
#> # A tibble: 3 x 2
#>   From  To   
#>   <chr> <chr>
#> 1 b     a    
#> 2 a     a.1  
#> 3 a.1   c
```

``` r
library(igraph)
#> 
#> Attaching package: 'igraph'
#> The following objects are masked from 'package:stats':
#> 
#>     decompose, spectrum
#> The following object is masked from 'package:base':
#> 
#>     union
g <- igraph::graph_from_data_frame(
    get_dependencies(parsed_expr, unique_names = TRUE))
plot(g)
```

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" />
