<!-- badges: start -->
[![Travis build status](https://travis-ci.com/wuweizhang0723/STAT302project2.svg?branch=master)](https://travis-ci.com/wuweizhang0723/STAT302project2)
[![Codecov test coverage](https://codecov.io/gh/wuweizhang0723/STAT302project2/branch/master/graph/badge.svg)](https://codecov.io/gh/wuweizhang0723/STAT302project2?branch=master)
<!-- badges: end -->

## Use

The vignette demonstrates example usage of all functions.
In order to install the vignettes with the package from Github, use the following code:

``` r
# install.packages("devtools")
devtools::install_github("wuweizhang0723/STAT302project2", build_vignette = TRUE, build_opts = c())
library(STAT302project2)
# Use this to view the vignette in the STAT302project2 HTML help
help(package = "STAT302project2", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302project2")
```
