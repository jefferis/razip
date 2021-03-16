
# razip

<!-- badges: start -->
[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![Docs](https://img.shields.io/badge/docs-100%25-brightgreen.svg)](https://jefferis.github.io/razip/reference/)
[![Travis build status](https://travis-ci.com/jefferis/razip.svg?branch=master)](https://travis-ci.com/jefferis/razip)
[![R-CMD-check](https://github.com/jefferis/razip/workflows/R-CMD-check/badge.svg)](https://github.com/jefferis/razip/actions)
<!-- badges: end -->

The goal of razip is to provide efficient random access to the contents of large
zip files by cacheing zip files offsets in memory. Contents can then be read
directly to memory, optionally unserialising. The main intended use case is the
storage of collections of tens of thousands of serialised R objects (e.g. [nat
neurnlists](https://natverse.org/nat/reference/neuronlist.html) neurons) into
single zip files that may be GB in size while still allowing efficient (order
5ms) read access times.

## Installation

You can install the development version of razip from github

``` r
remotes::install_github("jefferis/razip")
```
you also need to ensure that you have the latest version of the zip package installed

``` r
remotes::install_github("r-lib/zip")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(razip)
# written as 
# write.neurons(nl, "~/Desktop/flywire_neurons_flow_FlyWireqs.zip", format='qs')
raz=RAZip$new("~/Desktop/flywire_neurons_flow_FlyWireqs.zip")
raz
zl=raz$ziplist()
bench::mark(s1=raz$get(sample(zl$filename, 1)), check = F)
bench::mark(s5=raz$mget(sample(zl$filename, 5)), check = F)
```

