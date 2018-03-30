library(testthat)
library(Capstone)

filename <- system.file("inst/extdata/signif.txt", package = "Capstone")
context(filename)

test_check("Capstone")
