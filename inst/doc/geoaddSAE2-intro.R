## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", 
                      warning = FALSE, message = FALSE)

## ----setup--------------------------------------------------------------------
library(geoaddSAE2)
data(simulated_sae)
head(simulated_sae)

## -----------------------------------------------------------------------------
fit <- geosae(
  data      = simulated_sae,
  formula   = y ~ x1,
  vardir    = vardir,
  nonlinear = "x2",
  spatial   = c("lat", "lon"),
  B         = 50,
  seed      = 1
)

fit

## -----------------------------------------------------------------------------
summary(fit)

## -----------------------------------------------------------------------------
fit_cmp <- geosae(
  data      = simulated_sae,
  formula   = y ~ x1,
  vardir    = vardir,
  nonlinear = "x2",
  spatial   = c("lat", "lon"),
  compare   = TRUE,
  B         = 50,
  seed      = 1
)
fit_cmp$comparison

