## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

has_spdep <- requireNamespace("spdep", quietly = TRUE)

## ----setup--------------------------------------------------------------------
library(geoaddSAE2)

data(simulated_sae)

head(simulated_sae)

## ----plot-nonlinear, fig.width = 7, fig.height = 3.5--------------------------
library(mgcv)
d <- simulated_sae
w <- 1 / d$vardir

nonlinear_candidates <- c("x2", "x3")

smooth_models <- lapply(
  nonlinear_candidates,
  function(v) {
    gam(
      as.formula(sprintf("y ~ s(%s, bs = 'ps')", v)),
      data = d,
      weights = w,
      method = "REML"
    )
  }
)

names(smooth_models) <- nonlinear_candidates

op <- par(mfrow = c(1, length(smooth_models)))

for (v in names(smooth_models)) {
  plot(
    smooth_models[[v]],
    residuals = TRUE,
    pch = 16,
    cex = 0.6,
    shade = TRUE,
    xlab = v,
    ylab = sprintf("s(%s)", v)
  )
}

par(op)

## ----moran, eval = has_spdep--------------------------------------------------
coords <- cbind(d$lon, d$lat)

nb <- spdep::knn2nb(spdep::knearneigh(coords, k = 4))
lw <- spdep::nb2listw(nb, style = "W")

fit_nonspatial <- mgcv::gam(y ~ x1 + x2 + s(x3, bs = "ps"),
                            data = d,weights = 1 / vardir,method = "REML")

res_std <- residuals(fit_nonspatial,type = "pearson")
spdep::moran.test(res_std,lw)

set.seed(1)
spdep::moran.mc(res_std,lw,nsim = 999)

## ----geoadditive-fit----------------------------------------------------------
fit <- geosae(
  data      = simulated_sae,
  formula   = y ~ x1 + x2,
  vardir    = vardir,
  nonlinear = "x3",
  spatial   = c("lat", "lon"),
  B         = 50,
  seed      = 1
)

fit

## ----geoadditive-summary------------------------------------------------------
summary(fit)

## ----model-comparison---------------------------------------------------------
fit_cmp <- geosae(
  data      = simulated_sae,
  formula   = y ~ x1 + x2,
  vardir    = vardir,
  nonlinear = "x3",
  spatial   = c("lat", "lon"),
  compare   = TRUE,
  B         = 50,
  seed      = 1
)

fit_cmp$comparison

## ----complete-example---------------------------------------------------------
library(geoaddSAE2)

data(simulated_sae)

fit <- geosae(
  data      = simulated_sae,
  formula   = y ~ x1 + x2,
  vardir    = vardir,
  nonlinear = "x3",
  spatial   = c("lat", "lon"),
  compare   = TRUE,
  B         = 50,
  seed      = 1
)

summary(fit)

head(fit$estimation)

fit$parameters

fit$diagnostics

fit$comparison

