base_args <- function(...) {
  data(simulated_sae, package = "geoaddSAE2", envir = environment())
  args <- list(data = simulated_sae, formula = y ~ x1 + x2, vardir = quote(vardir),
               nonlinear = "x3", spatial = c("lat", "lon"), B = 2, seed = 1)
  modifyList(args, list(...))
}
run <- function(args) do.call(geosae, args)

test_that("data bukan data frame", {
  expect_error(run(base_args(data = 1:10)), "must be a data frame")
})

test_that("formula bukan formula", {
  expect_error(run(base_args(formula = "y ~ x1")), "must be a formula")
})

test_that("vardir tidak ada / nonpositif", {
  expect_error(run(base_args(vardir = quote(tidak_ada))), "name of a single column")
  d <- simulated_sae; d$vardir[1] <- 0
  expect_error(run(base_args(data = d)), "positive numeric column")
  d$vardir[1] <- -0.1
  expect_error(run(base_args(data = d)), "positive numeric column")
})

test_that("respons tidak ditemukan", {
  expect_error(run(base_args(formula = tidak_ada ~ x1)), "not found in `data`")
})

test_that("nonlinear: kosong, tidak ada, tumpang tindih, bukan numerik", {
  expect_error(run(base_args(nonlinear = NULL)), "at least one covariate")
  expect_error(run(base_args(nonlinear = "tidak_ada")), "`nonlinear` variable\\(s\\) not found")
  expect_error(run(base_args(formula = y ~ x1 + x3)), "both `formula` and `nonlinear`")
  d <- simulated_sae; d$x3 <- as.character(d$x3)
  expect_error(run(base_args(data = d)), "must be numeric")
})

test_that("spatial: panjang salah, tidak ada, bukan numerik", {
  expect_error(run(base_args(spatial = "lat")), "length 2")
  expect_error(run(base_args(spatial = c("lat", "lon", "x1"))), "length 2")
  expect_error(run(base_args(spatial = c("lat", "tidak_ada"))), "`spatial` variable\\(s\\) not found")
  d <- simulated_sae; d$lat <- as.character(d$lat)
  expect_error(run(base_args(data = d)), "must be numeric")
})

test_that("nilai hilang pada variabel model", {
  for (v in c("y", "x1", "x3", "lat", "vardir")) {
    d <- simulated_sae; d[[v]][1] <- NA
    expect_error(run(base_args(data = d)), "missing values", info = v)
  }
})

test_that("B dan proxmat_k tidak valid", {
  for (b in list(0, -1, 2.5, NA, c(1, 2)))
    expect_error(run(base_args(B = b)), "`B` must be a positive integer")
  expect_error(run(base_args(compare = TRUE, proxmat_k = 0)), "`proxmat_k` must be a positive integer")
})

test_that("proxmat tidak sesuai dimensi (compare = TRUE)", {
  expect_error(run(base_args(compare = TRUE, proxmat = diag(5))), "square matrix")
})
