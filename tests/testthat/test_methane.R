context("Dynamic CH4 emissions")

test_that("Dynamic CH4 emissions increase CH4 concentration", {

  rcp45 <- system.file(
    "input", "hector_rcp45.ini",
    package = "hector"
  )
  hc <- newcore(rcp45)
  invisible(run(hc))
  outvars <- c(RF_CH4(), ATMOSPHERIC_CH4())
  yrs <- 1750:2100
  result <- fetchvars(hc, yrs, outvars, scenario = "default")
  setvar(hc, NA, RH_CH4_FRAC(), 0.001, NA)
  invisible(reset(hc))
  invisible(run(hc))
  result2 <- fetchvars(hc, yrs, outvars, scenario = "methane")
  expect_true(all(result2[["value"]] >= result[["value"]]))

})
