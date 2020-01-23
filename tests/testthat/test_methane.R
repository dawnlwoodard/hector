context("Dynamic CH4 emissions")

test_that("Dynamic CH4 emissions increase CH4 concentration", {

  rcp45 <- system.file(
    "input", "hector_rcp45.ini",
    package = "hector"
  )
  hc <- newcore(rcp45)
  invisible(run(hc))
  outvars_ch4 <- c(RF_CH4(), ATMOSPHERIC_CH4())
  outvars_co2 <- c(ATMOSPHERIC_C(), ATMOSPHERIC_CO2())
  yrs <- 1750:2100
  result_ch4 <- fetchvars(hc, yrs, outvars_ch4, scenario = "default")
  result_co2 <- fetchvars(hc, yrs, outvars_co2, scenario = "default")
  setvar(hc, NA, RH_CH4_FRAC(), 0.001, NA)
  invisible(reset(hc))
  invisible(run(hc))
  result_ch4_2 <- fetchvars(hc, yrs, outvars_ch4, scenario = "methane")
  result_co2_2 <- fetchvars(hc, yrs, outvars_co2, scenario = "methane")
  expect_true(all(result_ch4_2[["value"]] >= result_ch4[["value"]]))
  # CO2 concentration should change
  expect_true(all(result_co2_2[["value"]] != result_co2[["value"]]))

})
