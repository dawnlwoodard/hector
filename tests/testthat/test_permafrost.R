context("Permafrost submodel")

test_that("Permafrost submodel works", {

  rcp45 <- system.file(
    "input", "hector_rcp45.ini",
    package = "hector"
  )
  raw_ini <- trimws(readLines(rcp45))
  new_ini <- raw_ini
  new_ini[grep("^permafrost_c *=", new_ini)] <- "permafrost_c=1035"
  icsv <- grep("^ *.*?=csv:", new_ini)
  csv_paths_l <- regmatches(new_ini[icsv],
                            regexec(".*?=csv:(.*?\\.csv)", new_ini[icsv]))
  csv_paths <- vapply(csv_paths_l, `[[`, character(1), 2)
  csv_full_paths <- file.path(dirname(rcp45_file), csv_paths)
  new_ini_l <- Map(
    gsub,
    pattern = csv_paths,
    replacement = csv_full_paths,
    x = new_ini[icsv]
  )
  new_ini[icsv] <- unlist(new_ini_l, use.names = FALSE)
  ini_file <- tempfile()
  writeLines(new_ini, ini_file)
  hc <- newcore(ini_file, suppresslogging = FALSE)
  on.exit(file.remove(ini_file), add = TRUE)
  invisible(run(hc))
  yrs <- seq(1750, 2100)
  variables <- c(GLOBAL_TEMP(), ATMOSPHERIC_C())
  pf_results <- fetchvars(hc, yrs, variables, scenario = "permafrost")
  pf_soil <- fetchvars(hc, yrs, SOIL_C(), scenario = "permafrost")

  # Permafrost pool should always be shrinking, never growing
  pf_pf <- fetchvars(hc, yrs, PERMAFROST_C(),
                     scenario = "permafrost")[["value"]]
  expect_true(all(diff(pf_pf) <= 0))

  hc2 <- newcore(rcp45)
  invisible(run(hc2))
  orig_results <- fetchvars(hc2, yrs, variables, scenario = "default")
  orig_soil <- fetchvars(hc2, yrs, SOIL_C(), scenario = "default")
  orig_pf <- fetchvars(hc2, yrs, PERMAFROST_C(),
                       scenario = "default")[["value"]]

  # Original permafrost should always be zero
  expect_true(all(orig_pf == 0))

  # Accounting for permafrost should elevate CO2 and temperature
  expect_true(all(pf_results[["value"]] >= orig_results[["value"]]))

})
