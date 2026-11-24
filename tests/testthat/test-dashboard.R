library(testthat); library(shiny)
source("../../R/data_functions.R")
d <- load_outcomes("../../data/clean_survey.csv")

test_that("filters work and an empty selection means everything", {
  expect_equal(nrow(filter_outcomes(d)), nrow(d))
  f <- filter_outcomes(d, institutions = "PTI01", programs = "Trades")
  expect_true(all(f$institution_id == "PTI01" & f$program_area == "Trades"))
  expect_equal(nrow(filter_outcomes(d, years = 2024)), sum(d$grad_year == 2024))
})
test_that("small groups are suppressed and large ones are not", {
  tab <- data.frame(g = c("a", "b"), n = c(9, 10), rate = c(.5, .6))
  s <- suppress_small(tab, "n", "rate")
  expect_true(is.na(s$rate[1])); expect_equal(s$rate[2], .6)
  expect_equal(s$suppressed, c(TRUE, FALSE))
})
test_that("a filtered slice below the minimum cell size shows no rate", {
  tiny <- head(d, 6)
  k <- kpis(tiny)
  expect_equal(k$respondents, 6); expect_true(is.na(k$employment_rate)); expect_true(is.na(k$median_wage))
})
test_that("summary agrees with a direct calculation", {
  s <- summarise_by(d, "program_area")
  tr <- d[d$program_area == "Trades" & !is.na(d$emp), ]
  expect_equal(s$employment_rate[s$program_area == "Trades"], weighted.mean(tr$emp, tr$weight))
  expect_equal(sum(s$n), nrow(d))
})
test_that("server reacts to the program filter", {
  old <- setwd("../.."); on.exit(setwd(old))
  app_env <- new.env(); sys.source("app.R", envir = app_env)
  setwd(old)
  testServer(app_env$server, {
    session$setInputs(inst = NULL, prog = NULL, yr = c("2023", "2024", "2025"))
    all_n <- sum(tab()$n)
    session$setInputs(prog = "Trades")
    expect_equal(nrow(tab()), 1)
    expect_lt(sum(tab()$n), all_n)
    expect_equal(output$k_n, format(sum(d$program_area == "Trades"), big.mark = ","))
  })
})
