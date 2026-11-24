# Data helpers for the outcomes dashboard. Synthetic data; no real people or institutions.
suppressPackageStartupMessages({library(dplyr)})
MIN_CELL <- 10   # groups with fewer respondents are suppressed, as is usual for published statistics

load_outcomes <- function(path = "data/clean_survey.csv") {
  d <- read.csv(path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  d$emp <- ifelse(is.na(d$employed_6mo), NA, as.numeric(d$employed_6mo == "Yes"))
  d
}

filter_outcomes <- function(d, institutions = NULL, programs = NULL, years = NULL) {
  if (length(institutions)) d <- d[d$institution_id %in% institutions, ]
  if (length(programs))     d <- d[d$program_area %in% programs, ]
  if (length(years))        d <- d[d$grad_year %in% years, ]
  d
}

suppress_small <- function(tab, n_col = "n", value_cols, min_n = MIN_CELL) {
  small <- tab[[n_col]] < min_n
  for (v in value_cols) tab[[v]][small] <- NA
  tab$suppressed <- small
  tab
}

kpis <- function(d) {
  e <- d[!is.na(d$emp), ]
  list(respondents = nrow(d),
       employment_rate = if (nrow(e) >= MIN_CELL) weighted.mean(e$emp, e$weight) else NA_real_,
       median_wage = if (sum(!is.na(d$hourly_wage)) >= MIN_CELL) median(d$hourly_wage, na.rm = TRUE) else NA_real_,
       mean_quality = if (nrow(d) >= MIN_CELL) mean(d$q_quality, na.rm = TRUE) else NA_real_)
}

summarise_by <- function(d, by = "program_area") {
  d |> group_by(across(all_of(by))) |>
    summarise(n = n(),
              employment_rate = ifelse(sum(!is.na(emp)) > 0, weighted.mean(emp, weight, na.rm = TRUE), NA_real_),
              median_wage = ifelse(sum(!is.na(hourly_wage)) > 0, median(hourly_wage, na.rm = TRUE), NA_real_),
              mean_quality = mean(q_quality, na.rm = TRUE), .groups = "drop") |>
    as.data.frame() |> suppress_small("n", c("employment_rate", "median_wage", "mean_quality"))
}
