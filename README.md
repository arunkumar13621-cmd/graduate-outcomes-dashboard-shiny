# Graduate Outcomes Explorer (R Shiny)

An interactive dashboard on the cleaned synthetic survey: filters for institution, program area and graduation year; headline figures (respondents, weighted employment rate, median wage, mean rating); an employment chart, a wage chart and a summary table; CSV download.

**Run:** `Rscript -e "shiny::runApp('.')"` **Test:** `Rscript -e "testthat::test_dir('tests/testthat')"` (5 tests, 14 checks). `screenshot.png` is a capture of the running app.

## Design choices
* **Small-cell suppression:** any group with fewer than 10 respondents shows "suppressed" instead of a rate, as is usual for published statistics. The data functions are separate from the app, so they are tested without a browser.
* **Weights:** employment rates use the institution weights from the pipeline.
* Tests cover the filters, suppression, a check of the summary against a direct calculation, and the app's server logic with `testServer`.
* The running app was also checked in a browser: with the Trades filter it shows 603 respondents and 69.5% employed, which matches the pipeline.

## Limits
No login, no deployment settings and no accessibility audit. Rates for small groups that pass the minimum size (for example 13 respondents) can still be unstable. Only the file path loads data, so refreshing the data means replacing the CSV.
