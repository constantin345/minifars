context("test-fars_summarize_years")

test_that("Checks if the summary of number of accidents
           grouped by monthand year is correct computed", {
  years <- c(2013, 2014, 2015)
  df <- fars_summarize_years(years=years)

  filename <- "accident_summarize.rds"
  filename_path <- system.file("extdata", filename, package="minifars")
  df_sum <- readRDS(filename_path)
  expect_equal(df, df_sum)
})
