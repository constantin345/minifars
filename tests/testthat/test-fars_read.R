context("test-fars_read")

test_that("checks if the file is read correct", {
  df <- fars_read("accident_2013_test.csv.bz2")
  expect_is(df, "tbl_df")

  filename <- "accident_2013_test.csv.rds"
  path_file <- system.file("extdata", filename, package="minifars")
  df_test <- readRDS(path_file)
  expect_equal(df, df_test)
})

test_that("checks if an error messege is printed,
           when the file doesn't exist on disk", {
  filename <- "accident_2010.csv.bz2"
  errMessage <- paste0("file '", filename, "' does not exist")
  expect_error(fars_read(filename), errMessage)
})
