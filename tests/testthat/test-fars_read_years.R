context("test-fars_read_years")

#test_that("checks if the list with dataframes is correct created", {
#  years <- c(2013, 2014)
#  list_ <- fars_read_years(years=years)

#  df_2013 <- readRDS("accident_2013.csv.rds")

#  df_test2013 <- data.frame(MONTH=df_2013$MONTH, year=2013)

#  df_2014 <- readRDS("accident_2014.csv.rds")

#  df_test2014<-data.frame(MONTH=df_2014$MONTH, year=2014)

#  expect_equal(list_[[1]], df_test2013)
#  expect_equal(list_[[2]], df_test2014)

#})

test_that("checks if a warning message is print,
           when a year doesn't have a file associated on disk", {
  years <- c(2013, 2018)
  expect_warning(fars_read_years(years=years), "invalid year: 2018")
})
