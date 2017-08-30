context("test-make_filename")

test_that("string consistent with a file name from FARS daset", {
  expect_equal(make_filename(2013), "accident_2013.csv.bz2")
})
