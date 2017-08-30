context("test-fars_map_state")

test_that("checks if an error message is printed,
          when state.num parameter is invalid", {

  year <- 2014
  state.num <- 4000
  errMessage <- paste0("invalid STATE number: ", state.num)
  expect_error(fars_map_state(state.num, year), errMessage)
})
