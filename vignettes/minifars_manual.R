## ------------------------------------------------------------------------
library(minifars)

## ---- eval=FALSE---------------------------------------------------------
#  library(minifars)
#  save_ <- getwd()
#  dir_data <- system.file("extdata", file="accident_2013.csv.bz2", package="minifars")
#  setwd(dirname(dir_data))

## ---- eval=FALSE---------------------------------------------------------
#  getwd(save_)

## ---- eval=FALSE---------------------------------------------------------
#  fars_summarize_years(years=2014)
#  fars_summarize_years(c(2014, 2015))

## ---- eval=FALSE---------------------------------------------------------
#  fars_map_state(state.num=24, year=2014)
#  fars_map_state(1, year=2015)

