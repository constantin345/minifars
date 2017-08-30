#' Reads a file from disk in a dataframe.
#'
#' @param filename A character string giving the name of the file
#' @return If there is a file on disk then the output will be a dataframe.
#' If the file doesn't exist  stops execution and print an error message.
#'
#' @examples
#'
#' fars_read("accident_2013.csv.bz2")
#' fars_read("accident_2014.csv.bz2")
#' \dontrun{
#' fars_read("accident_2.csv.bz2")
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#' @export
fars_read <- function(filename) {
        filename_path <- system.file("extdata", filename, package="minifars")
        if(!file.exists(filename_path))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                read_csv(filename_path, progress = FALSE)
        })
        tbl_df(data)
}

#' Using year as input creates a string consistent with a files names from
#' FARS dataset.
#'
#' fars_data.zip (FARS dataset) represents the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' This data are from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System.
#'
#' @param year A integer number giving the year.
#' @return a string consistent with a files names from FARS dataset.
#'
#' @examples
#' make_filename(2013)
#' make_filename(2014)
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        file <- sprintf("accident_%d.csv.bz2", year)
}

#' From a vector of years creates a list of dataframes with information from
#' FARS dataset.
#'
#' fars_data.zip (FARS dataset) represents the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' This data are from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System.
#'
#' @param years A vector of integers, each element of the vector representing
#' one year.
#'
#' @return a list with dataframes, each dataframe has just the columns MONTH and year.
#' In the case that the function don't find information about a year
#' (from vector \code{years}) in FARS a warning message will be returned.
#'
#'
#' @examples
#' fars_read_years(c(2013, 2014, 2))
#' fars_read_years(c(2013, 2014))
#'
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @export
fars_read_years <- function(years) {
        MONTH <- NULL

        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        mutate(dat, year = year) %>%
                        select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' From a vector of years creates a summary of number of accidents grouped by month
#' and year. Those information are from FARS dataset.
#'
#' fars_data.zip (FARS dataset) represents the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' This data are from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System.
#'
#' @param years A vector of integers - each value represents a year
#' @return a dataframe in wide format contains a summary of number of accidents on
#' each combination year and month. In the case that the function don't find
#' information about a year (from vector \code{years}) in FARS, a warning message it
#' will be returned before to return dataframe.
#'
#' @examples
#' fars_summarize_years(c(2013, 2014, 2))
#' fars_summarize_years(c(2013, 2014))
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @export
fars_summarize_years <- function(years) {
        year <- NULL
        MONTH <- NULL
        n <- NULL

        dat_list <- fars_read_years(years)
        bind_rows(dat_list) %>%
        group_by(year, MONTH) %>%
        summarize(n = n()) %>%
        spread(year, n)
}

#' Using data from FARS  it creates a map of an USA state which contains positions
#' (longitude and latitude) of accidents from a year
#'
#' fars_data.zip (FARS dataset) represents the American public yearly data regarding
#' fatal injuries suffered in motor vehicle traffic crashes.
#' This data are from the US National Highway Traffic Safety Administration's
#' Fatality Analysis Reporting System.
#'
#' @param state.num An integer number giving the id of the state
#' @param year An integer number giving the year
#'
#' @return
#' Draw a map (of \code{state.num}) with the positions
#' (longitude and latitude) of accidents from \code{year}.
#'
#' If the \code{state.num} is invalid stop execution and print a message.
#'
#' If there aren't accidents in \code{year} a message will be print.
#'
#'
#' @examples
#' fars_map_state(24, 2014)
#' fars_map_state(1, 2015)
#' \dontrun{
#' fars_map_state(7,2013)
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#' @export

fars_map_state <- function(state.num, year) {
        STATE <- NULL

        filename <- make_filename(year)

        data <- fars_read(filename)

        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
          points(LONGITUD, LATITUDE, pch = 46)
        })
}
