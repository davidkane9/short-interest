# This script downloads short interest data from FINRA. 

# https://www.finra.org/finra-data/browse-catalog/equity-short-interest/files

# Note that "Prior to June 2021, the data contains short interest positions in
# over-the-counter securities only and does not reflect short interest data in
# exchange-listed securities."

# Files seems to have a format like this:
# https://cdn.finra.org/equity/otcmarket/biweekly/shrt20190415.csv

# Difficulty is that the dates of the files are the 15th or the last day of the
# month, unless that day is a week-end or a holiday. (Or a Presidential
# funeral!) So, we just try lots of possible dates, only succeeding when the
# file is there.

# Useful functions, courtesy of ChatGPT

library(tidyverse)
library(httr2)

# Function to generate specific dates and return as a vector of Date objects

generate_dates <- function(start_date, end_date) {
  seq(from = as.Date(start_date), to = as.Date(end_date), by = "month") %>%
    map(~ {
      month_start <- floor_date(.x, "month")
      last_day <- ceiling_date(.x, "month") - days(1)
      
      specific_days <- c(12, 13, 14, 15,
                         as.integer(format(last_day, "%d")) - 2,
                         as.integer(format(last_day, "%d")) - 1,
                         as.integer(format(last_day, "%d")))
      
      # Generate dates for the specific days, ensuring the output is a Date vector
      specific_dates <- specific_days %>% map(~ month_start + days(.x - 1)) %>% unlist()
      
      as.Date(specific_dates)
    }) %>%
    unlist() %>%
    # Ensure the result is a Date vector
    as.Date()
}


generate_urls <- function(dates_vector) {
  base_url <- "https://cdn.finra.org/equity/otcmarket/biweekly/shrt"
  urls <- map_chr(dates_vector, ~ paste0(base_url, format(.x, "%Y%m%d"), ".csv"))
  return(urls)
}


read_csv_safely <- function(url) {
  tryCatch({
    # Attempt to download and read the CSV
    temp <- tempfile()
    download.file(url, temp, quiet = TRUE)
    df <- read_delim(url,
                     delim = "|",
                     skip = 1,
                     col_types = cols(
                       date = col_date(format = "%Y%m%d"),
                       symbol = col_character(),
                       name = col_character(),
                       issuer = col_character(),
                       market = col_character(),
                       short = col_double(),
                       short_1 = col_double(),
                       split = col_character(),
                       dvol = col_double(),
                       days_to_cover = col_double(),
                       revision = col_character(),
                       perc_change = col_double(),
                       numb_change = col_double(),
                       settle_date = col_date(format = "%Y-%m-%d")
                     ),
                     col_names = c("date", "symbol", "name", "issuer", "market",
                                   "short", "short_1", "split", "dvol",
                                   "days_to_cover", "revision", "perc_change",
                                   "numb_change", "settle_date"),
                     id = "file")
      
    unlink(temp) # Remove the temporary file
    return(df)
  }, error = function(e) {
    message("Failed to download or read: ", url)
    return(NULL) # Return NULL on failure
  })
}

# Although the web page suggests that the files go back to 2016, and though
# there are some links you can click on, the files actually start to exist only
# in December 2017.

dates_vector <- generate_dates("2017-12-01", "2024-02-28")

urls <- generate_urls(dates_vector)


# Attempt to download and combine all existing files into a single tibble. How
# thing takes a couple of minutes.

x <- map(urls, read_csv_safely) %>% compact() %>% bind_rows()

# x consists of 146 separate files, with a total of 2.6 million rows.

# write_rds(x, file = "data/finra_data.rds")

