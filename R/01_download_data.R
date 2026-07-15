library(tidyverse)

url_in <- paste0(
  "https://raw.githubusercontent.com/",
  "CSSEGISandData/COVID-19/master/",
  "csse_covid_19_data/csse_covid_19_time_series/"
)

file_names <- c(
  "time_series_covid19_confirmed_US.csv",
  "time_series_covid19_confirmed_global.csv",
  "time_series_covid19_deaths_US.csv",
  "time_series_covid19_deaths_global.csv"
)

urls <- str_c(url_in, file_names)

confirmed_us_url     <- urls[1]
confirmed_global_url <- urls[2]
deaths_us_url        <- urls[3]
deaths_global_url    <- urls[4]

confirmed_us_raw <- read_csv(
  confirmed_us_url,
  show_col_types = FALSE
)

confirmed_global_raw <- read_csv(
  confirmed_global_url,
  show_col_types = FALSE
)

deaths_us_raw <- read_csv(
  deaths_us_url,
  show_col_types = FALSE
)

deaths_global_raw <- read_csv(
  deaths_global_url,
  show_col_types = FALSE
)

