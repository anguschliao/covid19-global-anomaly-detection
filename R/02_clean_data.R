library(tidyverse)

# Clean US confirmed cases -------------------------------------

confirmed_us_clean <- confirmed_us_raw %>%
  pivot_longer(
    cols = -c(
      UID,
      iso2,
      iso3,
      code3,
      FIPS,
      Admin2,
      Province_State,
      Country_Region,
      Lat,
      Long_,
      Combined_Key
    ),
    names_to = "date",
    values_to = "confirmed"
  ) %>%
  mutate(
    date = mdy(date)
  ) %>%
  group_by(
    state = Province_State,
    country = Country_Region,
    date
  ) %>%
  summarize(
    confirmed = sum(confirmed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    country,
    state,
    date
  )


# Clean global confirmed cases --------------------------------------------

confirmed_global_clean <- confirmed_global_raw %>%
  pivot_longer(
    cols = -c(
      `Province/State`,
      `Country/Region`,
      Lat,
      Long
    ),
    names_to = "date",
    values_to = "confirmed"
  ) %>%
  mutate(
    date = mdy(date)
  ) %>%
  group_by(
    country = `Country/Region`,
    date
  ) %>%
  summarize(
    confirmed = sum(confirmed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    country,
    date
  )

# Clean United States deaths ----------------------------------------------

deaths_us_clean <- deaths_us_raw %>%
  pivot_longer(
    cols = -c(
      UID,
      iso2,
      iso3,
      code3,
      FIPS,
      Admin2,
      Province_State,
      Country_Region,
      Lat,
      Long_,
      Combined_Key,
      Population
    ),
    names_to = "date",
    values_to = "deaths"
  ) %>%
  mutate(
    date = mdy(date)
  ) %>%
  group_by(
    state = Province_State,
    country = Country_Region,
    date
  ) %>%
  summarize(
    deaths = sum(deaths, na.rm = TRUE),
    population = sum(Population, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    country,
    state,
    date
  )

# Clean global deaths ------------------------------------------------------

deaths_global_clean <- deaths_global_raw %>%
  pivot_longer(
    cols = -c(
      `Province/State`,
      `Country/Region`,
      Lat,
      Long
    ),
    names_to = "date",
    values_to = "deaths"
  ) %>%
  mutate(
    date = mdy(date)
  ) %>%
  group_by(
    country = `Country/Region`,
    date
  ) %>%
  summarize(
    deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(
    country,
    date
  )

# Check cleaned datasets ---------------------------------------------------

glimpse(confirmed_us_clean)
glimpse(confirmed_global_clean)
glimpse(deaths_us_clean)
glimpse(deaths_global_clean)

summary(confirmed_us_clean)
summary(confirmed_global_clean)
summary(deaths_us_clean)
summary(deaths_global_clean)

# Combine global confirmed cases and deaths -------------------------------

covid_global_clean <- confirmed_global_clean %>%
  full_join(
    deaths_global_clean,
    by = c("country", "date")
  ) %>%
  arrange(
    country,
    date
  )

# Combine United States confirmed cases and deaths ------------------------

covid_us_clean <- confirmed_us_clean %>%
  full_join(
    deaths_us_clean,
    by = c("state", "country", "date")
  ) %>%
  arrange(
    country,
    state,
    date
  )

# Check combined datasets --------------------------------------------------

glimpse(covid_global_clean)
glimpse(covid_us_clean)

# Calculate daily global values -------------------------------------------

covid_global_clean <- covid_global_clean %>%
  group_by(country) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    daily_cases = confirmed - lag(confirmed),
    daily_deaths = deaths - lag(deaths)
  ) %>%
  ungroup()

# Calculate daily United States values ------------------------------------

covid_us_clean <- covid_us_clean %>%
  group_by(country, state) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    daily_cases = confirmed - lag(confirmed),
    daily_deaths = deaths - lag(deaths)
  ) %>%
  ungroup()

# Validate final datasets --------------------------------------------------

covid_global_clean %>%
  summarize(
    number_of_countries = n_distinct(country),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    missing_confirmed = sum(is.na(confirmed)),
    missing_deaths = sum(is.na(deaths)),
    negative_daily_cases = sum(daily_cases < 0, na.rm = TRUE),
    negative_daily_deaths = sum(daily_deaths < 0, na.rm = TRUE)
  )

covid_us_clean %>%
  summarize(
    number_of_states = n_distinct(state),
    first_date = min(date, na.rm = TRUE),
    last_date = max(date, na.rm = TRUE),
    missing_confirmed = sum(is.na(confirmed)),
    missing_deaths = sum(is.na(deaths)),
    negative_daily_cases = sum(daily_cases < 0, na.rm = TRUE),
    negative_daily_deaths = sum(daily_deaths < 0, na.rm = TRUE)
  )
