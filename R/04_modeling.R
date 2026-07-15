library(tidyverse)
library(tidymodels)
library(tidyclust)

# Create country-level features ------------------------------------

# Latest cumulative totals by country

latest_country_totals <- covid_global_clean %>%
  filter(date == latest_global_date) %>%
  select(
    country,
    confirmed,
    deaths
  )

# Daily reporting features

country_daily_features <- covid_global_clean %>%
  group_by(country) %>%
  summarize(
    max_daily_cases = max(
      daily_cases,
      na.rm = TRUE
    ),
    sd_daily_cases = sd(
      daily_cases,
      na.rm = TRUE
    ),
    negative_case_corrections = sum(
      daily_cases < 0,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# Combine country-level features

country_anomaly_features <- latest_country_totals %>%
  inner_join(
    country_daily_features,
    by = "country"
  ) %>%
  filter(
    !is.na(confirmed),
    !is.na(deaths),
    confirmed >= 100000
  ) %>%
  mutate(
    death_case_ratio = deaths / confirmed,
    death_case_percent = death_case_ratio * 100
  )

# create recipe for country clusters

country_cluster_recipe <- recipe(
  ~ .,
  data = country_anomaly_features
) %>%
  update_role(
    country,
    new_role = "id"
  ) %>%
  step_log(
    confirmed,
    deaths,
    max_daily_cases,
    offset = 1
  ) %>%
  step_normalize(
    all_numeric_predictors()
  )

