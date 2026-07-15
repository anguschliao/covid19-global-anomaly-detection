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
    mean_daily_cases = mean(
      daily_cases,
      na.rm = TRUE
    ),
    sd_daily_cases = sd(
      daily_cases,
      na.rm = TRUE
    ),
    max_daily_deaths = max(
      daily_deaths,
      na.rm = TRUE
    ),
    negative_case_corrections = sum(
      daily_cases < 0,
      na.rm = TRUE
    ),
    negative_death_corrections = sum(
      daily_deaths < 0,
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

# log transform skewed variables

country_anomaly_features <- country_anomaly_features %>%
  mutate(
    log_confirmed = log1p(confirmed),
    log_deaths = log1p(deaths),
    log_max_daily_cases = log1p(
      pmax(max_daily_cases, 0)
    ),
    log_mean_daily_cases = log1p(
      pmax(mean_daily_cases, 0)
    ),
    log_sd_daily_cases = log1p(
      pmax(sd_daily_cases, 0)
    ),
    log_max_daily_deaths = log1p(
      pmax(max_daily_deaths, 0)
    )
  )

# feature selection

country_model_data <- country_anomaly_features %>%
  select(
    log_confirmed,
    log_deaths,
    death_case_percent,
    log_max_daily_cases,
    log_sd_daily_cases,
    negative_case_corrections
  )

# check for invalid

country_model_data %>%
  summarize(
    across(
      everything(),
      ~ sum(is.na(.x))
    )
  )

# check for infinites

country_model_data %>%
  summarize(
    across(
      everything(),
      ~ sum(!is.finite(.x))
    )
  )

# scale

country_model_scaled <- scale(country_model_data)

# choose k clusters

set.seed(100)

cluster_comparison <- tibble(
  clusters = 2:6,
  total_withinss = map_dbl(
    clusters,
    ~ kmeans(
      country_model_scaled,
      centers = .x,
      nstart = 25
    )$tot.withinss
  )
)

cluster_comparison

# plot cluster results

ggplot(
  cluster_comparison,
  aes(
    x = clusters,
    y = total_withinss
  )
) +
  geom_line() +
  geom_point() +
  scale_x_continuous(
    breaks = 2:6
  ) +
  labs(
    title = "Choosing the number of country clusters",
    subtitle = "Lower within-cluster variation indicates tighter clusters",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal()


