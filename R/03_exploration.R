# Global data exploration --------------------------------------

# Largest daily increases

covid_global_clean %>%
  select(country, date, daily_cases) %>%
  arrange(desc(daily_cases)) %>%
  slice_head(n = 10)

# Largest corrections

covid_global_clean %>%
  filter(daily_cases < 0) %>%
  select(country, date, daily_cases) %>%
  arrange(daily_cases) %>%
  slice_head(n = 10)

# Countries with highest spikes

largest_outbreaks <- covid_global_clean %>%
  group_by(country) %>%
  summarize(
    max_daily_cases = max(daily_cases, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(max_daily_cases)) %>%
  slice_head(n = 4)

largest_outbreaks

selected_countries <- largest_outbreaks %>%
  pull(country)

# Plot daily cases for top 4 countries showing largest daily spikes

global_plot_data <- covid_global_clean %>%
  filter(country %in% selected_countries)

ggplot(
  global_plot_data,
  aes(
    x = date,
    y = daily_cases
  )
) +
  geom_line() +
  facet_wrap(
    vars(country),
    scales = "free_y"
  ) +
  labs(
    title = "Daily COVID-19 cases",
    subtitle = "Countries with the highest observed daily case counts",
    x = "Date",
    y = "Daily cases"
  ) +
  theme_minimal()

# total deaths case ratio by country

latest_global_date <- max(covid_global_clean$date, na.rm = TRUE)

country_death_case_ratio <- covid_global_clean %>%
  filter(date == latest_global_date) %>%
  filter(
    !is.na(confirmed),
    !is.na(deaths),
    confirmed > 0
  ) %>%
  mutate(
    death_case_ratio = deaths / confirmed,
    death_case_percent = death_case_ratio * 100
  )

minimum_confirmed_cases <- 100000

country_death_case_ratio <- country_death_case_ratio %>%
  filter(confirmed >= minimum_confirmed_cases)

country_death_case_ratio %>%
  select(
    country,
    confirmed,
    deaths,
    death_case_percent
  ) %>%
  arrange(desc(death_case_percent)) %>%
  slice_head(n = 10)

# Use IQR to identify countries with anomalous ratios

ratio_q1 <- quantile(
  country_death_case_ratio$death_case_percent,
  0.25,
  na.rm = TRUE
)

ratio_q3 <- quantile(
  country_death_case_ratio$death_case_percent,
  0.75,
  na.rm = TRUE
)

ratio_iqr <- ratio_q3 - ratio_q1

upper_ratio_limit <- ratio_q3 + 1.5 * ratio_iqr

death_case_ratio_anomalies <- country_death_case_ratio %>%
  filter(death_case_percent > upper_ratio_limit) %>%
  select(
    country,
    confirmed,
    deaths,
    death_case_percent
  ) %>%
  arrange(desc(death_case_percent))

death_case_ratio_anomalies

# create scatter plot of death case ratios with anomalous countries highlighted

ggplot(
  country_death_case_ratio,
  aes(
    x = confirmed,
    y = death_case_percent
  )
) +
  geom_point() +
  geom_point(
    data = death_case_ratio_anomalies,
    size = 2.5
  ) +
  geom_text(
    data = death_case_ratio_anomalies,
    aes(label = country),
    nudge_y = 0.15,
    check_overlap = TRUE
  ) +
  geom_hline(
    yintercept = upper_ratio_limit,
    linetype = "dashed"
  ) +
  scale_x_log10(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  labs(
    title = "Potential anomalies in reported COVID-19 death-to-case ratios",
    subtitle = "Countries above the 1.5 × IQR upper threshold are labelled",
    x = "Total confirmed cases (log scale)",
    y = "Deaths as a percentage of confirmed cases"
  ) +
  theme_minimal()

# US data exploration --------------------------------------

# US states and population

state_populations <- covid_us_clean %>%
  distinct(state, population) %>%
  arrange(desc(population))

state_populations

# States with largest daily spikes per 100,000

covid_us_explore <- covid_us_clean %>%
  filter(population >= 500000) %>%
  mutate(
    daily_cases_per_100k = (daily_cases / population) * 100000
  )

largest_state_outbreaks <- covid_us_explore %>%
  group_by(state) %>%
  summarize(
    max_daily_cases_per_100k = max(
      daily_cases_per_100k,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(max_daily_cases_per_100k)) %>%
  slice_head(n = 4)

largest_state_outbreaks

selected_states <- largest_state_outbreaks %>%
  pull(state)

us_plot_data <- covid_us_explore %>%
  filter(state %in% selected_states)

# Plot States with largest daily spikes per 100,000

us_plot_data <- covid_us_explore %>%
  filter(state %in% selected_states)

ggplot(
  us_plot_data,
  aes(
    x = date,
    y = daily_cases
  )
) +
  geom_line() +
  facet_wrap(
    vars(state),
    scales = "free_y"
  ) +
  labs(
    title = "Daily COVID-19 Cases",
    subtitle = "States selected by highest daily cases per 100,000 population",
    x = "Date",
    y = "Daily Cases"
  ) +
  theme_minimal()

# dates of the largest spikes

largest_state_spikes <- covid_us_explore %>%
  filter(state %in% selected_states) %>%
  group_by(state) %>%
  slice_max(
    order_by = daily_cases_per_100k,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup() %>%
  select(
    state,
    date,
    daily_cases,
    daily_cases_per_100k
  )

largest_state_spikes
