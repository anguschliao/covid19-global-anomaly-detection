library(tidyverse)
library(tidymodels)
library(tidyclust)

# Create country-level features ------------------------------------

# Latest date

latest_global_date <- max(
  covid_global_clean$date,
  na.rm = TRUE
)

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

# Prepare data for k-means

country_cluster_prepped <- country_cluster_recipe %>%
  prep()

country_cluster_processed <- country_cluster_prepped %>%
  bake(
    new_data = NULL
  )

# Remove ID column

country_cluster_data <- country_cluster_processed %>%
  select(-country)


# evaluate clusters performance

set.seed(100)

cluster_comparison <- tibble(
  clusters = 2:8,
  total_withinss = map_dbl(
    2:8,
    ~ kmeans(
      country_cluster_data,
      centers = .x,
      nstart = 25
    )$tot.withinss
  )
)

cluster_comparison

# plot the elbow

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
    breaks = 2:8
  ) +
  labs(
    title = "Elbow method for selecting the number of clusters",
    x = "Number of clusters",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal()

# based on elbow plot, 4 clusters selected

# Specify k-means model

country_kmeans_spec <- k_means(
  num_clusters = 4
) %>%
  set_engine(
    "stats",
    nstart = 25
  )


# create workflow

country_kmeans_workflow <- workflow() %>%
  add_recipe(
    country_cluster_recipe
  ) %>%
  add_model(
    country_kmeans_spec
  )

# fit model

set.seed(100)

country_kmeans_fit <- country_kmeans_workflow %>%
  fit(
    data = country_anomaly_features
  )

# Cluster assignment

country_cluster_results <- augment(
  country_kmeans_fit,
  new_data = country_anomaly_features
)

# Summarize Clusters

country_cluster_summary <- country_cluster_results %>%
  group_by(
    .pred_cluster
  ) %>%
  summarize(
    number_of_countries = n(),
    
    median_confirmed = median(
      confirmed,
      na.rm = TRUE
    ),
    
    median_deaths = median(
      deaths,
      na.rm = TRUE
    ),
    
    median_death_case_percent = median(
      death_case_percent,
      na.rm = TRUE
    ),
    
    median_max_daily_cases = median(
      max_daily_cases,
      na.rm = TRUE
    ),
    
    median_sd_daily_cases = median(
      sd_daily_cases,
      na.rm = TRUE
    ),
    
    median_negative_case_corrections = median(
      negative_case_corrections,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  )

country_cluster_summary

# Plot country clusters -----------------------------------------

ggplot(
  country_cluster_results,
  aes(
    x = confirmed,
    y = death_case_percent,
    color = .pred_cluster
  )
) +
  geom_point(
    size = 2,
    alpha = 0.8
  ) +
  scale_x_log10(
    labels = scales::label_number(
      scale_cut = scales::cut_short_scale()
    )
  ) +
  labs(
    title = "K-means clusters of global COVID-19 reporting patterns",
    subtitle = "Countries grouped using cumulative and daily reporting features",
    x = "Total confirmed cases (log scale)",
    y = "Deaths as a percentage of confirmed cases",
    color = "Cluster"
  ) +
  theme_minimal()
