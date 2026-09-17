# NYUS.2.3 feature generation using weather downloaded by UFEED

library(UFEED)
library(dplyr)
library(readr)


# 1. User settings -------------------------------------------------------------

longitude <- -76.50
latitude <- 43.06
cultivar <- "Riesling"

# The prediction period. UFEED also downloads an earlier calendar year so that
# rolling and dormant-season features have adequate weather history.
prediction_start_date <- as.Date("2025-09-01")
prediction_end_date <- as.Date("2026-04-30")

# "power" is easiest for most users. "power_ee" requires Google Earth Engine
# authentication, while "power_open_meteo" is another supported UFEED option.
weather_data_source <- "power"

cultivar_file <- "Cultivars_NYUS_2_3.Rdata"
model_feature_file <- "../Using model/NYUS_2_3_model_features.csv"
output_file <- "UFEED_temperature_features_NYUS_2_3.csv"


# 2. Download daily temperature with UFEED -------------------------------------

temperature_variables <- c("T2M", "T2M_MAX", "T2M_MIN")
current_year <- as.integer(format(Sys.Date(), "%Y"))
first_download_year <- as.integer(format(prediction_start_date, "%Y")) - 1L
last_download_year <- as.integer(format(prediction_end_date, "%Y"))

if (last_download_year > current_year) {
  stop("The requested prediction period extends beyond the current year.")
}

weather_parts <- list()
last_completed_year <- min(last_download_year, current_year - 1L)

if (first_download_year <= last_completed_year) {
  weather_parts$completed <- UFEED::UFEED_download_history_weather(
    lon = longitude,
    lat = latitude,
    start_year = first_download_year,
    end_year = last_completed_year,
    weather_data_source = weather_data_source,
    parameters = temperature_variables,
    pairwise = TRUE
  )
}

if (last_download_year == current_year) {
  weather_parts$current <- UFEED::UFEED_download_present_weather(
    lon = longitude,
    lat = latitude,
    weather_data_source = weather_data_source,
    parameters = temperature_variables,
    pairwise = TRUE,
    past_days = 9,
    forecast_days = 8
  )
}

daily_temperature <- bind_rows(weather_parts) %>%
  mutate(Date = as.Date(Date)) %>%
  filter(Date <= prediction_end_date) %>%
  arrange(Date) %>%
  distinct(Date, lon, lat, .keep_all = TRUE) %>%
  select(Date, lon, lat, all_of(temperature_variables))


# 3. Generate the temperature modules used to train NYUS.2.3 ------------------

temperature_features <- UFEED::UFEED_compute_weather_features(
  weather_data = daily_temperature,
  feature_profile = "history",
  included_module = c(
    "EWMA_REWMA_features",
    "cumulative_temp_features",
    "season_summary_features"
  ),
  ewma_rewma_cols = temperature_variables,
  season_max_cols = "T2M_MAX",
  season_min_cols = "T2M_MIN",
  message_progress = TRUE
)

feature_data <- UFEED::UFEED_wrap_up(
  weather_data = daily_temperature,
  weather_features = temperature_features,
  soil_features = NULL,
  start_filter_date = prediction_start_date,
  end_filter_date = prediction_end_date,
  clean_names = FALSE
)


# 4. Add the NYUS.2.3 cultivar one-hot columns ---------------------------------

load(cultivar_file)  # Loads the character vector named Cultivars.

cultivar_column <- paste0(
  "Cultivar.",
  gsub("[[:space:]]+", "_", trimws(cultivar))
)

if (!cultivar_column %in% Cultivars) {
  stop(
    "Unsupported cultivar. Choose one of: ",
    paste(gsub("_", " ", sub("^Cultivar\\.", "", Cultivars)), collapse = ", ")
  )
}

feature_data[Cultivars] <- 0L
feature_data[[cultivar_column]] <- 1L


# 5. Enforce the exact NYUS.2.3 model-input schema -----------------------------

model_features <- readr::read_csv(
  model_feature_file,
  show_col_types = FALSE
)$feature

missing_features <- setdiff(model_features, names(feature_data))
if (length(missing_features) > 0L) {
  stop("Missing model features: ", paste(missing_features, collapse = ", "))
}

feature_data <- feature_data %>%
  select(Date, all_of(model_features))

readr::write_csv(feature_data, output_file, na = "")

cat("Wrote", nrow(feature_data), "rows to", output_file, "\n")
