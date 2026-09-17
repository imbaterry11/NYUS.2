# NYUS.2.3 feature generation from user-supplied daily Tmin/Tmax

library(UFEED)
library(dplyr)
library(readr)


# 1. User settings -------------------------------------------------------------

input_file <- "daily_temperature_data_example.csv"
output_file <- "daily_temperature_data_example_feature_extracted_NYUS_2_3.csv"
cultivar_file <- "Cultivars_NYUS_2_3.Rdata"
model_feature_file <- "../Using model/NYUS_2_3_model_features.csv"

longitude <- -76.50
latitude <- 43.06
cultivar <- "Riesling"

# The repository example is in degrees Fahrenheit. Use "C" for Celsius input.
input_temperature_unit <- "F"


# 2. Read daily minimum and maximum temperature --------------------------------

daily_raw <- readr::read_csv(input_file, show_col_types = FALSE)
names(daily_raw) <- tolower(sub("^\\ufeff", "", names(daily_raw)))

required_input <- c("date", "tmax", "tmin")
if (!all(required_input %in% names(daily_raw))) {
  stop("Input must contain date, tmax, and tmin columns.")
}

daily_temperature <- daily_raw %>%
  transmute(
    Date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%m/%d/%y")),
    T2M_MAX = as.numeric(tmax),
    T2M_MIN = as.numeric(tmin)
  )

if (toupper(input_temperature_unit) == "F") {
  daily_temperature <- daily_temperature %>%
    mutate(
      T2M_MAX = (T2M_MAX - 32) * 5 / 9,
      T2M_MIN = (T2M_MIN - 32) * 5 / 9
    )
} else if (toupper(input_temperature_unit) != "C") {
  stop("input_temperature_unit must be 'F' or 'C'.")
}

daily_temperature <- daily_temperature %>%
  mutate(
    T2M = (T2M_MAX + T2M_MIN) / 2,
    lon = longitude,
    lat = latitude
  ) %>%
  select(Date, lon, lat, T2M, T2M_MAX, T2M_MIN) %>%
  arrange(Date)

if (any(!complete.cases(daily_temperature))) {
  stop("Input contains an invalid or missing date/temperature value.")
}


# 3. Generate the same temperature modules used to train NYUS.2.3 -------------

temperature_variables <- c("T2M", "T2M_MAX", "T2M_MIN")

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
