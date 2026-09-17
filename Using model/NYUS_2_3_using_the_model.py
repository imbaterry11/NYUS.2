#!/usr/bin/env python3

import pandas as pd
from autogluon.tabular import TabularPredictor


# 1. Load the trained NYUS.2.3 predictor
predictor = TabularPredictor.load("NYUS_2_3_light", require_version_match=False)


# 2. Select the feature file created by either supported R workflow
# UFEED-download approach:
feature_file = "UFEED_temperature_features_NYUS_2_3.csv"

# User-supplied Tmin/Tmax approach (uncomment this instead when applicable):
# feature_file = "daily_temperature_data_example_feature_extracted_NYUS_2_3.csv"

data = pd.read_csv(feature_file)

dates = data["Date"]
model_features = pd.read_csv("NYUS_2_3_model_features.csv")["feature"].tolist()

missing_features = sorted(set(model_features) - set(data.columns))
if missing_features:
    raise ValueError(f"Missing NYUS.2.3 model features: {missing_features}")


# 3. Predict with the best distilled model
predictions = predictor.predict(
    data[model_features],
    model="WeightedEnsemble_L2_DSTL",
)


# 4. Save dates and predicted LT50 values
output = pd.DataFrame(
    {
        "Date": dates,
        "LT50_predicted": predictions,
    }
)
output.to_csv("LT50_pred_NYUS_2_3.csv", index=False)

print(output.tail())
