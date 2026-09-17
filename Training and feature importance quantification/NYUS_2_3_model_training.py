#!/usr/bin/env python3

import pandas as pd
from autogluon.tabular import TabularPredictor


# 1. Read the NYUS 2.3 training data
train_data = pd.read_csv("All_training_data_NYUS_2_3.csv")


# 2. Remove columns that should not be used for model training
# Cultivar.* one-hot-encoded columns are retained.
train_data = train_data.drop(
    columns=["Date", "lon", "lat", "Cultivar"],
    errors="ignore",
)

# Remove any feature column containing only missing values.
train_data = train_data.dropna(axis=1, how="all")

print("Training data shape:", train_data.shape)
print(train_data["LT50"].describe())


# 3. Train the full AutoGluon model using all observations
predictor_LT50 = TabularPredictor(
    label="LT50",
    problem_type="regression",
    path="NYUS_2_3",
).fit(
    train_data,
    presets="best_quality",
    num_bag_folds=10,
    num_stack_levels=4,
    time_limit=7200,
)


# 4. Distill the full model into smaller deployment models
# Allow up to one additional hour for distillation after the two-hour fit.
distilled_models = predictor_LT50.distill(time_limit=3600)

with open("NYUS_2_3_distilled_models.txt", "w") as file:
    file.write("\n".join(distilled_models))

print("Distilled models:", distilled_models)


# 5. Save the model leaderboard
leaderboard = predictor_LT50.leaderboard(extra_info=True)
leaderboard.to_csv("NYUS_2_3_leaderboard.csv", index=False)

print(leaderboard)
print("Model saved in: NYUS_2_3")
