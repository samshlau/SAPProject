library(baseballr)
library(dplyr)
library(tidyr)

# Function to process data for a given year
process_year = function(year) {
  # Fetch data
  hitterDF = fg_bat_leaders(startseason = year, endseason = year, qual = 100)
  statcast = statcast_leaderboards(year = year, min_pa = 100)
  
  # Clean and merge data
  statcast = statcast %>%
    rename(
      PlayerName = `last_name, first_name`,
      brl_pa = brl_pa
    ) %>%
    separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
    mutate(PlayerName = paste(First, Last)) %>%
    select(-Last, -First)
  
  combinedDF = merge(hitterDF, statcast, by = "PlayerName")
  
  # Select relevant columns for modeling
  modelDF = combinedDF %>%
    select(
      wRC_plus, brl_pa,
      avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
      Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`,
      `F-Strike_pct`, `Cent_pct+` 
    )
  
  # Build full and reduced models
  full_model = lm(wRC_plus ~.^2, data = modelDF)
  reduced_model = step(full_model)
  
  # Predict wRC+
  combinedDF$Predicted_wRC_plus = predict(reduced_model, newdata = modelDF)
  
  # Create analyzeDF
  analyzeDF = data.frame(
    Name = combinedDF$PlayerName,
    Year = year,
    wRC_plus = combinedDF$wRC_plus,
    Predicted_wRC_plus = combinedDF$Predicted_wRC_plus,
    Delta = combinedDF$Predicted_wRC_plus - combinedDF$wRC_plus
  )
  
  return(analyzeDF)
}

# List of years to process
years = c(2018, 2019, 2021, 2022, 2023, 2024)

# Process each year and store results in a list
analyzeDF_list = lapply(years, process_year)

# Access analyzeDF for each year
analyzeDF_2018 = analyzeDF_list[[1]]
analyzeDF_2019 = analyzeDF_list[[2]]
analyzeDF_2021 = analyzeDF_list[[3]]
analyzeDF_2022 = analyzeDF_list[[4]]
analyzeDF_2023 = analyzeDF_list[[5]]
analyzeDF_2024 = analyzeDF_list[[6]]


# Example: Compare 2019 predicted vs actual
# Calculate the difference between Predicted Delta and Actual Delta for each year
analyzeDF_2019 = analyzeDF_2019 %>%
  mutate(
    Delta_2019_2018 = wRC_plus - analyzeDF_2018$wRC_plus[match(Name, analyzeDF_2018$Name)],
    Predicted_Delta_2019_2018 = Predicted_wRC_plus - analyzeDF_2018$wRC_plus[match(Name, analyzeDF_2018$Name)],
    Delta_Difference_2019_2018 = Predicted_Delta_2019_2018 - Delta_2019_2018
  )

analyzeDF_2021 = analyzeDF_2021 %>%
  mutate(
    Delta_2021_2019 = wRC_plus - analyzeDF_2019$wRC_plus[match(Name, analyzeDF_2019$Name)],
    Predicted_Delta_2021_2019 = Predicted_wRC_plus - analyzeDF_2019$wRC_plus[match(Name, analyzeDF_2019$Name)],
    Delta_Difference_2021_2019 = Predicted_Delta_2021_2019 - Delta_2021_2019
  )

analyzeDF_2022 = analyzeDF_2022 %>%
  mutate(
    Delta_2022_2021 = wRC_plus - analyzeDF_2021$wRC_plus[match(Name, analyzeDF_2021$Name)],
    Predicted_Delta_2022_2021 = Predicted_wRC_plus - analyzeDF_2021$wRC_plus[match(Name, analyzeDF_2021$Name)],
    Delta_Difference_2022_2021 = Predicted_Delta_2022_2021 - Delta_2022_2021
  )

analyzeDF_2023 = analyzeDF_2023 %>%
  mutate(
    Delta_2023_2022 = wRC_plus - analyzeDF_2022$wRC_plus[match(Name, analyzeDF_2022$Name)],
    Predicted_Delta_2023_2022 = Predicted_wRC_plus - analyzeDF_2022$wRC_plus[match(Name, analyzeDF_2022$Name)],
    Delta_Difference_2023_2022 = Predicted_Delta_2023_2022 - Delta_2023_2022
  )

# For 2024, only calculate the predicted wRC+ (no deltas or differences)
analyzeDF_2024 = analyzeDF_2024 %>%
  mutate(
    Delta_2024_2023 = NA,  # No data for 2025, so set to NA
    Predicted_Delta_2024_2023 = NA,
    Delta_Difference_2024_2023 = NA
  )




# Function to assign scores based on Delta_Difference
assign_score = function(delta_difference) {
  if (is.na(delta_difference)) {
    return(NA)  # Skip NA values
  } else if (abs(delta_difference) <= 5) {
    return(1) # Amazing
  } else if (abs(delta_difference) <= 10) {
    return(0.75) #good
  } else if (abs(delta_difference) <= 15) {
    return(0.5) #mid
  } else {
    return(0) #dogwater
  }
}

# Apply the scoring function to each year's analyzeDF
analyzeDF_2019 = analyzeDF_2019 %>%
  mutate(Accuracy_Score = sapply(Delta_Difference_2019_2018, assign_score))

analyzeDF_2021 = analyzeDF_2021 %>%
  mutate(Accuracy_Score = sapply(Delta_Difference_2021_2019, assign_score))

analyzeDF_2022 = analyzeDF_2022 %>%
  mutate(Accuracy_Score = sapply(Delta_Difference_2022_2021, assign_score))

analyzeDF_2023 = analyzeDF_2023 %>%
  mutate(Accuracy_Score = sapply(Delta_Difference_2023_2022, assign_score))

# For 2024, skip since we don't have Delta_Difference
analyzeDF_2024 = analyzeDF_2024 %>%
  mutate(Accuracy_Score = NA)

# Calculate overall accuracy for each year
accuracy_2019 = mean(analyzeDF_2019$Accuracy_Score, na.rm = TRUE)
accuracy_2021 = mean(analyzeDF_2021$Accuracy_Score, na.rm = TRUE)
accuracy_2022 = mean(analyzeDF_2022$Accuracy_Score, na.rm = TRUE)
accuracy_2023 = mean(analyzeDF_2023$Accuracy_Score, na.rm = TRUE)

# Print accuracy scores
accuracy_2019 #= .733
accuracy_2021 #= #.7409
accuracy_2022 #= #.657
accuracy_2023 #= #.704
  
term = accuracy_2019 + accuracy_2021 + accuracy_2022 + accuracy_2023
term = term / 4
term

#.708 which means its accurate but on the cusp of very accurate. 
