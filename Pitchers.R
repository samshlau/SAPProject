library(coursekata)
library(data.table)
library(baseballr)
library(dplyr)
library(purrr)
library(tidyr)
pitchDF23 = fg_pitcher_leaders(startseason = 2023, endseason = 2023, qual = 30)
pitchDF22 = fg_pitcher_leaders(startseason = 2022, endseason = 2022, qual = 30)
pitchDF21 = fg_pitcher_leaders(startseason = 2021, endseason = 2021, qual = 30)
pitchDF20 = fg_pitcher_leaders(startseason = 2020, endseason = 2020, qual = 30)
pitchDF19 = fg_pitcher_leaders(startseason = 2019, endseason = 2019, qual = 30)
pitchDF18 = fg_pitcher_leaders(startseason = 2018, endseason = 2018, qual = 30)
pitchDF24 = fg_pitcher_leaders(startseason = 2024, endseason = 2024, qual = 30)
pitchDF <- bind_rows(
  pitchDF24,pitchDF23, pitchDF22, pitchDF21, pitchDF20, pitchDF19, 
  pitchDF18
)


combine_statcast <- function(start_year = 2018, end_year = 2024) {
  do.call(rbind, lapply(start_year:end_year, function(y) {
    statcast_leaderboards(year = as.character(y), min_pitches = 447, player_type = 'pitcher')
  }))
}

combined_statcast <- combine_statcast() #Statcast for 2015-2023

colnames(combined_statcast)[colnames(combined_statcast) == "year"] <- "Season"
fix_name_order <- function(df, col = "last_name, first_name") {
  df[[col]] <- sapply(df[[col]], function(x) {
    parts <- unlist(strsplit(x, ", "))
    if (length(parts) == 2) paste(parts[2], parts[1]) else x
  })
  return(df)
}
combined_statcast <- fix_name_order(combined_statcast) #Fixed name order
colnames(combined_statcast)[colnames(combined_statcast) == "last_name, first_name"] <- "PlayerName"
pitchDF$Season <- as.integer(pitchDF$Season)
combined_statcast$Season <- as.integer(combined_statcast$Season)
#DO NOT TOUCH

combinedDF <- merge(pitchDF, combined_statcast, by = c("PlayerName", "Season"))


# Set seed so that we get the same random sample each time
set.seed(123)
# Get total number of rows (N) in dataset
N = nrow(combinedDF)
# Get size of train set so that it's 80% of dataset, and round to nearest whole number
train_size = round(.8 * N)
# Sample random row indeces from the full dataset, without replacement
train_rows <- sample.int(n = N, size = train_size, replace = F)
# Select the sampled row indeces for train set, remaining go to test set
train <- combinedDF[train_rows, ]
test <- combinedDF[-train_rows, ]


multi_model = lm(SIERA~poly(`K-BB_pct`,3) + `ERA-` + `GB_pct` + poly(`SwStr_pct`,3) , data = train) #RMSE = 7.404143
errors_multi <- test$SIERA - predict(multi_model, newdata = test)
rmse_multi <- sqrt(mean(errors_multi^2))
rmse_multi
summary(multi_model) # adj R^2 = .8742

newszn <- fg_pitcher_leaders(startseason = 2024, endseason = 2024, qual = 100)
predicted_SIERA = predict(multi_model, newdata = newszn)
newszn$Pred_SIERA <- predicted_SIERA
newszn$error <- newszn$SIERA - newszn$Pred_SIERA
arrange(newszn,desc(error))
mean(newszn$error, na.rm = TRUE) #.19772
mean(newszn$`Pred_SIERA`)

newDF <- newszn[, c("PlayerName", "SIERA")]

newDF


plot(multi_model)





