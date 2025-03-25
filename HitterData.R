#SAP Projects
#Team sorts by name order in leage 16+ is NL

allDF = fg_team_batter(startseason = "2023", endseason = "2015", sortstat = "WAR")

team_babip = data.frame(Team = allDF$team_name, BABIP = allDF$BABIP)

plot(team_babip$Team, team_babip$BABIP)



ggplot(team_babip, aes(x = reorder(Team, BABIP), y = BABIP)) +
  geom_col(fill = "steelblue") +
  coord_flip() +  # Flip for better readability
  labs(title = "BABIP by MLB Team", x = "Team", y = "BABIP") +
  theme_minimal()

mean(team_babip$BABIP)



#This above was practice


#Lets use the baseballr statcast data we have from 2015 to 2023 and see how that compares to the 2024 players
#We might need to do some manual assignment to what classifies as a good season. Maybe do a WAR to performacne type thing

hitterDF = fg_bat_leaders(startseason = 2018, endseason = 2018, qual = 100)
statcast = statcast_leaderboards(year = 2018, min_pa = 100)


statcast <- statcast %>%
  rename(
    PlayerName = `last_name, first_name`,  # Adjust based on actual column names
    brl_pa = brl_pa,
  )
statcast <- statcast %>%
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

#DO NOT TOUCH


combinedDF = merge(hitterDF, statcast, by = "PlayerName")


# List of columns to exclude
exclude_cols = c("wRC", "wRC_plus", "wRAA", "wOBA", "OPS+", "ISO", "BABIP", "xwOBA", "xAVG", "xSLG", 
                 "Age", "AgeRng", "SeasonMin", "SeasonMax", "G", "AB", "PA", "H", "1B", "2B", "3B", 
                 "HR", "R", "RBI", "SB", "CS", "BB", "SO", "IBB", "HBP", "SH", "SF", "GDP", "TB", "OBP", "SLG", "AVG", 
                 "PlayerNameRoute", "team_name", "Season", "xMLBAMID", "PlayerName", "Bats", "playerid", "WAR", "WAROld", 
                 "rFTeamV", "rBTeamV", "team_name_abb", "team_id")

# Create a new data frame without the excluded columns
model_data = combinedDF[, !(names(combinedDF) %in% exclude_cols)]
model_data_numeric <- model_data %>%
  select_if(is.numeric)

model_data_clean = model_data_numeric %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

test_model = lm(OPS ~., data = model_data_clean)
summary(test_model)
summary(step(test_model))
model_data$team

modelDF = combinedDF %>%
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )
testDF = data.frame(PlayerName = combinedDF$PlayerName, pfx_CU_pct = combinedDF$pfx_CU_pct)



full_model = lm(OPS ~ brl_pa * avg_hit_speed * avg_hit_angle * HardHit_pct * K_pct * BB_pct * `BB_pct+` * `FB_pct+`, data = modelDF)
full_model = lm(OPS ~.^2, data = modelDF)
reduced_noInter = step(full_model)
summary(full_model) #

reduced_model = step(full_model) #Multiple R-squared:  0.8417,	Adjusted R-squared:  0.7719 for 2018



summary(reduced_model)
reduced_model = step(reduced_model)

plot(reduced_model)

tempdf = data.frame(Name = combinedDF$Name, Hard_pctandbrl_pc = (combinedDF$Hard_pct *  combinedDF$Barrel_pct), WAR = combinedDF$oWAR)
str(combinedDF)


# Create a 3D scatterplot
plot_ly(modelDF, x = ~xwOBA, y = ~brl_pa, z = ~wOBA, color = ~wOBA) %>%
  add_markers() %>%
  layout(scene = list(
    xaxis = list(title = "xwOBA"),
    yaxis = list(title = "Barrel Rate (brl_pa)"),
    zaxis = list(title = "wOBA")
  ))



#2019:
hitterDF19 = fg_bat_leaders(startseason = 2019, endseason = 2019, qual = 100) #Gets the FG Hitting 
statcast19 = statcast_leaderboards(year = 2019, min_pa = 100) #Gets the Statcast percentile data

statcast19$`last_name, first_name` #Filtering to have same column Namesfor Names as HittersDF
statcast19 <- statcast19 %>%
  rename(
    PlayerName = `last_name, first_name`,
    brl_pa = brl_pa,
  )

statcast19 <- statcast19 %>% #Flips the names to lineup 
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

#DO NOT TOUCH

combinedDF = merge(hitterDF19, statcast19, by = "PlayerName") #Merges Dataset

modelDF = combinedDF %>% #This Is the Dataframe used for the stepwise regression and has all the predictors for OPS
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )

full_model = lm(OPS ~.^2, data = modelDF) #Interaction model to predict OPS
summary(full_model) #
reduced_model = step(full_model) #Multiple R-squared:  0.8493,	Adjusted R-squared:  0.7818 for 2019
summary(reduced_model)



#full_model = lm(OPS ~ brl_pa * avg_hit_speed * avg_hit_angle * HardHit_pct * K_pct * BB_pct * `BB_pct+` * `FB_pct+`, data = modelDF)


testDF = data.frame(PlayerName = combinedDF$PlayerName, pfx_CU_pct = combinedDF$pfx_CU_pct)



#2021:

hitterDF21 = fg_bat_leaders(startseason = 2021, endseason = 2021, qual = 100)
statcast21 = statcast_leaderboards(year = 2021, min_pa = 100)
statcast21 <- statcast21 %>%
  rename(
    PlayerName = `last_name, first_name`,
    brl_pa = brl_pa,
  )

statcast21 <- statcast21 %>%
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

combinedDF = merge(hitterDF21, statcast21, by = "PlayerName")

modelDF = combinedDF %>%
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )
full_model = lm(OPS ~.^2, data = modelDF)
summary(full_model) #
reduced_model = step(full_model) #Multiple R-squared:  0.843,	Adjusted R-squared:  0.7891 for 2021
summary(reduced_model) 

#2022:

hitterDF22 = fg_bat_leaders(startseason = 2022, endseason = 2022, qual = 100)
statcast22 = statcast_leaderboards(year = 2022, min_pa = 100)
statcast22 <- statcast22 %>%
  rename(
    PlayerName = `last_name, first_name`,
    brl_pa = brl_pa,
  )

statcast22 <- statcast22 %>%
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

combinedDF = merge(hitterDF22, statcast22, by = "PlayerName")

modelDF = combinedDF %>%
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )
full_model = lm(OPS ~.^2, data = modelDF)
summary(full_model) #
reduced_model = step(full_model) #Multiple R-squared:  0.8092,	Adjusted R-squared:  0.7509for 2022
summary(reduced_model) 

#2023

hitterDF23 = fg_bat_leaders(startseason = 2023, endseason = 2023, qual = 100)
statcast23 = statcast_leaderboards(year = 2023, min_pa = 100)
statcast23 <- statcast23 %>%
  rename(
    PlayerName = `last_name, first_name`,
    brl_pa = brl_pa,
  )

statcast23 <- statcast23 %>%
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

combinedDF = merge(hitterDF23, statcast23, by = "PlayerName")

modelDF = combinedDF %>%
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )
full_model = lm(OPS ~.^2, data = modelDF)
summary(full_model) #
reduced_model = step(full_model) #Multiple R-squared:  0.7933,	Adjusted R-squared:  0.6979 for 2023
summary(reduced_model) 

#2024:


hitterDF24 = fg_bat_leaders(startseason = 2024, endseason = 2024, qual = 100)
statcast24 = statcast_leaderboards(year = 2024, min_pa = 100)
statcast24 <- statcast24 %>%
  rename(
    PlayerName = `last_name, first_name`,
    brl_pa = brl_pa,
  )

statcast24 <- statcast24 %>%
  separate(PlayerName, into = c("Last", "First"), sep = ", ", remove = FALSE) %>%
  mutate(PlayerName = paste(First, Last)) %>%  # Combine into "First Last" format
  select(-Last, -First)  # Drop the temporary columns

combinedDF = merge(hitterDF24, statcast24, by = "PlayerName")

modelDF = combinedDF %>%
  select(
    OPS, brl_pa,
    avg_hit_speed, avg_hit_angle, HardHit_pct, K_pct, BB_pct, GB_FB, `BB_pct+`, `FB_pct+`,  `O-Swing_pct`, Swing_pct,
    Contact_pct, anglesweetspotpercent, `Pull_pct+`, Clutch, LD_pct, pfx_CU_pct, pfx_FA_pct, pfx_CH_pct, `pfx_O-Contact_pct`
  )
full_model = lm(OPS ~.^2, data = modelDF)
summary(full_model) #
reduced_model = step(full_model) #Multiple R-squared:  0.8101,	Adjusted R-squared:  0.7476 for 2024
summary(reduced_model) 
