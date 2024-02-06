'''
Project: Reds Hackathon 2024
Title: MLB’s Freaky Friday: Pitcher Role Reversal

Authors:
  -Emma Bluman
  -John Ilyevsky
  -Rohan Patel
  -Christopher Putnam
  -Daniel Mueller

Research Focus: What traditional relief pitchers would be effective starting pitchers?
'''

#READ IN LIBRARIES
library(tidyverse)
library(rpart)
library(rpart.plot)
library(data.table)
library(gridExtra)
library(class)
library(proxy)



#READ IN DATA
savant_pitch_level <- fread("/Users/emmabluman/Desktop/reds_case_study/savant_pitch_level.csv")
fangraphs_season_level  <- fread("/Users/emmabluman/Desktop/reds_case_study/fangraphs_season_level.csv")



#JUSTIFICATION FOR RESEARCH QUESTION-----

'''
As you read above, our reasearch question focuses on finding relief pitchers that could be effective starting
pitchers. We believe that turning a relief pitcher to a starter could offer tremendious value to a team.
Relief pitchers get paid less in the free agent market, arbitration, and pre-arbitration. Getting a pitcher
at the monetary value of a relief pitcher, and utilizing them as starting could offer extreme surplus value.
Look below for more evidence.
'''

#Filter to appearance in 5 games to filter out players with insignificantly low appearances
SP_VALUE <- fangraphs_season_level %>% filter(G > 5) %>%
  group_by(Role) %>%
  summarize(avg_WAR = mean(WAR), avg_IP = mean(IP), avg_WAR_IP = avg_WAR/avg_IP, avg_G = mean(G), avg_pLI= mean(pLI))

war_p <- fangraphs_season_level %>% filter(G > 5) %>%
  ggplot(aes(x = WAR, fill = Role)) +
  geom_boxplot() +
  labs(x = "WAR", title = "WAR: Relievers vs Starters") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "lightblue")) +
  guides(fill = guide_legend(reverse=TRUE))

ip_p <- fangraphs_season_level %>% filter(G > 5) %>%
  ggplot(aes(x = IP, fill = Role)) +
  geom_boxplot() +
  labs(x = "IP", title = "IP: Relievers vs Starters") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "lightblue")) +
  guides(fill = guide_legend(reverse=TRUE))

war_ip_p <- war_p <- fangraphs_season_level %>% filter(G > 5) %>%
  ggplot(aes(x = WAR/IP, fill = Role)) +
  geom_boxplot() +
  labs(x = "WAR/IP", title = "WAR/IP: Relievers vs Starters") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "lightblue")) +
  guides(fill = guide_legend(reverse=TRUE))

grid.arrange(war_p, ip_p, war_ip_p, ncol = 3)
'''
These boxplots allow you to see the value a starting pitcher has relative to a relief pitcher.
They offer more overall WAR over the course of a season. One could say that is just because they pitch
more innings, but you can see that starting pitchers offer more WAR/IP as well. Thus the overall
accumulation is more, and their value per inning is greater as well.
'''

#RELIEF PITCHERS WITH STARTING PITCHER PITCH MIX------

'''
One major dtinguisher between starting pitchers and relief pitchers is having a diverse enough pitch mix
that allows you to face hitters multiple times.
'''

#Define broader pitch type -- for simplification (Read analysis at the end of this section for more explanation)
savant_pitch_level <- savant_pitch_level %>%
  mutate(broader_pitch_type = case_when(pitch_type %in% c("FC", "FF", "SI", "CS", "FA") ~ "Fastball",
                                        pitch_type %in% c("CH", "FO", "KN", "FS") ~ "Off-Speed",
                                        pitch_type %in% c("CU", "EP", "KC", "SC", "SL", "SV", "ST") ~ "Breaking"))

#clean the data frame
savant_pitch_level <- savant_pitch_level %>%
  separate(player_name, into = c("Last", "First"), sep = ", ") %>%
  mutate(player_name = paste(First, Last, sep = " "), year = year(game_date)) %>%
  select(-First, -Last)

#Use fangraphs role title because defines their general role over the course of the season
Roles <- fangraphs_season_level %>%
  filter(G > 5) %>% #Filter to pitchers with 5 Games to remove significantly low mix
  rename(year = Season) %>%
  select(MLBAMID, Role, year)

Pitches <- savant_pitch_level %>%
  filter(!is.na(pitch_type)) %>%
  right_join(Roles, by = c("pitcher" = "MLBAMID", "year")) %>%
  group_by(player_name, year, broader_pitch_type, Role) %>%
  summarize(count = n()) %>%
  group_by(player_name, year) %>%
  mutate(percentage = count / sum(count)) #Find the percentage of the time they throw each of the three major pitch types

Pitches <- select(Pitches, player_name, year, Role, broader_pitch_type, percentage)


#Find the standard starting pitcher pitch mix
SP_pitches <- Pitches %>%
  filter(Role == "SP") %>%
  group_by(player_name, year) %>%
  pivot_wider(names_from = broader_pitch_type, values_from = percentage)


#Filtering out the bottom 10% of each pitch type to remove outliers
SP_pitches_analysis <- SP_pitches %>%
  group_by(Role) %>%
  summarize(pct_fastballs = quantile(Fastball, 0.1, na.rm = TRUE),
            pct_offspeed = quantile(`Off-Speed`, 0.1, na.rm = TRUE),
            pct_breaking = quantile(Breaking, 0.1, na.rm = TRUE))

#Filter relief pitchers to find pitchers that have comparable pitch mixes to a starting pitchers
result_RP_qualifying_mix <- Pitches %>%
  filter(Role == "RP") %>%
  group_by(player_name, year) %>%
  pivot_wider(names_from = broader_pitch_type, values_from = percentage) %>%
  summarise(pct_fastballs = quantile(coalesce(Fastball, 0), 0.1),
            pct_offspeed = quantile(coalesce(`Off-Speed`, 0), 0.1),
            pct_breaking = quantile(coalesce(Breaking, 0), 0.1)) %>%
  filter(pct_fastballs >= SP_pitches_analysis$pct_fastballs,
         pct_offspeed >= SP_pitches_analysis$pct_offspeed,
         pct_breaking >= SP_pitches_analysis$pct_breaking,
         year == 2023)

'''
Here we discover a list 217 pitchers that were considered relief pitchers by fangraphs that have pitch mixes that are
comparable to that of a starting pitcher in terms of having/utilizing a fastball, breaking ball, and off-speed pitch.

LIMITATIONS:We used broader pitch type classifaction for simplification reasons. This does not paint the full picture
for pitchers who throw multiple types fast balls, breaking balls, and/or offspeed pitches. Another limitation
of this approach is that it does not look at how many pitches each pitcher throws to righty hitters vs. lefty
hitters and if that matters to have an effective pitch mix to be a starter versus reliever. We suggest future
iterations of this code to look for ways to define pitch mix to address these issues.
'''


#RELIEVERS ADJUSTED STARTERS--------

'''
Through early exploratory analysis we discovered that relievers typically have better stuff_plus (hypothesis: they throw fewer
innings in an outing and can be more aggressive/throw with more strain per pitch) and lower location_plus (hypothesis: starters
have to throw more in the zone in order to record outs and go mutliple innings). We created a model below that adjusts all pitchers

'''

#Visualize the difference between starters and relievers

location_p <- fangraphs_season_level %>%
  filter(!is.na(Location_plus), G > 5) %>%
  ggplot(aes(x = Location_plus, fill = Role)) +
  geom_boxplot() +
  labs(x = "Location Plus", title = "Location Plus: Relievers vs Starters") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "lightblue")) +
  guides(fill = guide_legend(reverse=TRUE))

stuff_p <- fangraphs_season_level %>%
  filter(!is.na(Stuff_plus), G > 5) %>%
  ggplot(aes(x = Stuff_plus, fill = Role)) +
  geom_boxplot() +
  labs(x = "Stuff Plus", title = "Stuff Plus: Relievers vs Starters") +
  theme_minimal() +
  scale_fill_manual(values = c("coral", "lightblue")) +
  guides(fill = guide_legend(reverse=TRUE))

grid.arrange(location_p, stuff_p, ncol = 2)

fangraphs_season_level <- fangraphs_season_level %>%
  rename(player_name = Name, year = Season)

#Calculate said difference from above
fangraphs_totals <- fangraphs_season_level %>% filter(Stuff_plus != "NA", Location_plus != "NA", Pitching_plus != "NA") %>%
  mutate(Stuff = Pitches*Stuff_plus, Location = Pitches*Location_plus, Total = Pitches*Pitching_plus) %>%
  group_by(Role) %>%
  summarize(Stff = sum(Stuff)/sum(Pitches), Loc = sum(Location)/sum(Pitches), Pitching = sum(Total)/sum(Pitches)) %>%
  mutate(DiffStff = Stff - lag(Stff), DiffLoc = Loc - lag(Loc), DiffPitching = Pitching - lag(Pitching))

#Turn all pitchers into relievers
fangraphs_season_level <- fangraphs_season_level %>%
  mutate(Reliever_adjusted_stff = ifelse(Role == "SP", Stuff_plus - fangraphs_totals$DiffStff, Stuff_plus),
         Reliever_adjusted_loc = ifelse(Role == "SP", Location_plus - fangraphs_totals$DiffLoc, Location_plus),
         Reliever_adjusted_pitching = ifelse(Role == "SP", Pitching_plus - fangraphs_totals$DiffPitching, Pitching_plus))

SP <- fangraphs_season_level %>% filter(Role == "SP", year == 2023) %>% group_by(player_name, year, Role, IP, G)


# Assuming you have already calculated total differences
total_diff_stff <- -fangraphs_totals$DiffStff
total_diff_loc <- fangraphs_totals$DiffLoc
total_diff_pitching <- -fangraphs_totals$DiffPitching

# Use these total differences in your SP dataframe
SP <- SP %>%
  group_by(player_name, year, Role, IP, G) %>%
  summarize(
    Reliever_adjusted_stff = Stuff_plus + total_diff_stff,
    Reliever_adjusted_loc = Location_plus - total_diff_loc,
    Reliever_adjusted_pitching = Pitching_plus + total_diff_pitching,
    .groups = "drop"
  ) %>%
  filter(Reliever_adjusted_stff != "NA", Reliever_adjusted_loc != "NA", Reliever_adjusted_pitching != "NA")

Starters <- fangraphs_season_level %>% filter(Role == "SP") %>% group_by(player_name, year, Role, IP, G) %>%
  select(Stuff_plus, Location_plus, Pitching_plus)

RP <- fangraphs_season_level %>% filter(Role == "RP") %>% group_by(player_name, year, Role, IP, G) %>%
  select(Stuff_plus, Location_plus, Pitching_plus) %>%
  filter(Stuff_plus != "NA", Location_plus != "NA", Pitching_plus != "NA") %>%
  rename(Reliever_adjusted_stff = Stuff_plus, Reliever_adjusted_loc = Location_plus, Reliever_adjusted_pitching = Pitching_plus)

Combined <- bind_rows(SP, RP) %>%
  filter(G > 5) %>%
  select(-Role)

result_starter_adjusted <- left_join(Pitches, Combined, by = c("player_name", "year"))

total_diff_stff <- fangraphs_totals$DiffStff
total_diff_loc <- -fangraphs_totals$DiffLoc
total_diff_pitching <- fangraphs_totals$DiffPitching

# Use these total differences in your SP dataframe
result_starter_adjusted <- result_starter_adjusted %>%
  group_by(player_name, year, Role, IP, G) %>%
  summarize(
    Starter_adjusted_stff = Reliever_adjusted_stff + total_diff_stff,
    Starter_adjusted_loc = Reliever_adjusted_loc - total_diff_loc,
    Starter_adjusted_pitching = Reliever_adjusted_pitching + total_diff_pitching,
    .groups = "drop"
  )

result_starter_adjusted <- result_starter_adjusted %>% filter(Starter_adjusted_stff != "NA", Starter_adjusted_loc != "NA", Starter_adjusted_pitching != "NA") %>%
  filter(year == 2023)

#STAMINA-------

'''
Another factor for whether a reliever could be a starter, is if they have the stamina to pitch 5+ innings in an outing
and 160+ innings over the course of a season. This matters for whether they can be effective over the course of this many
innings and if they can do it without getting hurt. Below we look at a few different way to measure if pitchers can
effectively do so.
'''

###MODEL 1: PITCHERS WHO THROW MULTIPLE DAYS IN A ROW

#Determine the success of each individual pitch in savant table
for (i in 1:nrow(savant_pitch_level)) {

  savant_pitch_level$success_pitch[i] <- 0

  if (savant_pitch_level$description[i] == "foul" || savant_pitch_level$description[i] == "foul_tip" ) {
    savant_pitch_level$success_pitch[i] <- 0.5
  }

  if (savant_pitch_level$description[i] == "called_strike") {
    savant_pitch_level$success_pitch[i] <- 0.75
  }

  if (savant_pitch_level$description[i] == "swinging_strike" || savant_pitch_level$description[i] == "swinging_strike_blocked") {
    savant_pitch_level$success_pitch[i] <- 1
  }

  if (savant_pitch_level$pitch_name[i] == "Changeup" || savant_pitch_level$pitch_name[i] == "Cutter"
      || savant_pitch_level$pitch_name[i] == "Sinker" || savant_pitch_level$pitch_name[i] == "Forkball"
      || savant_pitch_level$pitch_name[i] == "Split-Finger") {

    if (savant_pitch_level$bb_type[i] == "ground_ball") {

      if (!is.na(savant_pitch_level$launch_speed[i]) && savant_pitch_level$launch_speed[i] < 95) {
        savant_pitch_level$success_pitch[i] <- 1
      }
    }
  }

  if (!is.na(savant_pitch_level$launch_speed[i]) && (savant_pitch_level$bb_type[i] == "popup" || savant_pitch_level$bb_type[i] == "fly_ball")) {

    if (!is.na(savant_pitch_level$launch_speed[i]) && savant_pitch_level$launch_speed[i] < 95) {
      savant_pitch_level$success_pitch[i] <- 1
    }
  }
}


#For each outing a pitcher has, what was their average success as defined above
savant_pitch_level_rp_group <- savant_pitch_level %>%
  filter(rp_indicator == 1) %>%
  group_by(player_name,game_date) %>%
  summarize(#mean_velo = mean(release_speed, na.rm = T),
    #mean_spin = mean(release_spin_rate, na.rm = T),
    sum_dre = sum(delta_run_exp, na.rm = T),
    mean_success = mean(success_pitch, na.rm = T),
    num_pitches = n()) %>%
  arrange(player_name, game_date)

savant_pitch_level_rp_group_23 <- savant_pitch_level_rp_group %>%
  filter(year(game_date) == 2023)


#make game date a datetime
savant_pitch_level_rp_group$game_date <- as.Date(savant_pitch_level_rp_group$game_date)
savant_pitch_level_rp_group$game_date_2 <- as.character.Date(savant_pitch_level_rp_group$game_date)

savant_pitch_level_rp_group_23$game_date <- as.Date(savant_pitch_level_rp_group_23$game_date)
savant_pitch_level_rp_group_23$game_date_2 <- as.character.Date(savant_pitch_level_rp_group_23$game_date)

#Find days since last appearance
savant_pitch_level_rp_group <- savant_pitch_level_rp_group %>%
  arrange(player_name, game_date) %>%
  mutate(days_since_last_app = as.numeric(game_date - lag(game_date)))

savant_pitch_level_rp_group_23 <- savant_pitch_level_rp_group_23 %>%
  arrange(player_name, game_date) %>%
  mutate(days_since_last_app = as.numeric(game_date - lag(game_date)))


#Label 2 days in a row
savant_pitch_level_rp_group <- savant_pitch_level_rp_group %>%
  arrange(player_name, game_date) %>%
  mutate(three_days_in_a_row = ifelse(days_since_last_app == 1 & lag(days_since_last_app) == 1, 1, 0),
         back_to_back = ifelse(days_since_last_app == 1, 1, 0))

savant_pitch_level_rp_group_23 <- savant_pitch_level_rp_group_23 %>%
  arrange(player_name, game_date) %>%
  mutate(three_days_in_a_row = ifelse(days_since_last_app == 1 & lag(days_since_last_app) == 1, 1, 0),
         back_to_back = ifelse(days_since_last_app == 1, 1, 0))

#Label three days in a row
three_in_row <- savant_pitch_level_rp_group %>%
  filter(three_days_in_a_row == 1)

three_in_row_23 <- savant_pitch_level_rp_group_23 %>%
  filter(three_days_in_a_row == 1)

#Find players back to back days
back_to_back_players <- savant_pitch_level_rp_group %>%
  filter(three_days_in_a_row == 0)  %>%
  filter(back_to_back == 1) %>%
  select(-three_days_in_a_row, -back_to_back, -game_date_2)

back_to_back_players_23 <- savant_pitch_level_rp_group_23 %>%
  filter(three_days_in_a_row == 0)  %>%
  filter(back_to_back == 1) %>%
  select(-three_days_in_a_row, -back_to_back, -game_date_2)

names_b2b <- as.vector(back_to_back_players$player_name) #all players who have done it
names_b2b_23 <- as.vector(back_to_back_players_23$player_name)

#Which pitchers threw back to back and what was their success?
b2b_baseline <- savant_pitch_level_rp_group %>%
  filter(player_name %in% names_b2b) %>%
  filter(back_to_back == 0) %>%
  group_by(player_name) %>%
  summarize(sum_dre_non = sum(sum_dre),
            pitches_non = sum(num_pitches),
            appearances_non = n(),
            dre_per_app_non = sum_dre_non / appearances_non,
            dre_per_pitch_non = sum_dre_non / pitches_non,
            mean_success_no = mean(mean_success, na.rm = T))

b2b_baseline_23 <- savant_pitch_level_rp_group_23 %>%
  filter(player_name %in% names_b2b) %>%
  filter(back_to_back == 0) %>%
  group_by(player_name) %>%
  summarize(sum_dre_non = sum(sum_dre),
            pitches_non = sum(num_pitches),
            appearances_non = n(),
            dre_per_app_non = sum_dre_non / appearances_non,
            dre_per_pitch_non = sum_dre_non / pitches_non,
            mean_success_no = mean(mean_success, na.rm = T))

b2b_performance <- back_to_back_players %>%
  group_by(player_name) %>%
  summarize(sum_dre_yes = sum(sum_dre),
            pitches_yes = sum(num_pitches),
            appearances_yes = n(),
            dre_per_app_yes = sum_dre_yes / appearances_yes,
            dre_per_pitch_yes = sum_dre_yes / pitches_yes,
            mean_success_yes = mean(mean_success, na.rm = T))

b2b_performance_23 <- back_to_back_players_23 %>%
  group_by(player_name) %>%
  summarize(sum_dre_yes = sum(sum_dre),
            pitches_yes = sum(num_pitches),
            appearances_yes = n(),
            dre_per_app_yes = sum_dre_yes / appearances_yes,
            dre_per_pitch_yes = sum_dre_yes / pitches_yes,
            mean_success_yes = mean(mean_success, na.rm = T))

b2b_diff <- left_join(b2b_performance, b2b_baseline)
b2b_diff_23 <- left_join(b2b_performance_23, b2b_baseline_23)

b2b_diff <- b2b_diff %>%
  group_by(player_name) %>%
  mutate(diff_dre = dre_per_app_yes - dre_per_app_non,
         diff_success = mean_success_yes - mean_success_no)
b2b_diff_23 <- b2b_diff_23 %>%
  group_by(player_name) %>%
  mutate(diff_dre = dre_per_app_yes - dre_per_app_non,
         diff_success = mean_success_yes - mean_success_no)


#Find success level for three days in a row
names_three_in_row <- as.vector(three_in_row$player_name)
names_three_in_row_23 <- as.vector(three_in_row_23$player_name)

three_in_row_players_not_three_in_row <- savant_pitch_level_rp_group %>%
  filter(player_name %in% names_three_in_row) %>%
  filter(three_days_in_a_row == 0) %>%
  group_by(player_name) %>%
  summarize(sum_dre_non = sum(sum_dre),
            pitches_non = sum(num_pitches),
            appearances_non = n(),
            dre_per_app_non = sum_dre_non / appearances_non,
            dre_per_pitch_non = sum_dre_non / pitches_non,
            mean_success_no = mean(mean_success, na.rm = T))
three_in_row_players_not_three_in_row_23 <- savant_pitch_level_rp_group_23 %>%
  filter(player_name %in% names_three_in_row) %>%
  filter(three_days_in_a_row == 0) %>%
  group_by(player_name) %>%
  summarize(sum_dre_non = sum(sum_dre),
            pitches_non = sum(num_pitches),
            appearances_non = n(),
            dre_per_app_non = sum_dre_non / appearances_non,
            dre_per_pitch_non = sum_dre_non / pitches_non,
            mean_success_no = mean(mean_success, na.rm = T))


three_in_row_performance <- three_in_row %>%
  group_by(player_name) %>%
  summarize(sum_dre_yes = sum(sum_dre),
            pitches_yes = sum(num_pitches),
            appearances_yes = n(),
            dre_per_app_yes = sum_dre_yes / appearances_yes,
            dre_per_pitch_yes = sum_dre_yes / pitches_yes,
            mean_success_yes = mean(mean_success, na.rm = T))
three_in_row_performance_23 <- three_in_row_23 %>%
  group_by(player_name) %>%
  summarize(sum_dre_yes = sum(sum_dre),
            pitches_yes = sum(num_pitches),
            appearances_yes = n(),
            dre_per_app_yes = sum_dre_yes / appearances_yes,
            dre_per_pitch_yes = sum_dre_yes / pitches_yes,
            mean_success_yes = mean(mean_success, na.rm = T))

diff_three_performance <- left_join(three_in_row_performance, three_in_row_players_not_three_in_row)
diff_three_performance_23 <- left_join(three_in_row_performance_23, three_in_row_players_not_three_in_row_23)

#Find success over the three days
diff_three_performance <- diff_three_performance %>%
  group_by(player_name) %>%
  mutate(diff_dre = dre_per_app_yes - dre_per_app_non,
         diff_success = mean_success_yes - mean_success_no)
diff_three_performance_23 <- diff_three_performance_23 %>%
  group_by(player_name) %>%
  mutate(diff_dre = dre_per_app_yes - dre_per_app_non,
         diff_success = mean_success_yes - mean_success_no)


#Summarizing the results of b2b
b2b_diff %>%
  ungroup() %>%
  summarize(mean_dre_diff = mean(diff_dre),
            mean_success_diff = mean(diff_success))
b2b_diff_23 %>%
  ungroup() %>%
  summarize(mean_dre_diff = mean(diff_dre),
            mean_success_diff = mean(diff_success))

b2b_diff <- b2b_diff %>%
  mutate(b2b_diff_success_oe = diff_success + 0.000394,
         b2b_diff_dre_oe = diff_dre - 0.0226)
b2b_diff_23 <- b2b_diff_23 %>%
  mutate(b2b_diff_success_oe = diff_success + 0.000394,
         b2b_diff_dre_oe = diff_dre - 0.0226)

b2b_output <- b2b_diff %>%
  group_by(player_name) %>%
  select(player_name, appearances_yes, b2b_diff_success_oe, b2b_diff_dre_oe) %>%
  filter(appearances_yes > 25) %>%
  filter(b2b_diff_dre_oe < .5 && b2b_diff_success_oe > -.5)

b2b_output_23 <- b2b_diff_23 %>%
  group_by(player_name) %>%
  select(player_name, appearances_yes, b2b_diff_success_oe, b2b_diff_dre_oe) %>%
  filter(appearances_yes > 8) %>%
  filter(b2b_diff_dre_oe < .5 && b2b_diff_success_oe > -.5)

#Visualize results
result_multiple_days <- tibble(b2b_output)
result_multiple_days_23 <- tibble(b2b_output_23)


b2b_p1 <- result_multiple_days %>%
  ggplot(aes(x = player_name, y = b2b_diff_dre_oe)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkgrey") +
  coord_flip() +
  labs(title = "The Best RPs on 'Back-to-Backs' (2021-2023)",
       y = "Delta Run Expectancy over Expected", x = "Player Name")+
  theme_minimal()

b2b_p2 <- result_multiple_days %>%
  ggplot(aes(x = player_name, y = b2b_diff_success_oe)) +
  geom_bar(stat = "identity", fill = "coral", color = "darkgrey") +
  coord_flip() +
  labs(title = "The Best RPs on 'Back-to-Backs' (2021-2023)",
       y = "Pitch Success over Expected", x = "Player Name")+
  theme_minimal()

grid.arrange(b2b_p1, b2b_p2, ncol = 2)

b2b23_p1 <- result_multiple_days_23 %>%
  ggplot(aes(x = player_name, y = b2b_diff_dre_oe)) +
  geom_bar(stat = "identity", fill = "lightblue", color = "darkgrey") +
  coord_flip() +
  labs(title = "The Best RPs on 'Back-to-Backs' (2021-2023)",
       y = "Delta Run Expectancy over Expected", x = "Player Name")+
  theme_minimal()

b2b23_p2 <- result_multiple_days_23 %>%
  ggplot(aes(x = player_name, y = b2b_diff_success_oe)) +
  geom_bar(stat = "identity", fill = "coral", color = "darkgrey") +
  coord_flip() +
  labs(title = "The Best RPs on 'Back-to-Backs' (2021-2023)",
       y = "Pitch Success over Expected", x = "Player Name")+
  theme_minimal()

grid.arrange(b2b23_p1, b2b23_p2, ncol = 2)

###MODEL 2: PITCHERS WHO THREW SIGNIFICANT AMOUNTS IN ONE WEEK

#Define 5 day peroid and 7 day peroid
result_five_days <-savant_pitch_level_rp_group %>%
  mutate(Week = lubridate::floor_date(game_date, unit = "5 days")) %>%
  group_by(player_name, Week) %>%
  summarize(TotalPitches = sum(num_pitches), mean_dre = mean(sum_dre),
            success = mean(mean_success), appearances = n())

result_five_days_23 <-savant_pitch_level_rp_group_23 %>%
  mutate(Week = lubridate::floor_date(game_date, unit = "5 days")) %>%
  group_by(player_name, Week) %>%
  summarize(TotalPitches = sum(num_pitches), mean_dre = mean(sum_dre),
            success = mean(mean_success), appearances = n())

weekly_data <-  savant_pitch_level_rp_group %>%
  mutate(Week = lubridate::floor_date(game_date, unit = "week")) %>%
  group_by(player_name, Week) %>%
  summarize(TotalPitches = sum(num_pitches), mean_dre_per_app = mean(sum_dre),
            success = mean(mean_success), appearances = n())
weekly_data_23 <-  savant_pitch_level_rp_group_23 %>%
  mutate(Week = lubridate::floor_date(game_date, unit = "week")) %>%
  group_by(player_name, Week) %>%
  summarize(TotalPitches = sum(num_pitches), mean_dre_per_app = mean(sum_dre),
            success = mean(mean_success), appearances = n())

weekly_data <- weekly_data %>%
  mutate(total_dre = mean_dre_per_app * appearances,
         dre_pitch = total_dre / TotalPitches)
weekly_data_23 <- weekly_data_23 %>%
  mutate(total_dre = mean_dre_per_app * appearances,
         dre_pitch = total_dre / TotalPitches)

weekly_data %>%
  ungroup() %>%
  summarize(pop_mean_dre_pitch = mean(dre_pitch, na.rm = T),
            pop_mean_success = mean(success, na.rm = T))
weekly_data_23 %>%
  ungroup() %>%
  summarize(pop_mean_dre_pitch = mean(dre_pitch, na.rm = T),
            pop_mean_success = mean(success, na.rm = T))

weekly_data <- weekly_data %>%
  mutate(mean_dre_pitch_oe = dre_pitch + 0.00255,
         mean_success_oe = success - 0.399,
         total_dre_oe = mean_dre_pitch_oe * TotalPitches)
weekly_data_23 <- weekly_data_23 %>%
  mutate(mean_dre_pitch_oe = dre_pitch + 0.00255,
         mean_success_oe = success - 0.399,
         total_dre_oe = mean_dre_pitch_oe * TotalPitches)

weekly_75 <- weekly_data %>%
  filter(TotalPitches > 75)
weekly23_75 <- weekly_data_23 %>%
  filter(TotalPitches > 75)


#List of pitchers who threw 75 pitches in one week
result_75 <- weekly_75 %>%
  group_by(player_name) %>%
  summarize(total = n(),
            dre_pitch_oe_mean_75 = mean(mean_dre_pitch_oe),
            mean_success_oe_75 = mean(mean_success_oe)) %>%
  arrange(desc(total))
result23_75 <- weekly23_75 %>%
  group_by(player_name) %>%
  summarize(total = n(),
            dre_pitch_oe_mean_75 = mean(mean_dre_pitch_oe),
            mean_success_oe_75 = mean(mean_success_oe)) %>%
  arrange(desc(total))

###MODEL 3: Threw multiple innings in one outing



#TREE DIAGRAMS and LOGISTIC MODEL---------



#Version 1: All stats except cumalitive
fangraphs_tree <- fangraphs_season_level %>% filter(G > 5) %>%
#  rename(player_name = Name, year = Season) %>%
  select(-PlayerId, -player_name, -MLBAMID, -NameASCII, -GS, -MD, -SD,
         -HLD, -Pulls, -G, -SV, -BS, -HardHit, -H, -TBF, -ER, -Events,
         -Pitches, -HR, -SO, -BB, -IBB, -HBP, -BK, -GB, -LD, -FB, -IFFB,
         -BU, -IFH, -BUH, -Strikes, -Balls, -RS, -R, -IP, -Barrels, -HardHit,
         -gmLI, -inLI, -exLI, -Team, -L, -W, -RAR, -WAR, -Dollars, -XX_pct)


train_frangraphs <- fangraphs_tree %>% filter(year != 2023)

test_fangraphs <- fangraphs_tree %>% filter(year == 2023) %>%
  select(-Role)


set.seed(123)

tree_model <- rpart(Role ~., data = train_frangraphs, method = "class")
rpart.plot(tree_model)


predictions <- predict(tree_model, test_fangraphs, type = "class")



# Add the predicted role to the test data
test_fangraphs <- cbind(test_fangraphs, PredictedRole = predictions)

# Add back the pitcher name and role
output <- test_fangraphs %>%
  left_join(fangraphs_season_level,
            by = c("Throws", "Age", "K_minus_BB_pct", "SIERA", "WHIP", "BABIP")) %>%
  select(PlayerId, player_name, Role, PredictedRole)


error_label <- output %>%
  filter(Role != PredictedRole)





#Version 2 based on pitch mix
fangraphs_tree2 <- fangraphs_season_level %>% filter(G > 5) %>%
#  rename(player_name = Name, year = Season) %>%
  select(year, Role, Throws, FA_pct, FAv, SL_pct, SLv, CT_pct, CTv, CB_pct, CBv, CH_pct,
         CHv, SF_pct, SFv, KN_pct, KNv, FA_pct_sc, FC_pct_sc, FS_pct_sc,
         FO_pct_sc, SI_pct_sc, SL_pct_sc, CU_pct_sc, KC_pct_sc, EP_pct_sc,
         CH_pct_sc, SC_pct_sc, KN_pct_sc, UN_pct_sc, vFA_sc, vFC_sc, vFS_sc,
         vFO_sc, vSI_sc, vSL_sc, vCU_sc, vKC_sc, vEP_sc, vCH_sc, vSC_sc, vKN_sc,
         FA_X_sc, FC_X_sc, FS_X_sc, FO_X_sc, SI_X_sc, SL_X_sc, CU_X_sc, KC_X_sc,
         EP_X_sc, CH_X_sc, SC_X_sc, KN_X_sc, FA_Z_sc, FC_Z_sc, FS_Z_sc, FO_Z_sc,
         SI_Z_sc, SL_Z_sc, CU_Z_sc, KC_Z_sc, EP_Z_sc, CH_Z_sc, SC_Z_sc, KN_Z_sc,
         Stf_plus_CH, Loc_plus_CH, Pit_plus_CH, Stf_plus_CU, Loc_plus_CU, Pit_plus_CU,
         Stf_plus_FA, Loc_plus_FA, Pit_plus_FA, Stf_plus_SI, Loc_plus_SI, Pit_plus_SI,
         Stf_plus_SL, Loc_plus_SL, Pit_plus_SL, Stf_plus_KC, Loc_plus_KC, Pit_plus_KC,
         Stf_plus_FC, Loc_plus_FC, Pit_plus_FC, Stf_plus_FS, Loc_plus_FS, Pit_plus_FS,
         Stf_plus_FO, Loc_plus_FO, Pit_plus_FO, Stuff_plus, Location_plus, Pitching_plus)


train_frangraphs2 <- fangraphs_tree2 %>% filter(year != 2023)

test_fangraphs2 <- fangraphs_tree2 %>% filter(year == 2023) %>%
  select(-Role)


set.seed(123)

tree_model2 <- rpart(Role ~., data = train_frangraphs2, method = "class")
rpart.plot(tree_model2)


predictions2 <- predict(tree_model2, test_fangraphs2, type = "class")

# Add the predicted role to the test data
test_fangraphs2 <- cbind(test_fangraphs2, PredictedRole = predictions2)

fangraphs_season_level <- fangraphs_season_level

# Add back the pitcher name and role
output2 <- test_fangraphs2 %>%
  left_join(fangraphs_season_level %>%
              select(PlayerId, player_name, Role, FA_pct, FAv, SL_pct, SLv, CT_pct, CTv, CB_pct, CBv, CH_pct),
            by = c("FA_pct", "FAv", "SL_pct", "SLv", "CT_pct", "CTv", "CB_pct", "CBv", "CH_pct")) %>%
  select(PlayerId, player_name, Role, PredictedRole)

error_label2 <- output2 %>%
  filter(Role != PredictedRole)


#Find overlap in two models/Pitchers to dive deeper into
error_label_starters <- error_label %>% filter(PredictedRole == "SP") %>% mutate(tree_diagram_1 = 1.5)

error_label_starters2 <- error_label2 %>% filter(PredictedRole == "SP") %>% mutate(tree_diagram_2 = 1.5)

result_should_be_starters <- error_label_starters %>%
  full_join(error_label_starters2, by = c("player_name", "PlayerId", "Role", "PredictedRole")) %>%
  select(player_name, tree_diagram_1, tree_diagram_2) %>%
  mutate(tree_diagram_1 = coalesce(tree_diagram_1, 0),
         tree_diagram_2 = coalesce(tree_diagram_2, 0))


#FINAL OUTPUT---------

result_should_be_starters <- result_RP_qualifying_mix %>%
  mutate(qualifying_mix = 1) %>%
  mutate(qualifying_mix = coalesce(qualifying_mix, 0)) %>%
  select(player_name, qualifying_mix) %>%
  full_join(result_should_be_starters)

'''
result_should_be_starters <- result_multiple_days_23 %>%
  mutate(multiple_days_23 = .75) %>%
  mutate(multiple_days_23 = coalesce(multiple_days_23,0)) %>%
  select(player_name, multiple_days_23) %>%
  full_join(result_should_be_starters)

result_should_be_starters <- result_multiple_days %>%
  mutate(multiple_days_2123 = .25) %>%
  mutate(multiple_days_2123 = coalesce(multiple_days_2123,0)) %>%
  select(player_name, multiple_days_2123) %>%
  full_join(result_should_be_starters)
'''

result_should_be_starters <- b2b_output_23 %>%
  mutate(b2b_output_23 = .75) %>%
  mutate(b2b_output_23 = coalesce(b2b_output_23,0)) %>%
  select(player_name, b2b_output_23) %>%
  full_join(result_should_be_starters)

result_should_be_starters <- b2b_output %>%
  mutate(b2b_output = .25) %>%
  mutate(b2b_output = coalesce(b2b_output,0)) %>%
  select(player_name, b2b_output) %>%
  full_join(result_should_be_starters)

result_should_be_starters <- result23_75 %>%
  mutate(pitches75_23 = .75) %>%
  mutate(pitches75_23 = coalesce(pitches75_23,0)) %>%
  select(player_name, pitches75_23) %>%
  full_join(result_should_be_starters)

result_should_be_starters <- result_75 %>%
  mutate(pitches75 = .25) %>%
  mutate(pitches75 = coalesce(pitches75,0)) %>%
  select(player_name, pitches75) %>%
  full_join(result_should_be_starters)

result_should_be_starters <- result_should_be_starters %>%
  mutate(qualifiers = rowSums(select(., c("qualifying_mix", "b2b_output_23",
                                          "b2b_output", "pitches75_23", "pitches75",
                                          "tree_diagram_1", "tree_diagram_2")), na.rm = TRUE)) %>%
  filter(qualifiers == 1)

result_should_be_starter <- fangraphs_season_level %>%
  group_by(player_name, Role)  %>%
  summarize(PlayerId = mean(PlayerId)) %>%
  group_by(player_name) %>%
  summarize(n = n(), Role = Role) %>%
  filter(n == 1, Role == "RP") %>%
  select(player_name) %>%
  inner_join(result_should_be_starters)

result_should_be_starter <- result_should_be_starter %>%
  left_join(result_starter_adjusted) %>%
  select(-pitches75, -pitches75_23, -b2b_output, -b2b_output_23,
         -qualifying_mix, -tree_diagram_1, -tree_diagram_2)


starter_threshold <- fangraphs_season_level %>%
  filter(Role == "SP", year == 2023, IP > 161) %>%
  summarize(Stuff_plus = quantile(Stuff_plus, .1),
            Location_plus = quantile(Location_plus, .1),
            Pitching_plus = quantile(Pitching_plus, .1))


result_should_be_starter <- result_should_be_starter %>%
  filter(Starter_adjusted_stff >= starter_threshold$Stuff_plus,
         Starter_adjusted_loc >= starter_threshold$Location_plus,
         Starter_adjusted_pitching >= starter_threshold$Pitching_plus)


write_csv(result_should_be_starter, "/Users/emmabluman/Desktop/reds_case_study/should_be_starter.csv")

# Loading data
fread('fangraphs_season_level.csv') -> fg
fread('savant_pitch_level.csv') -> bs
fread('should_be_starter.csv') -> names

# Grabbing only the player's info (name, team, etc) and pitch-type specific metrics
fg[, c(1:9, 140:154, 156:242, 251:274, 279:305)] -> fg_reduced

set.seed(1)
# Looping through each player in the should_be_starter csv
lapply(names$player_name, function(i) {
  # Filtering the fg data to only include the current player's 2023 data
  fg_reduced %>%
    filter(NameASCII == i & Season == 2023) -> player

  # Getting rid of columns for pitches the player doesn't throw
  player %>%
    select(names(fg_reduced)[colSums(is.na(player)) == 0]) -> player

  # Reducing the remaining fangraphs data to only include the same pitches the current player throws
  fg_reduced %>%
    filter(NameASCII != i) %>%
    select(c(names(fg_reduced)[1:9], names(player)[10:ncol(player)])) -> rest_of_league
  rest_of_league %>%
    filter(if_all(names(player)[10:ncol(player)], ~ !is.na(.))) -> rest_of_league

  # Running K-Nearest Neighbors to predict the current player's role based on their pitch metrics
  knn_model <- knn(rest_of_league[,10:ncol(rest_of_league)], player[,10:ncol(player)], rest_of_league$Role, 5, prob = T)
  player$predicted_role <- knn_model

  # Grabbing only the players who are predicted to be starters from the KNN
  player %>%
    filter(predicted_role == 'SP') -> player

  if (nrow(player) > 0) {

    lapply(1:nrow(player), function(j) {
      rest_of_league -> compare

      # Finding the Euclidean distance between the current player and everyone else in the league
      proxy::dist(
        as.matrix(rest_of_league[,10:ncol(rest_of_league)]),
        as.matrix(player[j, 10:ncol(rest_of_league)]),
        method = 'Euclidean'
      ) -> compare$distance

      list(
        # Info about the current player
        player[j,] %>%
          select(NameASCII, Season, Role, predicted_role),
        # The ten other players in the league with the shortest distance to the current player (most similar pitch mixes)
        compare %>%
          arrange(distance) %>%
          select(c(NameASCII, Season, Team, Role)) %>%
          head(10)
      ) %>%
        return()

    }) %>%
      return()

  }

}) %>% Filter(f = Negate(is.null)) %>% unlist(recursive = F) -> narrowed_down_list

narrowed_down_list





