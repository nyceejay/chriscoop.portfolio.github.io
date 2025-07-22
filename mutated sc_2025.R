library(baseballr)
library(tidyverse)
library(gtExtras)
library(systemfonts)
library(paletteer)
library(ggthemes)
library(webshot2)
library(abdwr3edata)


#Adds the created statcast data to directory folder
sc_2025 = statcast_read_csv(dir ="C:\\Users\\chris\\OneDrive\\Documents\\statcast_csv_days_2025", 
                            pattern = "*.csv")

#Daily write 
write.csv(statcast_search(start_date = Sys.Date()-1,
                          end_date = Sys.Date()-1),
          "/Users/chris/OneDrive/Documents/statcast_csv_days_2025/sc_2025-07-21.csv",
          row.names = FALSE)



mutated_sc_2025 = sc_2025 %>% 
  mutate(
    team_batting = ifelse(inning_topbot == "Bot",
                          home_team,
                          away_team),
    team_pitching = ifelse(inning_topbot == "Bot",
                           away_team,
                           home_team),
    inning_bins = cut(inning, seq(1,9, by = 3),
                      include.lowest = T),
    on_base = ifelse(
      (!is.na(on_1b) | !is.na(on_2b) | !is.na(on_3b)),
      "runners on",
      "no runners on"
    ),
    risp = ifelse(
      (!is.na(on_2b) | !is.na(on_3b)),
      "RISP",
      "no RISP"
    ),
    bat_spray_angle = atan((hc_x - 125.42)/
                             (198.27 - hc_y)),
    bat_spray_angle = ifelse(
      stand == "L", -bat_spray_angle, bat_spray_angle
    ),
    oppo_pull = ifelse(
      bat_spray_angle < 0, "pull", "oppo"
    ),
    hardhit = ifelse(
      launch_speed >= 95, "hardhit", "not hardhit"
    ),
    sweetspot = ifelse(
      (launch_angle >= 8 | launch_angle <=32),
      "sweetspot",
      "not sweetspot"
    ),
    count = case_when(
      (balls == 0 & strikes == 0) ~ "First Pitch",
      ((balls == 0 & strikes == 1) |
         (balls == 0 & strikes == 2) |
         (balls == 1 & strikes == 2)) ~ "Pitchers Count",
      ((balls == 1 & strikes == 1) |
         (balls == 2 & strikes == 2)) ~ "Even",
      ((balls == 1 & strikes == 0) |
         (balls == 2 & strikes == 0) |
         (balls == 2 & strikes == 1) |
         (balls == 3 & strikes == 0) |
         (balls == 3 & strikes == 1)) ~ "Batters Count",
      (balls == 3 & strikes == 2) ~ "Full Count"
    ),
    attack_zone_heart = ifelse(
      ((plate_x <= 6.7/12) & 
         (plate_x >= -6.7/12)) &
        ((plate_z <= sz_top - 1/6*(sz_top - sz_bot)) & 
           (plate_z >= sz_bot + 1/6*(sz_top - sz_bot))),
      "heart",
      "no_heart"),
    attack_zone_shadow = ifelse(
      attack_zone_heart != "heart" &
        ((plate_x >= -13.3/12 & plate_x <= 13.3/12) &
           (plate_z >= (sz_bot - 1/6*(sz_top - sz_bot)) & 
              plate_z <= (sz_top + 1/6*(sz_top - sz_bot)))),
      "shadow",
      "no_shadow"
    ),
    attack_zone_chase = ifelse(
      ((attack_zone_shadow != "shadow") &
         (attack_zone_heart != "heart") &
         (plate_z >= sz_bot - 1/2*(sz_top - sz_bot) &
            (plate_z <= sz_top + 1/2*(sz_top - sz_bot))&
            (plate_x >= -20 & plate_x <= 20))),
      "chase", "no_chase"
    ))
  



generate_sc_contact_descriptions = function(a){
  paste0("c('foul','hit_into_play')")
}

generate_sc_whiff_descriptions = function(a){
  paste0("c('foul_tip','swinging_strike', 'swinging_strike_blocked')")
}

generate_sc_PA_events = function(a){
  paste0("c('single', 'double', 'triple', 
  'home_run', 'force_out', 'field_out',
  'strikeout', 'sac_fly', 'sac_bunt', 
  'double_play', 'triple_play', 'strikeout_double_play',
  'fielders_choice', 'walk', 
  'grounded_into_double_play', 'hit_by_pitch',
  'sac_fly_double_play')")
}



