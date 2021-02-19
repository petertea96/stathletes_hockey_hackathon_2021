##### NWHL (Does not have the same players as in Olympics) #####
ohl_data <- read.csv('data/hackathon_scouting.csv', 
                      stringsAsFactors = FALSE)
ohl_faceoff_data <- ohl_data %>%
  filter(Event == 'Faceoff Win') 

sort(table(ohl_faceoff_data$Player))




fo_winners <- ohl_faceoff_data %>%
  mutate(p1_won = 1,
         is_p1_home =  ifelse(Home.Team == Team, 1,0), 
         p1_team_players = ifelse(is_p1_home ==1,
                                  Home.Team.Skaters,
                                  Away.Team.Skaters),
         p2_team_players = ifelse(is_p1_home ==1,
                                  Away.Team.Skaters,
                                  Home.Team.Skaters),
         p1_team_goals = ifelse(is_p1_home ==1,
                                Home.Team.Goals,
                                Away.Team.Goals),
         p2_team_goals = ifelse(is_p1_home ==1,
                                Away.Team.Goals,
                                Home.Team.Goals)
         
  ) %>%
  select(-Home.Team.Skaters, -Away.Team.Skaters,
         -Home.Team.Goals, -Away.Team.Goals,
         -Detail.2, -Detail.3, -Detail.4, 
         -X.Coordinate.2, -Y.Coordinate.2 #,
         #-Home.Team, -Away.Team, -Event
  ) %>%
  rename(p1 = Player,
         p2 = Player.2) 

fo_losers <- ohl_faceoff_data %>%
  mutate(p1_won = 0,
         is_p1_home =  ifelse(Home.Team == Team, 0,1), 
         p1_team_players = ifelse(is_p1_home ==1,
                                  Home.Team.Skaters,
                                  Away.Team.Skaters),
         p2_team_players = ifelse(is_p1_home ==1,
                                  Away.Team.Skaters,
                                  Home.Team.Skaters),
         p1_team_goals = ifelse(is_p1_home ==1,
                                Home.Team.Goals,
                                Away.Team.Goals),
         p2_team_goals = ifelse(is_p1_home ==1,
                                Away.Team.Goals,
                                Home.Team.Goals)
  ) %>%
  select(game_date, Home.Team, Away.Team, Period, Clock,
         Team, Player.2, Event, X.Coordinate, Y.Coordinate, Detail.1, 
         Player, p1_won, is_p1_home, p1_team_players, p2_team_players,
         p1_team_goals, p2_team_goals) %>%
  rename(p1 = Player.2,
         p2 = Player) 

ohl_faceoff_data_all <- rbind(fo_winners, fo_losers)


# -- Add identifier of whether draw was {Defensive, Offensive, Neutral}
### Faceoff Zones
zone = ifelse( ohl_faceoff_data$X.Coordinate < 75, 'D',
               ifelse( (ohl_faceoff_data$X.Coordinate > 75) & (ohl_faceoff_data$X.Coordinate < 125), 'N',
                       ifelse(ohl_faceoff_data$X.Coordinate > 125, 'O', NA))
               
)

#head(zone,20)

zone_complement <- ifelse(zone == 'D', 'O',
                          ifelse(zone == 'O', 'D',
                                 ifelse(zone == 'N', zone, NA)))

ohl_faceoff_data_all$zone <- c(zone, zone_complement)

# Add Faceoff Side 
faceoff_side <- ifelse(ohl_faceoff_data$Y.Coordinate > 42.5, 'L', 'R')
faceoff_side_complement <- ifelse(faceoff_side == 'R', 'L', 'R')
ohl_faceoff_data_all$faceoff_side <- c(faceoff_side, faceoff_side_complement)



ohl_faceoff_data_all<- cbind(ohl_faceoff_data_all, 
                          nnet::class.ind(ohl_faceoff_data_all$zone),
                          nnet::class.ind(ohl_faceoff_data_all$faceoff_side))



# table(ohl_faceoff_data_all$Home.Team)

ohl_faceoff_data_all <- ohl_faceoff_data_all %>%
  mutate(is_p1_up_goals = ifelse( (p1_team_goals - p2_team_goals) > 0, 1,0 ),
         is_p1_down_goals = ifelse( (p1_team_goals - p2_team_goals) < 0, 1,0 ),
         is_p1_powerplay = ifelse( (p1_team_players - p2_team_players) >0,1,0 ),
         is_p1_sh = ifelse( (p1_team_players - p2_team_players) < 0,1,0 ),
         is_p1_pp1 =  ifelse( (p1_team_players - p2_team_players) == 1,1,0 ),
         is_p1_pp2 =  ifelse( (p1_team_players - p2_team_players) == 2,1,0 ),
         is_p1_sh1 = ifelse( (p1_team_players - p2_team_players) == -1,1,0 ),
         is_p1_sh2 = ifelse( (p1_team_players - p2_team_players) == -2,1,0 )
  ) 


# Only include players with more than 5 FOs taken?
players_of_interest <- ohl_faceoff_data_all %>%
  group_by(p1) %>%
  summarise(fot = n()) %>%
  arrange(fot) %>%
  filter(fot >= 5) %>%
  .$p1

ohl_faceoff_data_to_fit <-ohl_faceoff_data_all%>%
  filter(p1 %in% players_of_interest)

# Variables that seem to have no significant fixed effect:
# is_p1_up_goals, is_p1_down_goals, Period, percent_period_left
# Specicying 5on3 and 5on4 offers marginal model improvement (AIC reduces by 2 units)
ohl_model_fit <- glmer(formula = p1_won ~ is_p1_home + 
                             is_p1_powerplay + is_p1_sh +
                             # percent_period_left +
                             #is_p1_pp1 + is_p1_pp2 +
                             #is_p1_sh1 + is_p1_sh2 +
                             L + 
                             D + O + 
                             (1 + L | p1) +
                             (1 | p2), 
                           #data = ohl_faceoff_data_all, 
                           data =  ohl_faceoff_data_to_fit,
                           control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                           family = binomial)

summary(ohl_model_fit)
VarCorr(ohl_model_fit)
