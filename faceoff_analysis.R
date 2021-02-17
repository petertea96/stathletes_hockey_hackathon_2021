library(dplyr)
library(lme4)
setwd("C:/Users/Peter/Documents/stathletes_hockey_hackathon_2021")

##### Step 1 - Process Women's Olympic hockey data #####


olympic_data <- read.csv('data/hackathon_womens.csv',
                         stringsAsFactors = FALSE)


olympic_womens_faceoff_data <- olympic_data %>%
  filter(Event == 'Faceoff Win') 


fo_winners <- olympic_womens_faceoff_data %>%
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

fo_losers <- olympic_womens_faceoff_data %>%
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

olympic_faceoff_data <- rbind(fo_winners, fo_losers)

olympic_faceoff_data <- olympic_faceoff_data %>%
  mutate(is_p1_up_goals = ifelse( (p1_team_goals - p2_team_goals) > 0, 1,0 ),
         is_p1_down_goals = ifelse( (p1_team_goals - p2_team_goals) < 0, 1,0 ),
         is_p1_powerplay = ifelse( (p1_team_players - p2_team_players) >0,1,0 ),
         is_p1_sh = ifelse( (p1_team_players - p2_team_players) < 0,1,0 )
         ) %>%
  
  # -- Remove NCAA matches
  filter( !(Home.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints'))) %>%
  filter( !(Away.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints')))

  # -- Add identifier of whether draw was {Defensive, Offensive, Neutral}

table(olympic_faceoff_data$Home.Team)

olympic_faceoff_data %>%
  filter(is_p1_powerplay == 1) %>%
  group_by(p1) %>%
  summarise(n())


olympic_model_fit <- glmer(formula = p1_won ~ is_p1_home + 
                             is_p1_up_goals + is_p1_down_goals +
                             is_p1_powerplay + is_p1_sh +
                             Period +
                             (1 | p1) +
                             (1 | p2), 
                         data = olympic_faceoff_data, 
                         family = binomial)

summary(olympic_model_fit)

p1_intercept = ranef(olympic_model_fit)$p1
p2_intercept = ranef(olympic_model_fit)$p2
random_int_df <- data.frame(#player1 = rownames(p1_intercept),
  intercept1 = p1_intercept$`(Intercept)`,
  player2 = rownames(p2_intercept),
  intercept2 = p2_intercept$`(Intercept)`)

random_int_df %>%
  arrange(desc(intercept1)) %>%
  View()


##### NWHL (Does not have the same players as in Olympics) #####
nwhl_data <- read.csv('data/hackathon_nwhl.csv', 
                      stringsAsFactors = FALSE)
nwhl_faceoff_data <- nwhl_data %>%
  filter(Event == 'Faceoff Win') 

sort(table(nwhl_faceoff_data$Player))
