library(dplyr)
library(lme4)

setwd("C:/Users/Peter/Documents/stathletes_hockey_hackathon_2021")

##### Step 1 - Process Women's Olympic hockey data #####


olympic_data <- read.csv('data/hackathon_womens.csv',
                         stringsAsFactors = FALSE)

### What matches was observed?
# unique(olympic_data[,c('game_date','Home.Team','Away.Team')]) %>%
#   View()

olympic_womens_faceoff_data <- olympic_data %>%
  filter(Event == 'Faceoff Win') %>%
  mutate(is_home =  ifelse(Home.Team == Team, 1,0))

### Look at which teams played against one another
# unique(olympic_data[,c('Home.Team', 'Away.Team')]) %>%
#   View()

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
         -X.Coordinate.2, -Y.Coordinate.2, -is_home #,
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


# -- Add identifier of whether draw was {Defensive, Offensive, Neutral}
### Faceoff Zones
zone = ifelse( olympic_womens_faceoff_data$X.Coordinate < 75, 'D',
               ifelse( (olympic_womens_faceoff_data$X.Coordinate > 75) & (olympic_womens_faceoff_data$X.Coordinate < 125), 'N',
                       ifelse(olympic_womens_faceoff_data$X.Coordinate > 125, 'O', NA))
               
)

#head(zone,20)

zone_complement <- ifelse(zone == 'D', 'O',
                          ifelse(zone == 'O', 'D',
                                 ifelse(zone == 'N', zone, NA)))

olympic_faceoff_data$zone <- c(zone, zone_complement)

##### Add Faceoff Side  #####
faceoff_side <- ifelse(olympic_womens_faceoff_data$Y.Coordinate > 42.5, 'R', 'L')
# 
# faceoff_side <- ifelse( (olympic_womens_faceoff_data$is_home == 1) & (olympic_womens_faceoff_data$Period %% 2 !=0) & (olympic_womens_faceoff_data$Y.Coordinate > 42.5), 
#                            'R', 
#                            ifelse((olympic_womens_faceoff_data$is_home == 1) & (olympic_womens_faceoff_data$Period %% 2 ==0) & (olympic_womens_faceoff_data$Y.Coordinate < 42.5),
#                                   'R', 
#                                   ifelse((olympic_womens_faceoff_data$is_home == 0) & (olympic_womens_faceoff_data$Period %% 2 !=0) & (olympic_womens_faceoff_data$Y.Coordinate < 42.5),
#                                          'R', 
#                                          ifelse((olympic_womens_faceoff_data$is_home == 0) & (olympic_womens_faceoff_data$Period %% 2 ==0) & (olympic_womens_faceoff_data$Y.Coordinate > 42.5),
#                                                 'R', 'L'))))

# faceoff_side <- ifelse(faceoff_side == 'R', 'L', 'R')
faceoff_side_complement <- ifelse(faceoff_side == 'R', 'L', 'R')
olympic_faceoff_data$faceoff_side <- c(faceoff_side, faceoff_side_complement)

olympic_faceoff_data$p2_faceoff_side <- c(faceoff_side_complement, faceoff_side)

olympic_faceoff_data <- cbind(olympic_faceoff_data, 
                              nnet::class.ind(olympic_faceoff_data$zone),
                              nnet::class.ind(olympic_faceoff_data$faceoff_side))

##### Add strong side / Weak side indicator #####
player_handedness_df <- read.csv('./data/player_handednes.csv')

olympic_faceoff_data <- 
olympic_faceoff_data %>%
  left_join(player_handedness_df, by = c('p1' = 'player_name')) %>%
  mutate(is_strong_side = ifelse(faceoff_side == player_handedness,
                                 1,0)) %>%
  left_join(player_handedness_df, by = c('p2' = 'player_name')) %>%
  rename(p2_handedness = player_handedness.y) %>%
  mutate(is_p2_strong_side = ifelse(p2_faceoff_side == p2_handedness,
                                    1,0))


# -- What are the faceoff coordinates?
# table(olympic_faceoff_data$X.Coordinate)
# 
# unique(olympic_faceoff_data[,c('X.Coordinate', 'Y.Coordinate')]) %>%
#   arrange(X.Coordinate, Y.Coordinate)
# 
# table(olympic_faceoff_data$Home.Team)

olympic_faceoff_data <- olympic_faceoff_data %>%
  mutate(is_p1_up_goals = ifelse( (p1_team_goals - p2_team_goals) > 0, 1,0 ),
         is_p1_down_goals = ifelse( (p1_team_goals - p2_team_goals) < 0, 1,0 ),
         is_p1_powerplay = ifelse( (p1_team_players - p2_team_players) >0,1,0 ),
         is_p1_sh = ifelse( (p1_team_players - p2_team_players) < 0,1,0 ),
         is_p1_pp1 =  ifelse( (p1_team_players - p2_team_players) == 1,1,0 ),
         is_p1_pp2 =  ifelse( (p1_team_players - p2_team_players) == 2,1,0 ),
         is_p1_sh1 = ifelse( (p1_team_players - p2_team_players) == -1,1,0 ),
         is_p1_sh2 = ifelse( (p1_team_players - p2_team_players) == -2,1,0 )
         ) %>%
  
  # -- Remove NCAA matches
  filter( !(Home.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints'))) %>%
  filter( !(Away.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints')))

olympic_faceoff_data %>%
  filter(is_p1_pp2 ==1) %>%
  View()
# olympic_faceoff_data %>%
#   filter(is_p1_powerplay == 1) %>%
#   View()


# -- Add time remaining as a feature?
#str(olympic_faceoff_data)

# period_time = strptime(olympic_faceoff_data$Clock, 
#          format = "%M:%S")
# period_time_left = lubridate::minute(period_time) + lubridate::second(period_time)/60
# 
# olympic_faceoff_data$period_time_left <- period_time_left
# olympic_faceoff_data$percent_period_left <- olympic_faceoff_data$period_time_left/20

#quantile(olympic_faceoff_data$percent_period_left)
#quantile(olympic_faceoff_data$period_time_left)

# olympic_faceoff_data %>%
#   filter(is_p1_powerplay == 1) %>%
#   group_by(p1) %>%
#   summarise(n())

# -- Less FAceoffs are taken in 3rd period
# olympic_faceoff_data %>%
#   filter(p1_won == 1) %>%
#   group_by(Period) %>%
#   summarise(n())
# 
# # Most faceoffs are won in the O-Zone
# olympic_faceoff_data %>%
#   filter(p1_won == 1) %>%
#   group_by(zone) %>%
#   summarise(n())

# Only include players with more than 5 FOs taken?
players_of_interest <- olympic_faceoff_data %>%
  group_by(p1) %>%
  summarise(fot = n()) %>%
  arrange(fot) %>%
  filter(fot >= 5) %>%
  .$p1

olympic_faceoff_data_to_fit <- olympic_faceoff_data %>%
  filter(p1 %in% players_of_interest)

# Variables that seem to have no significant fixed effect:
# is_p1_up_goals, is_p1_down_goals, Period, percent_period_left
# Specicying 5on3 and 5on4 offers marginal model improvement (AIC reduces by 2 units)
olympic_model_fit <- glmer(formula = p1_won ~ is_p1_home + 
                             #is_p1_powerplay + is_p1_sh +
                             # percent_period_left +
                             is_p1_pp1 + is_p1_pp2 +
                             is_p1_sh1 + is_p1_sh2 +
                             #L +
                             is_strong_side +
                             is_p2_strong_side +
                             #D + O + 
                             (1 | p1) +
                             # Add uncorrelated random slopes
                             # https://stats.stackexchange.com/questions/31569/questions-about-how-random-effects-are-specified-in-lmer
                             #(D -1 | p1) +
                             (1 | p2), 
                         #data = olympic_faceoff_data_to_fit, 
                         data =  olympic_faceoff_data,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                         family = binomial)


summary(olympic_model_fit)
# VarCorr(olympic_model_fit)


predict_df <- data.frame(
  p1 = 'Brianne Jenner',
  p2 = 'Melodie Daoust',
  is_p1_home = 1,
  is_p1_pp1 = 1,
  is_p1_pp2 = 0,
  is_p1_sh1 = 0,
  is_p1_sh2 = 0,
  is_strong_side = 1,
  is_p2_strong_side = 1,
  stringsAsFactors = FALSE
)

predict(olympic_model_fit,
        newdata=predict_df,
        type="response",
        allow.new.levels=TRUE)


# Get standard deviations of BLUPs
randoms<-ranef(olympic_model_fit, postVar = TRUE)
qq <- attr(ranef(olympic_model_fit, postVar = TRUE)[[1]], "postVar")
length(qq)


p1_intercept = ranef(olympic_model_fit)$p1
p2_intercept = ranef(olympic_model_fit)$p2

random_int_df <- data.frame(
  player_name = rownames(p1_intercept),
  intercept1 = p1_intercept$`(Intercept)`,
  sd.intercept=sqrt(qq[,,1:length(qq)]),
  #D_zone_effect = p1_intercept$D,
  #O_zone_effect = p1_intercept$O,
  player2 = rownames(p2_intercept),
  intercept2 = p2_intercept$`(Intercept)`
  )



random_int_df <-
random_int_df %>% left_join( olympic_faceoff_data %>%
                               group_by(p1) %>%
                               summarise(fot = n()),
                                         by = c('player_name' = 'p1')
  
)

write.csv('./data/plot_intercept.csv', 
          x=random_int_df,
          row.names = FALSE)

##### Fitting Random slope #####
olympic_model_fit <- glmer(formula = p1_won ~ is_p1_home + 
                             is_p1_powerplay + is_p1_sh +
                             # percent_period_left +
                             #is_p1_pp1 + is_p1_pp2 +
                             #is_p1_sh1 + is_p1_sh2 +
                             #L +
                             
                             D + O + 
                             is_strong_side +
                             (1 + D + O | p1) +
                             (1 | p2), 
                           #data = olympic_faceoff_data_to_fit, 
                           data =  olympic_faceoff_data,
                           #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)),
                           family = binomial)



summary(olympic_model_fit)

##### Should we convert intercept scale to probability scale ? #####
logit_summary <- summary(olympic_model_fit)$coef
expit <- function(x) exp(x)/(1+exp(x))

summary_table <- data.frame(
odds_ratio = exp(logit_summary[,"Estimate"]),
absolute_effect = ((expit(logit_summary[,"Estimate"] + logit_summary[1,"Estimate"])) - expit(logit_summary[1,"Estimate"]))*100,
pvalue = logit_summary[,ncol(logit_summary)]
)

summary_table

fixef(olympic_model_fit)

# Prob winning a draw above average
plot_data <- data.frame(
 player_name = rownames(ranef(olympic_model_fit)$p1),
 p_win_draw_aa = expit(ranef(olympic_model_fit)$p1[,1]+fixef(olympic_model_fit)[1]) -  expit(fixef(olympic_model_fit)[1]),
 p_win_D_aa = expit(ranef(olympic_model_fit)$p1[,1]+fixef(olympic_model_fit)[1] + ranef(olympic_model_fit)$p1[,2] + fixef(olympic_model_fit)[5]) -  expit(fixef(olympic_model_fit)[1] + fixef(olympic_model_fit)[5]),
 p_win_O_aa = expit(ranef(olympic_model_fit)$p1[,1]+fixef(olympic_model_fit)[1] + ranef(olympic_model_fit)$p1[,3] + fixef(olympic_model_fit)[6]) -  expit(fixef(olympic_model_fit)[1] + fixef(olympic_model_fit)[6])#,
 #p_win_D_aa_v2 = expit(ranef(olympic_model_fit)$p1[,2] + fixef(olympic_model_fit)[5]) -  expit(fixef(olympic_model_fit)[5]),
 #p_win_O_aa_v2 = expit(ranef(olympic_model_fit)$p1[,3] + fixef(olympic_model_fit)[6]) -  expit(fixef(olympic_model_fit)[6])
 
)

# plot_data %>%
#   arrange(desc(p_win_O_aa_v2)) %>%
#   View()
# 
plot_data %>%
  arrange(desc(p_win_O_aa)) %>%
  View()

plot_data %>%
  arrange(desc(p_win_D_aa)) %>%
  View()

plot_data %>%
  arrange(desc(p_win_draw_aa)) %>%
  View()


write.csv('./data/faceoff_probs_glmm.csv', x = plot_data,
          row.names = FALSE)

##### NWHL (Does not have the same players as in Olympics) #####
# nwhl_data <- read.csv('data/hackathon_nwhl.csv', 
#                       stringsAsFactors = FALSE)
# nwhl_faceoff_data <- nwhl_data %>%
#   filter(Event == 'Faceoff Win') 
# 
# sort(table(nwhl_faceoff_data$Player))
