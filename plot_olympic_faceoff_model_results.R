# -- 1. Plot Player Random Interceps
# -- 2. Plot OFF skill vs. DEF faceoff Skill
# -- Maybe dumbell plot?
library(ggplot2)
library(dplyr)
setwd("~/stathletes_hockey_hackathon_2021")
source('./src/plot_theme.R')

olympic_womens_data <- read.csv('./data/hackathon_womens.csv',
                                stringsAsFactors = FALSE)

player_team_df <- 
  olympic_womens_data %>%
  select(Team, Player) %>% distinct()%>%
  mutate(Team = ifelse(Team == 'Olympic (Women) - Canada',
                       'CAN',
                       ifelse(Team == 'Olympic (Women) - Olympic Athletes from Russia',
                              'RUS', 
                              ifelse(Team == 'Olympic (Women) - Finland',
                                     'FIN', 
                                     ifelse(Team == 'Olympic (Women) - United States',
                                            'USA',
                                            ifelse(Team == 'St. Lawrence Saints','Saints',
                                                   'Knights'))))))

olympic_womens_faceoff_data <- olympic_womens_data %>%
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




##### Plotting Random Intercept #####
random_int_df <- read.csv('./data/plot_intercept.csv',
                      stringsAsFactors = FALSE)

random_int_df <-
random_int_df %>%
  left_join(player_team_df,
            by = c('player_name' = 'Player')) %>%
  filter(!Team %in% c('Knights')) %>%
  filter(fot >= 15)


random_int_df$player_name <- factor(random_int_df$player_name,
                              levels = random_int_df$player_name[order(random_int_df$intercept1)])



ggplot(data = random_int_df,
       aes(y = player_name, x = intercept1)) +
  geom_vline(xintercept=0, size=1.25) +
  geom_errorbar(aes(xmin=intercept1-1.96*sd.intercept, 
                    xmax=intercept1+1.96*sd.intercept), 
                width=0.5) + 
  geom_point(aes(color = Team),
             fill = 'black',
             size =5) +
  geom_rect(data=NULL,aes(xmin=-1.25,xmax=0,ymin=-Inf,ymax=Inf),
            fill="indianred", alpha = 0.01)+
  geom_rect(data=NULL,aes(xmin=0,xmax=1.25,ymin=-Inf,ymax=Inf),
            fill="green", alpha = 0.01) +
  annotate(geom="text", #x=172.5, 
           x= 0.45,
           y=1.2,
           hjust = 0,
           size = 5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Above Average",
           color="#058527") +
  annotate(geom="text", #x=172.5, 
           x= -1.2,
           y=17.5,
           hjust = 0,
           size = 5,
           family = 'Tahoma',
           fontface = 'bold',
           label="Below Average",
           color="red") +
  annotate("rect", xmin = 0.42, 
           xmax = 1.25, 
           ymin = 0,
           ymax = 2.5,
           alpha = .2) + 
  annotate("rect", xmin = -1.25, 
           xmax = -0.4, 
           ymin = 16,
           ymax = 18.5,
           alpha = .2) + 
  xlim(c(-1.25, 1.25))+
  scale_color_manual(values = c('CAN' = '#DA291C', 'FIN' = '#117A65',
                               'USA' = '#002F6C', 'RUS' = '#D4AC0D')) +
  labs(y = '',
       x = 'Random Intercept Value',
       title = "Women's 2018-19 Faceoff Skills",
       caption = '@ptea_test') +
  plot_theme()

ggsave('random_intercepts.jpg',
       width=7.25, height=5,
       dpi = 400)


##### Plot Defensive vs Offensive Faceoffs #####
plot_data <- read.csv('./data/faceoff_probs_glmm.csv',
                      stringsAsFactors = FALSE)



plot_data <- plot_data %>%
  left_join(player_team_df,
            by = c('player_name' = 'Player')) %>%
  filter(!Team %in% c('Knights')) %>% 
  left_join(olympic_faceoff_data %>%
              group_by(p1) %>%
              summarise(fot = n()),
            by = c('player_name' = 'p1'))# %>%
  #filter(fot >= 15)


##### Scatterplot of wins above average on OFF vs on DEF zone #####
ggplot() + 
  geom_point(data = plot_data %>% filter(fot >= 5),
             aes(x = p_win_O_aa,
                 y = p_win_D_aa, 
                 fill = Team),
             alpha = 0.15,
             shape = 21,
             color = 'black',
             size = 2.5
  )


##### Compare differences between OFF and DEF zones #####
canada <- plot_data %>%
  filter(fot >= 5) %>%
  filter(Team == 'CAN') %>%
  arrange(desc(p_win_draw_aa)) %>%
  head(5)
#
usa <- plot_data %>%
  filter(fot >= 5) %>%
  filter(Team == 'USA') %>%
  arrange(desc(p_win_draw_aa)) %>%
head(5)
#
russia <- plot_data %>%
  filter(fot >= 5) %>%
  filter(Team == 'RUS') %>%
  arrange(desc(p_win_draw_aa)) %>%
head(5)
#
finland <- plot_data %>%
  filter(fot >= 10) %>%
  filter(Team == 'FIN') %>%
  arrange(desc(p_win_draw_aa)) %>%
head(5)
#
all_dat <- rbind(canada, usa, finland, russia)


all_dat$player_name <- factor(all_dat$player_name,
                              levels = all_dat$player_name[order(all_dat$p_win_draw_aa)])
all_dat$Team <- factor(all_dat$Team,
                              levels =c('CAN', 'USA', 'FIN', 'RUS'))


colors <- c("Defense" = "blue", "Offense" = "red")


ggplot(data = all_dat) +
  
  geom_vline(xintercept = 0, size = 0.8,
             linetype="dashed", color = 'black') +

  
  # -- Draws line between points
  geom_segment(
    aes(x = p_win_D_aa,
        xend = p_win_O_aa,
        y = player_name,
        yend = player_name),
    size = 0.75, color = "black"
    
    
  ) + 
  geom_point(
    aes(x = p_win_D_aa, 
        y = player_name,
        fill = 'Defense'), 
    #fill = 'blue',

    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size =3.5) + 
  geom_point(
    aes(x = p_win_O_aa, 
        y = player_name,
        fill ='Offense'), 
    #fill = 'red',
    
    shape = 21, # This adds black outline to points (why can't I use fill/color???)
    size =3.5) +
  geom_rect(data=NULL,aes(xmin=-0.18,xmax=0,ymin=-Inf,ymax=Inf),
            fill="indianred", alpha = 0.05)+
   geom_rect(data=NULL,aes(xmin=0.18,xmax=0,ymin=-Inf,ymax=Inf),
             fill="green", alpha = 0.05)+
  scale_fill_manual(values = colors) +
  facet_grid(.~ Team) +
  scale_x_continuous(breaks = c(-0.1, 0, 0.1)) +
  labs(title = 'Zone Faceoff Skills',
       y = '',
       x = 'Above Average FOW Probabilty',
       caption = '@ptea_test',
       fill = "Zone") +
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  theme(strip.text = element_text(colour = 'black',face = 'bold'))+
  plot_theme()

ggsave('off_vs_def_slope.jpg',
       width=7.25, height=5,
       dpi = 400)

