### -- Plot face off win percentage for all players
library(dplyr)
library(ggplot2)
setwd("~/stathletes_hockey_hackathon_2021")
source('./src/plot_theme.R')
source('./src/gg_rink.R')
olympic_womens_data <- read.csv('./data/hackathon_womens.csv',
                                stringsAsFactors = FALSE)

olympic_womens_faceoff_data <- olympic_womens_data %>%
  filter(Event == 'Faceoff Win') %>%
  # -- Remove NCAA matches
  filter( !(Home.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints'))) %>%
  filter( !(Away.Team %in% c('Clarkson Golden Knights', 'St. Lawrence Saints')))




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

faceoff_data <- rbind(fo_winners, fo_losers)

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

faceoff_data$zone <- c(zone, zone_complement)

faceoff_data_aggregate <-
faceoff_data %>%
  group_by(p1) %>%
  summarise(tot_fo = n(),
            tot_won = sum(p1_won),
            tot_won_home = sum(ifelse((is_p1_home==1) & (p1_won==1),1,0)),
            tot_home_draws = sum(is_p1_home)) %>%
  mutate(tot_won_away = tot_won - tot_won_home,
         tot_away_draws = tot_fo - tot_home_draws,
         faceoff_percentage = tot_won / tot_fo) 

faceoff_data_aggregate %>%
  filter(tot_fo > 10) %>%
  arrange(desc(faceoff_percentage)) %>%
  View()





top_10_players <- faceoff_data_aggregate %>%
  filter(tot_fo > 10) %>%
  arrange(desc(faceoff_percentage)) %>%
  head(10) 

olympic_womens_faceoff_data_teams <-
olympic_womens_faceoff_data %>%
  select(Team, Player) %>% distinct() %>%
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



table(olympic_womens_faceoff_data$Team)
# Clarkson Golden Knights
#St. Lawrence Saints

# olympic_womens_faceoff_data %>%
#   filter(Team == 'St. Lawrence Saints') %>%
#   View()

top_10_players <-
top_10_players %>%
  left_join(olympic_womens_faceoff_data_teams,
            by= c('p1' = 'Player'))


top_10_players$name <- paste(top_10_players$p1,' (', top_10_players$Team, ')', sep='') 
top_10_players$name <- factor(top_10_players$name,
                              levels = top_10_players$name[order(top_10_players$faceoff_percentage)])



ggplot() + 
  geom_bar( data = top_10_players, 
            aes(x = faceoff_percentage*100, y= name, fill = tot_fo), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  geom_text(data = top_10_players,
            aes(x =faceoff_percentage*100 - 2, y= name,
                label =  paste('FO%: ', as.character(round(faceoff_percentage,3)*100))),
            position=position_dodge(0.8), 
            color = 'black',
            hjust = 1.1, 
            vjust = 0.5, 
            size = 4,
            fontface = 'bold') +
  scale_fill_gradient2(midpoint = mean(top_10_players$tot_fo),
                       low = "#00CCBB", mid = "#FFFBE3",
                       high = "#DD1717", space = "Lab" ) +
  labs(title = "Women's Top 10 Aggregate FOW%",
       x = "Faceoff Win (FOW) %",
       y='',
       fill = "Faceoffs Taken",
       caption = '@ptea_test'
       ) +
  plot_theme()

ggsave('faceoff_percentages_olympics.jpg',
       width=7.25, height=5,
       dpi = 400)

##### Faceoff Performance by Zone ####
faceoff_data_zone <-
  faceoff_data %>%
  filter(zone != 'N') %>%
  group_by(p1, zone) %>%
  summarise(tot_fo = n(),
            tot_won = sum(p1_won)) %>%
  mutate(faceoff_percentage = tot_won / tot_fo) 

plot_zone_data <- faceoff_data_zone %>%
  filter(p1 %in% top_10_players$p1) %>%
  left_join(top_10_players %>% select(p1,name))


zone.labs <- c("Defensive", "Offensive")
names(zone.labs) <- c("D", "O")

ggplot() + 
  geom_bar( data = plot_zone_data, 
            aes(x = faceoff_percentage*100, y= name, fill = tot_fo), 
            stat = "identity", position = 'dodge', width = 0.8, color = 'black') +
  facet_wrap(~zone, labeller = labeller(zone = zone.labs)) +
  geom_text(data = plot_zone_data,
            aes(x =faceoff_percentage*100 , y= name,
                label =  paste('FO%: ', as.character(round(faceoff_percentage,3)*100))),
            position=position_dodge(0.8), 
            color = 'black',
            hjust = 1.1, 
            vjust = 0.5, 
            size = 3.5,
            fontface = 'bold') +
  scale_fill_gradient2(midpoint = 30,
                       low = "#00CCBB", mid = "#FFFBE3",
                       high = "#DD1717", space = "Lab" ) +
  labs(title = "Women's Top 10 Aggregate FOW% by Zone",
       x = "Faceoff Win (FOW) %",
       y='',
       fill = "Faceoffs Taken",
       caption = '@ptea_test'
  ) +
  theme(strip.background =element_rect(fill="#f7e3c3"))+
  plot_theme()

ggsave('faceoff_by_zone_olympics.jpg',
       width=7.25, height=5,
       dpi = 400)




### GG Rink
unique(olympic_womens_faceoff_data$Y.Coordinate-42.5)
unique(olympic_womens_faceoff_data$X.Coordinate-100)

olympic_womens_faceoff_data <-
  olympic_womens_faceoff_data %>%
  mutate(coords_x = X.Coordinate-100,
         coords_y = Y.Coordinate-42.5
         ) %>%
  mutate(coords_y = case_when( coords_x < 0 & coords_y > 0 ~ -(coords_y),
                               coords_x < 0 & coords_y < 0 ~ abs(coords_y),
                               TRUE ~coords_y)) %>% 
  mutate(coords_x = abs(coords_x))
  


olympic_womens_faceoff_data %>%
ggplot(aes(x = coords_x, 
           y = coords_y))+
  gg_rink(side = 'right')+
  stat_density_2d(aes(fill = stat(nlevel)), geom = "polygon", show.legend = F, bins = 20, alpha = .5)+
  scale_fill_gradientn(colours = c('khaki1','pink1', 'red4'), trans = 'log10')+
  theme_bw()


+
  coord_flip()+
  facet_grid(~event_type)+
  theme(panel.grid = element_blank(),
        axis.line = element_line(),
        axis.ticks=  element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(hjust = .5))+
  labs(x = "",
       y = "",
       title = 'Maple Leafs 2019/20 Shot Heat Map')