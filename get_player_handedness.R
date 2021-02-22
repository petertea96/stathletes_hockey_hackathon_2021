##### Player Handedness #####
library(dplyr)
library(lme4)
#library(chron)
setwd("C:/Users/Peter/Documents/stathletes_hockey_hackathon_2021")

##### Step 1 - Process Women's Olympic hockey data #####


olympic_data <- read.csv('data/hackathon_womens.csv',
                         stringsAsFactors = FALSE)


olympic_womens_faceoff_data <- olympic_data %>%
  filter(Event == 'Faceoff Win') 

unique(c(olympic_womens_faceoff_data$Player, olympic_womens_faceoff_data$Player.2))


player_name <- c(
  "Emily Clark", "Marie-Philip Poulin","Olga Sosina",
  "Brianne Jenner", "Yevgenia Dyupina", "Lyudmila Belyakova",
  "Haley Irwin", "Jennifer Wakefield", "Yelena Dergachyova", 
  "Melodie Daoust", "Blayre Turnbull", "Anna Shokhina", 
  "Valeria Pavlova", "Linda Valimaki", "Riikka Sallinen",    
  "Sara Sakkinen", "Tanja Niskanen", "Susanna Tapani",     
  "Petra Nieminen", "Brianna Decker", "Kelly Pannek",       
  "Dani Cameranesi", "Hannah Brandt", "Gigi Marvin",       
  "Hilary Knight", "Bailey Bram", "Jillian Saulnier",   
  "Meghan Duggan", "Kayla Nielsen", "Michala Pejzlova",   
  "Nadine Edney", "Tia Stoddard", "Cassidy Vinkle",     
  "Jessica Poirier", "Charlea Pedersen", "Taylar Cianfarano",  
  "Justine Reyes", "Maggie McLaughlin", "Kalie Grant",        
  "Hayley Scamurra", "Victoria Bach", "Alex Carpenter",     
  "Dana Trivigno", "Jesse Compher", "Sarah Nurse",        
  "Sydney Brodt", "Noora Tulus", "Loren Gabel",        
  "Minnamari Tuominen", "Fanuza Kadirova", "Viktoria Kulishova",
  "Annina Rajahuhta", "Laura Stacey", "Saila Saari",
  "Amanda Kessel", "Kendall Coyne Schofield", "Jocelyne Lamoureux", 
  "Diana Kanayeva", "Yekaterina Smolina", "Natalie Spooner",
  "Jocelyne Larocque", "Kelly Mariani", "Rachael Smith",
  "Steph Keryluk", "Viivi Vainikka", "Michelle Karvinen"  
  
)



# Manually inputted from eliteprospects.com
player_handedness <- c('L','L','R',
                       'R', 'R', 'L',
                       'L', 'R', 'L',
                       'L', 'R', 'L',
                       'L', 'L', 'R',
                       'L', 'L', 'L',
                       'L', 'R', 'R',
                       'L', 'R', 'R',
                       'R', 'L', 'L',
                       'R', 'L', 'L',
                       'L', 'L', 'L',
                       'R', 'L', 'L',
                       'R', 'L', 'R',
                       'L', 'L', 'L',
                       'R', 'R', 'L',
                       'R', 'R', 'L',
                       
                       'R', 'L', "L", 
                       "L", "R", "L", 
                       'R', 'L', 'R', 
                       'L', 'R', 'R', 
                       'L', 'R', 'R', 
                       'R', 'L', 'L'
                       )

player_handedness_df <- data.frame(player_name,
                                   player_handedness)

write.csv('./data/player_handednes.csv',
          x = player_handedness_df,
          row.names = FALSE)


