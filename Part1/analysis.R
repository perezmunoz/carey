# Website for the an example in python kmeans 
# http://thespread.us/clustering.html
# https://github.com/Rapporter/templates/blob/master/NBA.tpl
library(dplyr)
library(stats)
library(cluster)
library(directlabels)
library(ggplot2)
library(fpc)
library(stats)

################################################
# Building active players tables: 
################################################
path='../Data/'
# Active players or not
is_active_players <- read.csv(paste(path,'active_players.csv',sep = ''))
#1
player_profile <- read.csv(paste(path,'player_profile.csv',sep = ''), header=FALSE)
names(player_profile) <- c('PlayerID','name','position1','position2','position3','shoots','weight','height','dob','birth_city','birth_state','experience','dod')
#2
salaries_final <- read.csv(paste(path,'salaries_final.csv',sep = ''))
#3
per_game_final <- read.csv(paste(path,'per_game_final.csv',sep = ''))
#4
playoff_statistics <- read.csv(paste(path,'playoff_statistics.csv',sep = ''), row.names=NULL, header = TRUE)
playoff_statistics$X <- NULL # Cleaning the last column
#5
totals_final <- read.csv(paste(path,'totals_final.csv',sep = ''))
#6
teams_basic_info <- read.csv(paste(path,'teams_basic_info.csv', sep = ''))

################################################
# Building active players tables for each case
################################################

# Active players
active_players <- subset(is_active_players, active == TRUE)$playerID #637
# Subset all tables to get only active players
#1
active_player_profile <- subset(player_profile, PlayerID %in% active_players) #634 players
active_player_profile$dob <- as.Date(active_player_profile$dob, format = '%m/%d/%y')
active_player_profile$dod <- NULL # players are still alive
# Irrelevants columns
active_player_profile$position1 <- NULL
active_player_profile$position2 <- NULL
active_player_profile$position3 <- NULL
active_player_profile$age <- floor(as.numeric(Sys.Date()-active_player_profile$dob)/365)
#2
active_salaries <- subset(salaries_final, PlayerID %in% active_players) #600 players
#2bis
active_salaries <- subset((active_salaries %>%
                                   group_by(PlayerID) %>%
                                   filter(Season == max(Season))),
                                select=c('PlayerID','Season','Team','Salary'))
length(unique(active_salaries$PlayerID)) # 600 != 613 players
# Duplicated players
salaries_dupliacted_id <- subset(active_salaries, duplicated(PlayerID))$PlayerID
# Extract duplicates
df_duplicates_salaries <- subset(active_salaries, PlayerID %in% salaries_dupliacted_id)
# The issue is that some players change the team during the season, so their Salary changes too
# We keep the highest salary
active_salaries <- active_salaries %>% group_by(PlayerID) %>% filter(Salary == max(Salary)) # no duplicates
# Add team's franchise id to facilitate merge with totals statistics
franchise_id_team_name <- data.frame(Team=c('New York Knicks','Memphis Grizzlies','Oklahoma City Thunder',
                                            'Charlotte Bobcats','Denver Nuggets','New Orleans Pelicans','Toronto Raptors',
                                            'Philadelphia 76ers','Portland Trail Blazers','Indiana Pacers','Miami Heat',
                                            'Dallas Mavericks','Brooklyn Nets','San Antonio Spurs','Milwaukee Bucks',
                                            'Detroit Pistons','Atlanta Hawks','Houston Rockets','Chicago Bulls','Golden State Warriors',
                                            'Los Angeles Clippers','Washington Wizards','Boston Celtics','Phoenix Suns','Minnesota Timberwolves',
                                            'Utah Jazz','Charlotte Hornets','Los Angeles Lakers','Sacramento Kings','Cleveland Cavaliers',
                                            'Orlando Magic','New Jersey Nets','New Orleans Hornets'),
                                     FranchiseID=c('NYK','MEM','OKC','CHA','DEN','NOP','TOR','PHI','POR','IND','MIA','DAL','BRK','SAS',
                                                  'MIL','DET','ATL','HOU','CHI','GSW','LAC','WAS','BOS','PHO','MIN','UTA','CHA','LAL',
                                                  'SAC','CLE','ORL','NJN','NOH'))

active_salaries <- subset(merge(active_salaries, franchise_id_team_name, by='Team'), select=c('PlayerID','Season','Team','FranchiseID','Salary'))
#3
active_per_game_final <- subset(per_game_final, PlayerID %in% active_players) #634 players
names(active_per_game_final) <- c("PlayerID","Season","Age","FranchiseID","Lg","Pos","G","GS","MP","FG","FGA","FGptg","X3P",
                                  "X3PA","X3Pptg","X2P","X2PA","X2Pptg","FT","FTA","FTptg","ORB","DRB","TRB","AST",
                                  "STL","BLK","TOV","PF","PTS")
# Filling the NA values with 0 for statistics
# Verification is NA values in the first 4 columns
table(is.na(active_per_game_final[,c(1:6)])) # no NA values
active_per_game_final[is.na(active_per_game_final)] <- 0
# Remember to use playoffs_statistics #4
#5
active_totals_final <- subset(totals_final, PlayerID %in% active_players) #634 players
names(active_totals_final) <- c("PlayerID","Season","Age","FranchiseID","Lg","Pos","G","GS","MP","FG","FGA","FGptg","X3P",
                                  "X3PA","X3Pptg","X2P","X2PA","X2Pptg","eFGptg","FT","FTA","FTptg","ORB","DRB","TRB",
                                  "AST","STL","BLK","TOV","PF","PTS")

################################################
# Building the final active players table
################################################

# There are 634 active players in active_player_profile
# There are 600 salaries in active_salaries

# Are the 600 players in active_salaries contained in active_player_profile
table(active_salaries$PlayerID %in% active_player_profile$PlayerID) # yes

# contains the PlayerID of players without information about their salary
player_wo_salary <- subset(active_player_profile,
                           !(active_player_profile$PlayerID %in% active_salaries$PlayerID))

# Active players with information about their salary
player_information_inter <- merge(active_player_profile, active_salaries,
                                  by='PlayerID',
                                  all.y=T)

# Are the all the team names contained in player_information_inter contained in active_totals_final
table(unique(player_information_inter$FranchiseID) %in% unique(active_totals_final$Tm)) # yes

# Active players with total statistics during the last season we have Salary information
player_information <- merge(player_information_inter, active_totals_final,
                             by=c('PlayerID','FranchiseID','Season'),
                             all.x=T)

# 69 players do not have matched the active_totals_final with player_information_inter
# Issue: salary for Season 2013-14 but no totals only till 2012-13
nrow(subset(player_information, is.na(X3P) & is.na(BLK) & is.na(ORB)))
# Solution: suppress these lines. Get around 90% of the data. Sufficient for the clustering
player_information <- player_information[!(is.na(player_information$X3P) &
                                            is.na(player_information$BLK) &
                                            is.na(player_information$ORB)),]
# Delete duplicate variable
player_information$Age <- NULL
# Change NA values to 0
player_information[,c(17:41)][is.na(player_information[,c(17:41)])] <- 0

# rearrange columns in player_information
# to have categorical (<11 column) vs numerical (>11 column) variables
# 11 is Season, 12 is Salary. From 13 is the rest
player_information <- player_information[c(1,2,4,5,8,9,10,13,15,16,3,14,6,7,11,12,17:41)]

################################################
# kmeans
################################################

# Define variables to kmeans. Which influence the salary?
# Experience
# Age
# Height
# Weight
# Personnal statistics on a Season
# With center=T and scale=T, data is normalized
km <- kmeans(scale(player_information[,c(12:41)], center = TRUE, scale = FALSE),
             centers = 5,
             iter.max = 100,
             trace=100)
# Question b.
# What the attributes selected to understand Salary?
# From analysis, we can suppress redundant variables that bring nothing to the interpretation
useless_variables <- c('FGptg','X3Pptg','X2Pptg','eFGptg','FRptg')
player_info <- subset(player_information,
                      select = !(names(player_information) %in% useless_variables))

# kmeans with refine data
km_info <- kmeans(scale(player_info[,c(12:37)], center = TRUE, scale = FALSE),
             centers = 5,
             iter.max = 100,
             trace=100)

# Question c.
# How many 'k' do you use?
# Find the optimum k
d <- dist(player_info[,c(12:37)])
# Run the test for 30 centroids
km_info_verif <- data.frame(k = c(2:16),
                            diameter = numeric(length = 15))
for(i in 1:15){
  print(i)
  set.seed(1234)
  km_temp <- kmeans(player_info[,c(12:37)],
                    centers = i+1,
                    iter.max = 100,
                    trace = 100)
  km_info_verif$diameter[i] <- mean(fpc::cluster.stats(d, km_temp$cluster)$diameter)
}
plot(x = km_info_verif$k,
     y = km_info_verif$diameter,
     type = 'b')

km_final <- kmeans(scale(player_info[,c(12:37)], center = TRUE, scale = FALSE),
                   centers = 5,
                   iter.max = 100,
                   trace=100)

# Question d.
# Can you get different results by using random centers? Does this affects your result?
# Find (k, attributes) so that output is stable
# Everything converges. Find the maximum slop in the plot: find the k.
# Or analyze the data and find what is better to interpret



