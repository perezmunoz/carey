# Website for the an example in python kmeans 
# http://thespread.us/clustering.html
library(dplyr)

<<<<<<< Updated upstream
# Loading the data
path='./Data/'
=======
################################################
# Building active players tables: 
################################################
path='../Data/'
>>>>>>> Stashed changes
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
                                     FranchiseID=c('NYK','MEM','OKC','CHA','DEN','NOP','TOR','PHI','POR','IND','MIA','DAL','NJN','SAS',
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



################################################
# Question 1: similar active players
################################################

# Define variables to kmeans. Which influence the salary?
# experience
# age
# height
# weight
active_player_kmeans <- data.frame(model.matrix(playerID ~ shoots + position1, data = active_player_profile))
active_player_kmeans$X.Intercept. <- NULL
active_player_kmeans <- merge(subset(active_player_profile, select = c('playerID','weight','height','age','experience')), active_player_kmeans, by = 'row.names')
active_player_kmeans$Row.names <- NULL
active_player_kmeans <- merge(active_player_kmeans, active_salaries_final_recent, all = T,
                              by.x = 'playerID', by.y = 'PlayerID')
# active_player_kmeans$Salary[is.na(active_player_kmeans$Salary)] <- 0
# active_player_kmeans$Season[is.na(active_player_kmeans$Season)] <- 0

library(stats)
km <- kmeans(na.omit(subset(active_player_kmeans, select = c(2:14))), iter.max = 100, centers = 10, trace = 100)
#fitted(kmean_results)

#install.packages('fpc')
library(fpc)

d <- dist(na.omit(subset(active_player_kmeans,select=c(2:14))))
km_verification <- data.frame(k=c(2:31),diameter=numeric(length = 30))
for(i in 1:30){
  print(i)
  km <- kmeans(na.omit(subset(active_player_kmeans, select = c(2:14))), iter.max = 100, centers=i+1)
  km_verification$diameter[i] <- mean(cluster.stats(d, km$cluster)$diameter)
}
plot(x=km_verification$k, y=km_verification$diameter)

install.packages('randomForest')
library(randomForest)
fit <- randomForest(Kyphosis ~ Age + Number + Start,   data=)

################################################
# Cross dependence variables
################################################
library(GGally)

ggpairs(data = active_player_profile, columns = c('position1','position2','position3','shoots','weight',
                                                  'height','experience','age'))


################################################
# Searching for influential variables
################################################
# Does the position influence the salary?






