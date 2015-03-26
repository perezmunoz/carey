# Loading the data
path='../Data/'
# Active players or not
is_active_players <- read.csv(paste(path,'active_players.csv',sep = ''))
#1
player_profile <- read.csv(paste(path,'player_profile.csv',sep = ''), header=FALSE)
names(player_profile) <- c('playerID','name','position1','position2','position3','shoots','weight','height','dob','birth_city','birth_state','experience','dod')
#2
salaries_final <- read.csv(paste(path,'salaries_final.csv',sep = ''))
#3
per_game_final <- read.csv(paste(path,'per_game_final.csv',sep = ''))
#4
playoff_statistics <- read.csv(paste(path,'playoff_statistics.csv',sep = ''), row.names=NULL, header = TRUE)
playoff_statistics$X <- NULL # Cleaning the last column
#5
totals_final <- read.csv(paste(path,'totals_final.csv',sep = ''))

################################################
# Building active players tables: 
################################################

# Active players
active_players <- subset(is_active_players, active == TRUE)$playerID #637
# Subset all tables to get only active players
#1
active_player_profile <- subset(player_profile, playerID %in% active_players) #634 players
active_player_profile$dob <- as.Date(active_player_profile$dob, format = '%m/%d/%y')
active_player_profile$dod <- NULL # players are still alive
active_player_profile$age <- floor(as.numeric(Sys.Date()-active_player_profile$dob)/365)
#2
active_salaries_final <- subset(salaries_final, PlayerID %in% active_players) #600 players
#2bis
active_salaries_final_recent <- subset((active_salaries_final %>% group_by(PlayerID) %>% filter(Season==max(Season))), select=c('PlayerID','Season','Salary'))
#3
active_per_game_final <- subset(per_game_final, PlayerID %in% active_players) #634 players
# Remember to use playoffs_statistics #4
#5
active_totals_final <- subset(totals_final, PlayerID %in% active_players) #634 players

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






