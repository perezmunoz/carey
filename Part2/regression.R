# Loading the data
path='./Data/'
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

# Active players in 2011 with salaries
#1
salaries_temp_2011 <- subset(salaries_final, Season == 2011) #468 players receiving a salary in 2011
salaries_2011 <- subset(salaries_temp_2011, select=c('PlayerID','Team','Salary'))
rownames(salaries_2011) <- NULL
# List of active players in 2011
active_players_2011 <- salaries_2011$PlayerID #468
# Subset all tables to get only active players
#2
player_profile_2011 <- subset(player_profile, playerID %in% active_players_2011) #634 players
player_profile_2011$dob <- as.Date(player_profile_2011$dob, format = '%m/%d/%y')
player_profile_2011$dod <- NULL # players are still alive
player_profile_2011$age <- floor(as.numeric(Sys.Date() - player_profile_2011$dob)/365)
player_profile_salaries <- merge(salaries_2011, player_profile_2011, by.x = 'PlayerID', by.y = 'playerID')
#3
per_game_2011 <- subset(per_game_final, Season == 2011) #634 players
per_game_salaries <- merge(salaries_2011, per_game_2011)
# Remember to use playoffs_statistics 
#4
playoff_statistics_2011 <- subset(playoff_statistics, Year == 2011)
#5
totals_2011 <- subset(totals_final, Season == 2011) #634 players
totals_salaries <- merge(salaries_2011,totals_2011)
#6
rm('salaries_final')
rm('salaries_temp_2011')
rm('player_profile')
rm('player_profile_2011')
rm('per_game_final')
rm('per_game_2011')
rm('playoff_statistics')
rm('totals_final')
rm('totals_2011')
rm('active_players_2011')

################################################
# Regression: 
################################################


reg1 <-lm(Salary ~ PTS, data = per_game_salaries)
summary(reg1)

reg2 <- lm(Salary ~ weight*height, data = player_profile_salaries)
summary(reg2)

ggpairs(data = per_game_salaries, columns = c('Salary','Age','Pos','G','GS','MP','FG','FGA','FG.','3P','3PA','3P.','2P','2PA','2P.','eFG.','FT','FTA','FT.','ORB','DRB','TRB','AST','STL','BLK','TOV','PF','PTS'))
