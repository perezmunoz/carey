library('plm')

setwd("C:\\Users\\Simon\\Documents\\NUS courses\\Hands-On with BA\\Guided Project 2\\Part3")
path='../Data/'

# Load data on players
data = read.csv(paste(path,'totals_final.csv',sep=''))
colnames(data)[4] <- 'FranchiseID' # rename 'Tm' attribute into 'FranchiseID' (more appropriate)

# Keep only data from last 10 years
# remove player with empty team attribute
# remove row with "TOT" in FranchiseID column, "TOT" means total, it doesn't correspond to a real team
data <- data[data[,'Season'] > 2003 & !is.na(data[,'FranchiseID']) & !data[,'FranchiseID'] == "" & !data[,"FranchiseID"] == "TOT",]

# Load salaries
salaries <- read.csv(paste(path,'salaries_final.csv',sep = ''))
salaries <- subset(salaries, Season %in% data[,'Season']) # Keep 10 past seasons
salaries <- subset(salaries, PlayerID %in% data[,'PlayerID']) # Remove fake players
salaries <- salaries[order(salaries$PlayerID, salaries$Season, -salaries$Salary),] # Sort by alphabetical order, then by season, then by highest salary (for player havong two salaries in one season)
salaries <- salaries[!duplicated(salaries[,c("PlayerID","Season")]),] # keep highest salary for player having more than one salary in one season

# Map Franchise ID with team name
franchise_id_team_name <- data.frame(Team=c('New York Knicks','Memphis Grizzlies','Oklahoma City Thunder',
                                            'Charlotte Bobcats','Denver Nuggets','New Orleans Pelicans','Toronto Raptors',
                                            'Philadelphia 76ers','Portland Trail Blazers','Indiana Pacers','Miami Heat',
                                            'Dallas Mavericks','Brooklyn Nets','San Antonio Spurs','Milwaukee Bucks',
                                            'Detroit Pistons','Atlanta Hawks','Houston Rockets','Chicago Bulls','Golden State Warriors',
                                            'Los Angeles Clippers','Washington Wizards','Boston Celtics','Phoenix Suns','Minnesota Timberwolves',
                                            'Utah Jazz','Charlotte Hornets','Los Angeles Lakers','Sacramento Kings','Cleveland Cavaliers',
                                            'Orlando Magic','New Jersey Nets','New Orleans Hornets','Charlotte Hornets','New Orleans/Oklahoma City',
                                            'Seattle SuperSonics'),
                                     FranchiseID=c('NYK','MEM','OKC','CHA','DEN','NOP','TOR','PHI','POR','IND','MIA','DAL','BRK','SAS',
                                                   'MIL','DET','ATL','HOU','CHI','GSW','LAC','WAS','BOS','PHO','MIN','UTA','CHA','LAL',
                                                   'SAC','CLE','ORL','NJN','NOH','CHO','NOK','SEA'))

# Add FranchiseID column to salary
salaries <- merge(franchise_id_team_name, salaries, by='Team')
salaries[,"Lg"] <- NULL

# Merge salaries and data into data
data <- merge(data,salaries, by = c("PlayerID","FranchiseID","Season"))
data <- data[!duplicated(data[,c("Season","PlayerID")]),]

# Load data on playoff
playoff = read.csv(paste(path,'playoff_statistics.csv',sep=''))
playoff$X <- NULL

# Add information on who was the champion (0 = not champion, 1 = champion)
champion <- subset(playoff,select=c("Season","Champion"))
colnames(champion) <- c("Season","FranchiseID")
champion$Champion <- 1
data <- merge(data,champion, all.x= TRUE)
data[,"Champion"][is.na(data[,"Champion"])] <- 0


# Add inforation on who was the runner-up
runnerUp <- subset(playoff,select=c("Season","Runner.Up"))
colnames(runnerUp) <- c("Season","FranchiseID")
runnerUp$RunnerUp <- 1
data <- merge(data,runnerUp,by = c("Season","FranchiseID"),all.x= TRUE)
data[,"RunnerUp"][is.na(data[,"RunnerUp"])] <- 0

# Top_Performer_Points
Top_Performer_Points <- subset(playoff,select=c("Season","Top_Performer_Points"))
colnames(Top_Performer_Points) <- c("Season","PlayerID")
Top_Performer_Points$TopPerformerPoints <- 1
data <- merge(data,Top_Performer_Points,by = c("Season","PlayerID"),all.x= TRUE)
data[,"TopPerformerPoints"][is.na(data[,"TopPerformerPoints"])] <- 0

# Top_Performer_Rebounds
Top_Performer_Rebounds <- subset(playoff,select=c("Season","Top_Performer_Rebounds"))
colnames(Top_Performer_Rebounds) <- c("Season","PlayerID")
Top_Performer_Rebounds$TopPerformerRebounds <- 1
data <- merge(data,Top_Performer_Rebounds,by = c("Season","PlayerID"),all.x= TRUE)
data[,"TopPerformerRebounds"][is.na(data[,"TopPerformerRebounds"])] <- 0

# Top_Performer_Assists
Top_Performer_Assists <- subset(playoff,select=c("Season","Top_Performer_Assists"))
colnames(Top_Performer_Assists) <- c("Season","PlayerID")
Top_Performer_Assists$TopPerformerAssists <- 1
data <- merge(data,Top_Performer_Assists,by = c("Season","PlayerID"),all.x= TRUE)
data[,"TopPerformerAssists"][is.na(data[,"TopPerformerAssists"])] <- 0

# Top_Performer_Win_Shares
Top_Performer_Win_Shares <- subset(playoff,select=c("Season","Top_Performer_Win_Shares"))
colnames(Top_Performer_Win_Shares) <- c("Season","PlayerID")
Top_Performer_Win_Shares$TopPerformerWinShares <- 1
data <- merge(data,Top_Performer_Win_Shares,by = c("Season","PlayerID"),all.x= TRUE)
data[,"TopPerformerWinShares"][is.na(data[,"TopPerformerWinShares"])] <- 0

# Add information about team
team_stat = read.csv(paste(path,'teams_statistics.csv',sep = ''))
team_stat$Rien <- NULL
team_stat$FranchiseID <- NULL
colnames(team_stat)[3] <- "FranchiseID"
team_stat <- team_stat[team_stat[,"Season"]>2003,] # keep 10 past seasons
team_stat <- subset(team_stat, select = c("Season","FranchiseID","WL_percentage","Finish","TopWS","Playoffs"))


# Indicate players that were TopWS for their team for each season
TopWS <- team_stat[,c("Season","FranchiseID","TopWS")]
TopWS$IsTopWS <- 1 
data <- merge(data,TopWS,by.x = c("Season","FranchiseID","PlayerID"), by.y = c("Season","FranchiseID","TopWS"),all.x=TRUE)
data[,"IsTopWS"][is.na(data[,"IsTopWS"])] <- 0

# For each player indicate the WL percentage of his team for each season
WL_percentage <- team_stat[,c("Season","FranchiseID","WL_percentage")]
data <- merge(data,WL_percentage)   # qui c'est qui gère le pâté en croûte ?!!!!!!!


# Panel
formula = Salary ~ Age+G+MP+FG+FTA+PF+IsTopWS+WL_percentage#+FGA+FTA+ORB+DRB+AST+STL+BLK+
# X3P+X3PA+X2P+X2PA+PTS tous cela ne sont pas des prédicteurs
analyse_data <- data
analyse_data <- analyse_data[seq(0, length(analyse_data[,1]), 2),]
result <- plm(formula, data = analyse_data, index=c("Season", "PlayerID"),effect = "time", model = "within")
summary(result)
 salaries[]

rate <- function(number){
  if(number < 0.001){return("***")}
  if(number < 0.01){return("**")}
  if(number < 0.05){return("*")}
  return("")
}

formulas <- c(Salary ~ Age,Salary ~ G,Salary ~ MP,Salary ~ FG,Salary ~ FTA,Salary ~ ORB,
              Salary ~ DRB,Salary ~ AST,Salary ~ STL,Salary ~ BLK,Salary ~ TOV,
              Salary ~ PF,Salary ~ FGA,Salary ~ FTA,Salary ~ X3P,Salary ~ X3PA,
              Salary ~ X2P,Salary ~ X2PA,Salary ~ PTS)

for(i in c(1:length(formulas))){
  result <- plm(formulas[[i]], data = analyse_data, index=c("Season", "PlayerID"),effect = "time", model = "within")
  result <- summary(result)
  coefficients <- coef(result)
  coefficients[1,"Pr(>|t|)"] <- paste(coefficients[1,"Pr(>|t|)"],rate(coefficients[1,"Pr(>|t|)"]))
  #print(paste(coefficients,rate(coefficients[1,"Pr(>|t|)"]),"\n"))
  print(coefficients)
  #print(rate(coefficients[1,"Pr(>|t|)"]))
}
summary(result)
sigma(result)
