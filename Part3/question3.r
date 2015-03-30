library('plm')
library('car')

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

# Equilibrate the data : throw player that don't have enough record (< 8 seasons)
keep <- function(player_id){
  number <- length(data[data[,"PlayerID"]==player_id,1])
  if(number<8){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

data <- data[sapply(data[,"PlayerID"],keep),]

# Add weight and heigth for each player
physical_info = subset(read.csv(paste(path,'player_profile.csv',sep = '')),select= c("PlayerID","Weight","Height"))
data <- merge(data,physical_info)

# Sort data by alphebtical order on player and by ascending season
data <- data[order(data[,"PlayerID"], data[,"Season"]),]

# Panel regression
formula = Salary ~ Age+PTS+X3P+AST+TopPerformerWinShares+G
fixed <- plm(formula, data = data, index=c("Season", "PlayerID"),effect = "time", model = "within")
summary(fixed)

random <- plm(formula, data = data, index=c("Season", "PlayerID"),effect = "time", model = "random")
phtest(fixed,random)


# Try predict salary from year n with data from year n-1
shifted_data <- data
shifted_data$Bool <- TRUE
for(i in c(1:(length(data[,1])-1))){
  shifted_data[i,"Salary"] <- data[i+1,"Salary"]
  if(data[i,"PlayerID"]!=data[i+1,"PlayerID"]){
    shifted_data[i,"Bool"] <- FALSE
  }
}
shifted_data[length(data[,1]),"Bool"] <- FALSE
shifted_data <- shifted_data[shifted_data[,"Bool"],]
shifted_data$Bool <- NULL

# Panel --------------------- Here you can add other predictors
formula = Salary ~ Age+PTS+PF+RunnerUp+G
# X3P+X3PA+X2P+X2PA+PTS tous cela ne sont pas des prédicteurs
fixedS <- plm(formula, data = shifted_data, index=c("Season", "PlayerID"),effect = "time", model = "within")
random <- plm(formula, data = shifted_data, index=c("Season", "PlayerID"),effect = "time", model = "random")
phtestS(fixedS,randomS)
summary(fixedS)



# Try display
# nbr_affichage <- 1
# player_affichage <- unique(data[,'PlayerID'])[sample(c(1:202),nbr_affichage)]
# reduced_data <- data[data[,"PlayerID"] %in% player_affichage,]
# 
# pdf(file='plot.pdf', width=100, height=100)
# par(mfrow=c(10,10), mar=c(0,0,0,0))
# yhat <- fixed$fitted
# scatterplot(Salary ~ Season, reduced_data, groups = reduced_data$PlayerID, group.by=TRUE,boxplots=TRUE,xlab="Season",ylab="Salary",smooth=F)
# dev.off()
# 
# 
# 
# min_salary <- min(data[,'Salary'])
# max_salary <- max(data[,'Salary'])
# min_season <- 2004
# max_season <- 2014
# 
# 
# n <- 10
# player_affichage <- player_affichage[sample(1:202,n)]
# colors <- rgb(c(seq(0, 1, 1 / 2*n), rep(1, n)), c(abs(seq(0, 1 - 1/n, 1/ n)), 1, abs(seq(1 - 1/n, 0, -1 /n))), c(rep(1, n), abs(seq(1, 0, -1 / n))))
# plot(seq(min_season,max_season,length=10),seq(min_salary,max_salary,length = 10),col="white")
# i <- 0
# coef = coef
# for(player in player_affichage){
#   i <- i+2
#   seasons <- data[data[,"PlayerID"]==player,"Season"]
#   salary <- data[data[,"PlayerID"]==player,"Salary"]
#   points(seasons,salary,col=colors[i],pch=i)
#   
# }

# rate <- function(number){
#   if(number < 0.001){return("***")}
#   if(number < 0.01){return("**")}
#   if(number < 0.05){return("*")}
#   return("")
# }
# 
# formulas <- c(Salary ~ Age,Salary ~ G,Salary ~ MP,Salary ~ FG,Salary ~ FTA,Salary ~ ORB,
#               Salary ~ DRB,Salary ~ AST,Salary ~ STL,Salary ~ BLK,Salary ~ TOV,
#               Salary ~ PF,Salary ~ FGA,Salary ~ FTA,Salary ~ X3P,Salary ~ X3PA,
#               Salary ~ X2P,Salary ~ X2PA,Salary ~ PTS)
# 
# for(i in c(1:length(formulas))){
#   result <- plm(formulas[[i]], data = analyse_data, index=c("Season", "PlayerID"),effect = "time", model = "within")
#   result <- summary(result)
#   coefficients <- coef(result)
#   coefficients[1,"Pr(>|t|)"] <- paste(coefficients[1,"Pr(>|t|)"],rate(coefficients[1,"Pr(>|t|)"]))
#   #print(paste(coefficients,rate(coefficients[1,"Pr(>|t|)"]),"\n"))
#   print(coefficients)
#   #print(rate(coefficients[1,"Pr(>|t|)"]))
# }
# summary(result)
# sigma(result)
