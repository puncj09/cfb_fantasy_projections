######## College Football DraftKings Projections ##########
# This code reads in data from several sources including: player stats scraped from CFB API, injury data, DraftKings salary info, and name matching data
# Further cleaning including fantasy point calculation
# also attempts to predict player ownership using Dirichlet regression
# writes this week's fantasy projections to .csv for lineup optimization in Python

setwd('C:/Users/punco/OneDrive/Desktop/Fantasy Football/2021/CFB')

###### LOAD PACKAGES ###########
library(tidyverse)
library(plyr)
library(dplyr)
library(MASS) # mvrnorm
library(DirichletReg) # Dirichlet regression
library(nlme) # weighted least squares

####### READ CSVs ############
# read in .csv files from several sources

# game stats data
data = read.csv('CFBGameStats.csv')
data$Value = as.numeric(data$Value)

# Team abbrevs (Add to this excel file manually)
team_abbrevs = read.csv("CFB_abbreviations.csv")

# Salary data
salary_slate = read.csv('DKSalariesWeek7.csv')

# Spread/over under from Rotowire ## https://www.rotowire.com/betting/college-football/ ##
lines <- read.csv("RotowireOdds.csv", skip=1) # remove top line

# Injuries (Set injured players' projection to 0, update other projections)
# https://legacy.donbest.com/ncaaf/injuries/ read into excel, save, read into R
injuries_csv <- read.csv("CFB_Injuries.csv")

# Name matching
names = read.csv("CFB_data_names.csv")

##### CLEAN THE DATA #########

# reshape
data = data %>%
  group_by(Player, Year, Week, StatCat, StatName) %>% tidyr::pivot_wider(names_from = c(StatName, StatCat), values_from = Value) 

data[is.na(data)] <- 0

# Name match
data[data=="Treveyon Henderson"] = "TreVeyon Henderson"

# Calculate fantasy points (use DraftKings scoring)
data = data %>% mutate(FP = (-1 * LOST_fumbles) + (6 * TD_receiving) + (.1 * YDS_receiving) + (1 * REC_receiving) +
                         (6 * TD_rushing) + (.1 * YDS_rushing) + (-1 * INT_passing) + (4 * TD_passing) +
                         (.04 * YDS_passing) + ifelse(YDS_passing >= 300, 3, 0) + ifelse(YDS_rushing >= 100, 3, 0) +
                         ifelse(YDS_receiving >= 100, 3, 0))

# Check
data[data$Player=='Kaylon Geiger',c(1,2,11,25,55)]

## Roll up team play data - run plays, pass plays
plays = data %>% dplyr::group_by(Team, Opponent, Week, Year) %>% dplyr::summarise(runs = sum(CAR_rushing), passes = sum(attempts_passing),
                                                       total_plays = runs+passes)
head(plays)

##### More data cleaning #####

# Add team totals using spread
data = data %>% dplyr::mutate(TeamTotal = (overUnder / 2) + (Spread/2))

## Subset by position
RBs = data[data$Position=='RB',]

RBs = na.omit(RBs)
RBs = RBs[RBs$overUnder != 0,]

RBs = RBs[,c("Team", "Player", "Opponent", "Week", "Year", "awayScore", "homeScore", "Spread","overUnder","YDS_receiving",
             "REC_receiving","YDS_rushing", "CAR_rushing", "Rushing","Overall","FirstDown","SecondDown",
             "ThirdDown", "PassingDowns", "Standard", "FP", "TeamTotal", "Position")]

WRs = data[data$Position %in% c('WR','TE'),]
WRs = na.omit(WRs)
WRs = WRs[WRs$overUnder != 0,]
WRs = WRs[,c("Team", "Player", "Opponent", "Week", "Year", "awayScore", "homeScore", "Spread","overUnder","YDS_receiving",
             "REC_receiving","YDS_rushing", "CAR_rushing", "Rushing","Overall","FirstDown","SecondDown",
             "ThirdDown", "PassingDowns", "Standard", "FP", "TeamTotal", "Position")]

QBs = data[data$Position=='QB',]
QBs = na.omit(QBs)
QBs = QBs[QBs$overUnder != 0,]
QBs = QBs[,c("Team", "Player", "Opponent", "Week", "Year", "awayScore", "homeScore", "Spread","overUnder",
             "YDS_rushing", "CAR_rushing", "attempts_passing", "comp_passing", "Rushing","Overall","FirstDown",
             "SecondDown", "ThirdDown", "PassingDowns", "Passing", "Standard", "FP", "TeamTotal", "Position")]

################ SLATE INFO #######################

salary_slate = salary_slate %>% dplyr::select(Name, Position, Salary, TeamAbbrev)
teams_in_slate = unique(salary_slate$TeamAbbrev)

################ SPREAD ####################

lines <- lines[,c("Team", "Spread", "Over.Under")]
lines$Team[lines$Team == "Mississippi"] <- "Ole Miss"
lines <- lines %>% dplyr::rename(overUnder = Over.Under)

# Is Rutgers in slate? add Rutgers + opponent to lines
rutgers_df = data.frame(Team=c("Rutgers", "Michigan State"),
                        Spread = c(4, -4),
                        overUnder = rep(50, 2))

lines = rbind(lines, rutgers_df)
# Add team total
lines <- lines %>% dplyr::mutate(TeamTotal = (overUnder/2) - (Spread/2))


# Merge spread/OU with teamabbrev
spread_data = merge(lines, team_abbrevs, all.x = TRUE, by = "Team")

# Only want games in slate
spread_slate = spread_data[spread_data$abbrev %in% teams_in_slate,]

# Order data by over under 
spread_slate = spread_slate[order(spread_slate$overUnder),]

# check that matchup spreads and over unders are EQUAL

# Merge Spread Info with DK salary info for the slate
salary_slate = merge(salary_slate, spread_slate, all.x = TRUE, by.x="TeamAbbrev", by.y="abbrev")

## CHECK NAMES ##
players_in_slate = names[names$Team %in% salary_slate$Team,]

# check that each player in slate (Draftkings list) exists in the data
name_check = merge(salary_slate[,c('Name', 'Team')], players_in_slate, all.x = TRUE, by.x = c('Name', 'Team'), 
      by.y = c('DK.Name', 'Team'))
name_check = name_check %>% dplyr::select(Name, Team, DK.Team)

name_check[is.na(name_check$DK.Team),]

# Add player offense share data
off_share = data %>% dplyr::filter(Year==2021) %>% dplyr::group_by(Player, Team) %>% 
  dplyr::mutate(Rushing = Rushing, Overall = Overall, Passing = Passing) %>% 
  dplyr::select(Team, Player, Position, Rushing, Overall, Passing)

# Merge off share with updated names
off_share = merge(off_share, players_in_slate[,c('Player','Team','API.Name','DK.Name')], all.x = TRUE, by.x = c('Player', 'Team'), by.y = c('Player', 'Team'))

# Merge offense share data with slate info
off_share = merge(off_share, salary_slate, all.x = TRUE, by.x = c("Player", "Team"), by.y = c("Name", "Team"))
off_share = off_share[!is.na(off_share$TeamAbbrev),]
off_share <- off_share %>% dplyr::rename(Position = Position.x) %>% dplyr::select(-Position.y)
off_share <- off_share %>% dplyr::distinct(Player, .keep_all = TRUE)

################## Amend certain player's usage #########################

#off_share$Overall[off_share$Player=='TreVeyon Henderson'] = .3
#off_share$Rushing[off_share$Player=='TreVeyon Henderson'] = .4

# Miami QB situation
off_share$Overall[off_share$Player=="D'Eriq King"] = 0
off_share$Passing[off_share$Player=="D'Eriq King"] = 0
off_share$Rushing[off_share$Player=="D'Eriq King"] = 0
off_share$Overall[off_share$Player=='Tyler Van Dyke'] = .6
off_share$Rushing[off_share$Player=='Tyler Van Dyke'] = .1
off_share$Passing[off_share$Player=='Tyler Van Dyke'] = 1

# Minnesota RB
off_share$Overall[off_share$Player=="Mar'Keise Irving"] = .4
off_share$Rushing[off_share$Player=="Mar'Keise Irving"] = .5

############## INJURIES - DATA CLEANING ###############

# Remove last 5 columns
injuries <- injuries_csv[,-c(6:ncol(injuries_csv))]

# Rename columns
colnames(injuries) <- c("Date", "Position", "Player", "Injury", "Status")

# Remove rows with last update, but store this date for later
last_update = injuries[1,1]
`%!in%` <- Negate(`%in%`)
injuries = injuries[injuries$Date %!in% last_update,]

# Remove repeating rows (Date, Position, Player, Injury, Status)
injuries = injuries[!(duplicated(injuries) | duplicated(injuries, fromLast = TRUE)), ]
row.names(injuries) <- NULL 

# Remove rows where all columns are equal (teams), store teams to add to 1 column
split_list = split(injuries, seq(nrow(injuries)))
row_teams_list = list()

for(i in 1:length(split_list)) {
  equal_check = outer(split_list[[i]], split_list[[i]], Vectorize(all.equal)) # check if rows are equal
  if(length(equal_check[equal_check %in% "1 string mismatch"]) == 0) { # add to list if true
    row_teams_list[[i]] <- split_list[[i]][1]
  } 
}

# Store teams in DF 1 column
injury_teams_df = do.call(rbind, row_teams_list)
colnames(injury_teams_df) = "Team"

# Add 'Team' column to injuries using index
index = as.numeric(rownames(injury_teams_df)) # use to subset rows later
team_vector = list()
for(i in 1:nrow(injury_teams_df)) {
  if(i==nrow(injury_teams_df)) {
    rep_by = (nrow(injuries)+1) - as.numeric(rownames(injury_teams_df)[i])
  }
  else {
    rep_by = as.numeric(rownames(injury_teams_df)[i+1]) - as.numeric(rownames(injury_teams_df)[i])
  }
  temp_rep_list = rep(injury_teams_df$Team[i], rep_by)
  team_vector <- c(team_vector, temp_rep_list)
}

temp_team_df <- data.frame(Team = do.call(rbind, team_vector))
temp_team_df$Team[temp_team_df$Team=='Minnesota U'] = 'Minnesota'
temp_team_df$Team[temp_team_df$Team=='Tennessee U'] = 'Tennessee'

# Merge
injuries_complete = cbind(injuries, temp_team_df)

# Remove rows with team names only (have rownames = index) 
injuries_complete = injuries_complete[as.numeric(rownames(injuries_complete)) %!in% index,]

# Add injury tag
injuries_complete = injuries_complete %>% mutate(Injured = ifelse(grepl("out", Status, fixed = TRUE), 1, 
                                                                  ifelse(grepl("has left team", Status, fixed = TRUE), 1,
                                                                         ifelse(grepl("OUT", Status, fixed = TRUE), 1, 0))))
# Add questionable tag
injuries_complete = injuries_complete %>% mutate(QuestDoubt = ifelse(grepl("?", Status, fixed = TRUE), 1,
                                                                     ifelse(grepl("doubt", Status, fixed = TRUE), 1, 0)))
injuries_complete$Team[injuries_complete$Team=="Central Florida"] = "UCF"

# merge injury tag to each player in off_share based on injuries_complete
off_share_with_injuries = merge(off_share, injuries_complete[,c("Status", "Injury", "Player", "Team", "Injured", "QuestDoubt")], by = c("Player", "Team"), all.x = TRUE)
off_share_with_injuries = off_share_with_injuries %>% mutate(Injured = replace_na(Injured, 0),
                                                             QuestDoubt = replace_na(QuestDoubt, 0))

# If injured, set offense share to 0
off_share_with_injuries = off_share_with_injuries %>% mutate(Rushing = ifelse(Injured==1, 0, Rushing),
                                                             Passing = ifelse(Injured==1, 0, Passing),
                                                             Overall = ifelse(Injured==1, 0, Overall),
                                                             overUnder = ifelse(Injured==1, 0, overUnder))

# Give their production to backups

### IS STARTING QB ###


# separate data by position, use as 'new data' for linear regression models

RB_all = off_share_with_injuries[off_share_with_injuries$Position=='RB',]
RB_all = RB_all%>% dplyr::distinct(Player, .keep_all = TRUE)

WR_all = off_share_with_injuries[off_share_with_injuries$Position %in% c('WR','TE'),]
WR_all = WR_all %>% dplyr::distinct(Player, .keep_all = TRUE)

QB_all = off_share_with_injuries[off_share_with_injuries$Position=='QB',]
QB_all = QB_all %>% dplyr::distinct(Player, .keep_all = TRUE)

## RUN MODELS position by position ###

## Model FP by position, use all data from 2020-2021 to train and test model

#####################################

get_linear_model = function(pos_df) { # Run linear model on training data
  # Remove FP < 2 (arbitrary)
  pos_df = pos_df[pos_df$FP>2,]
  
  set.seed(123)
  n = dim(pos_df)[1]
  nfolds = 10
  groups = rep(1:nfolds, n)
  cvgroups = sample(groups, n)
  
  # 10 fold CV loop
  for(ii in 1:nfolds) {
    groupii = (cvgroups == ii)
    trainset = pos_df[!groupii,]
    
    mod <- rlm(FP ~ Overall + overUnder, data = trainset, psi = psi.bisquare)
    #wt <- 1 / lm(abs(mod$residuals) ~ mod$fitted.values)$fitted.values^2
    #wls_model <- lm(FP ~ Overall + overUnder, data = trainset, weights=wt)
    
  }
  return(mod)
}

predict_points = function(mod, pos_df) {
  predicted = predict(mod, newdata = pos_df)
  return(predicted)
}

# Get linear model for each position 
mod_RB = get_linear_model(RBs)
mod_WR = get_linear_model(WRs)
mod_QB = get_linear_model(QBs)

# Get mean Projections from each model
RB_projections = RB_all[,c("Player", "Team", "Position", "overUnder")]
RB_projections$proj_points = predict_points(mod_RB, RB_all)

WR_projections = WR_all[,c("Player", "Team", "Position", "overUnder")]
WR_projections$proj_points = predict_points(mod_WR, WR_all)

QB_projections = QB_all[,c("Player", "Team", "Position", "overUnder")]
QB_projections$proj_points = predict_points(mod_QB, QB_all)

all_projections = rbind(RB_projections, WR_projections, QB_projections)
# View projections
all_projections[order(desc(all_projections$proj_points)),]

######## Get variance covariance matrix for a team #####

## get each player's historical variance
get_player_variance = function(df) {
  #Assign constant variance (sd) to player's with NA or not enough games
  rb_sigma = 9.5
  wr_sigma = 8.3
  qb_sigma = 12
  
  temp = df %>% dplyr::mutate(Position = ifelse(Position=="TE", "WR", Position)) %>% dplyr::select(Position, FP, Team, Player)
  temp = temp %>% dplyr::group_by(Year, Week, Position, Team, Player) %>% dplyr::summarise(FP = sum(FP))
  
  # Calculate each player's variance, store in df
  #player_games = temp %>% group_by(Position, Team, Player) %>% count()
  player_variance = temp %>% dplyr::group_by(Position, Team, Player) %>%
                                dplyr::summarise(FP_var = sd(FP), games = n()) %>%
                                  dplyr::select(Player, FP_var, games)
  
  # If player has NA variance or < 3 games played, set sigma = positional average
  player_variance = player_variance %>% dplyr::mutate(FP_var = ifelse(games < 3 | is.na(FP_var),
                                                               ifelse(Position=="RB", rb_sigma,
                                                                      ifelse(Position=="WR", wr_sigma, qb_sigma)), 
                                                                      FP_var))
  
  return(player_variance) # returns df
}

# get diagonals (player variance)
get_diagonals = function(full_data, team) {
  temp = full_data[full_data$Team==team,]
  diag = get_player_variance(temp)
  diag = diag[order(diag$Player),]
  
  return(diag)  
}

# Create covariance matrix for one team
create_covariance_mat = function(df, full_data, team) {
  
  df = df %>% dplyr::filter(Team==team)
  temp = df %>% dplyr::group_by(Player, Year, Week) %>% dplyr::summarise(FP = sum(FP))
    
  # pivot wider, each player is column, each week is row
  temp = temp %>% tidyr::pivot_wider(names_from = Player, values_from = FP, values_fill=0)

  # Create correlation matrix for team i
  cormat = cor(temp[,-c(1:2)], method = "pearson", use = "pairwise.complete.obs")
    
  # Get diagonals
  diags = get_diagonals(full_data, team)$FP_var
  diag(cormat) = diags
    
  cormat = data.frame(cormat)
  cormat$Team = team

  return(cormat)
}

# Get correlation for all teams
temp = data %>% dplyr::mutate(Position = ifelse(Position=="TE", "WR", Position)) %>% dplyr::select(Position, FP, Team)
teams_in_slate = unique(salary_slate$Team)
cor_dfs = list()

for(i in 1:length(teams_in_slate)) {
  temp_df = create_covariance_mat(temp, data, teams_in_slate[i])
  cor_dfs[[i]] <- temp_df
}

# Look at correlation
write.csv(cor_dfs[[22]], "correlationMatrix.csv")

############### GET MEANS FOR PLAYERS (PROJECTED POINTS) ##########

# Create mean vector (projected points for the week)
teams_in_slate = unique(salary_slate$Team)
muteam_list <- list()
for(i in 1:length(teams_in_slate)) {
  team_df = all_projections[all_projections$Team==teams_in_slate[i],]
  
  # Get projected points for each name in cor_dfs[[i]]
  team_players = data.frame(Player = rownames(cor_dfs[[i]]))
  team_projections = merge(team_df, team_players, all.y = TRUE, by = "Player", sort = TRUE)
  # Replace projected points NAs with 0
  team_projections = team_projections %>% dplyr::mutate(proj_points = replace_na(proj_points, 0))
  muteam_list[[i]] = team_projections$proj_points # vector of proj_points
}

# simulate fantasy points using the multivariate normal distribution
points_list <- list()
for(i in 1:length(teams_in_slate)) {
  ncols = ncol(cor_dfs[[i]])
  mu_list = muteam_list[[i]]
  sigma = cor_dfs[[i]][,-ncols]
  # Replace NAs with 0
  sigma[is.na(sigma)] = 0
  
  # Check for negative eigenvalues
  eigSigma = eigen(sigma)
  # Replace negatives with 0
  newEig = ifelse(eigSigma$values < 0, 0, eigSigma$values)
  # Convert back
  sigma <- eigSigma$vectors %*% diag(newEig) %*% t(eigSigma$vectors)
  
  points_df<-data.frame(mvrnorm(n=10000, mu=mu_list, Sigma=as.matrix(sigma)))
  
  colnames(points_df) <- rownames(cor_dfs[[i]])
  points_df$Team = teams_in_slate[i]
  
  points_list[[i]] <- points_df  
}

# For each list, pivot longer, then append to data frame
full_projections_list = list()
for(i in 1:length(teams_in_slate)) {
  team_proj_df = points_list[[i]]
  team_proj_df = team_proj_df %>% tidyr::pivot_longer(!Team, names_to = "Player", values_to = "Proj_Points")
  team_proj_df$index = rep(1:10000, each = ncol(points_list[[i]])-1)
  full_projections_list[[i]] <- team_proj_df
}

full_projections = do.call(rbind, full_projections_list)
  
# get positions - left join
full_projections = merge(full_projections, all_projections, all.x = TRUE, by = c("Player", "Team"))
# remove NAs 
full_projections = full_projections[!is.na(full_projections$Position),]
# Drop columns
full_projections = full_projections[,-c(ncol(full_projections))]

# Merge with salary

# Check for dupes in salary_slate
head(salary_slate[salary_slate$Name=="A.J. Abbott",])
# Drop dupes from salary slate
salary_slate = salary_slate[!duplicated(salary_slate$Name),] #This removes players with same name (could be bad)

# A.J. Abbott is on Wisconsin and Oregon, remove him
full_projections = full_projections[full_projections$Player != "A.J. Abbott",]
######### fix this^

projections_with_names_salaries = merge(full_projections, salary_slate[,c("Name", "Team", "Salary")], all.x = TRUE, by.x = c("Player","Team"),
                                        by.y = c("Name", "Team"))

# correlation table for analysis
corr_table = all_projections %>% dplyr::select(Player, Team, Position)
write.csv(corr_table, "corr_table.csv", row.names = FALSE)

#### CHANGE CSV NAME ####
# Write.csv for python use
write.csv(projections_with_names_salaries, "SimulatedProjections_Week7.csv", row.names = FALSE)

# Ownership
own_df = all_projections
own_df = merge(own_df, salary_slate[,c('Name', 'Team', 'Salary')], all.x = TRUE, by.x = c('Player', 'Team'), by.y = c('Name', 'Team'))
own_df = own_df[order(desc(own_df$proj_points)),]

# Write to .csv to adjust projections (copy industry projections - Rotowire)
write.csv(own_df, 'Week7_ownership_Fri.csv', row.names = FALSE)

# Read after making adjustments
updated_ownership = read.csv("Week7_ownership_Fri.csv") # use as test data in dirichlet regression below
updated_ownership = updated_ownership %>% dplyr::rename(Projection = proj_points, OverUnder = overUnder)
updated_ownership = updated_ownership %>% mutate(Position = ifelse(Position=='TE', 'WR', Position))


#### Dirichlet Regression to Predict Ownership ####

own = read.csv('Ownership_CFB.csv')

own_by_pos = own %>% dplyr::group_by(Week, Position) %>% dplyr::summarise(Own = sum(Own))

# Relative ownership by position
own = own %>% dplyr::mutate(RelativeOwn = case_when(Position=='WR' & Week==6 ~ Own / 3.46,
                                                    Position=='RB' & Week==6 ~ Own / 2.34,
                                                    Position=='QB' & Week==6 ~ Own / 1.81,
                                                    Position=='WR' & Week==5 ~ Own / 3.56,
                                                    Position=='RB' & Week==5 ~ Own / 2.59,
                                                    Position=='QB' & Week==5 ~ Own / 1.73))

# split into position
own_QB = own[own$Position=='QB',]
own_RB = own[own$Position=='RB',]
own_WR = own[own$Position=='WR',]

# Model each position
set.seed(100)

# Predict next week's ownership
# Get relative ownership
updated_ownership = updated_ownership %>% 
                      dplyr::mutate(RelativeOwn = case_when(Position=='WR' ~ 3.6,
                                                    Position=='RB' ~ 2.5,
                                                    Position=='QB' ~ 1.9))
# df for each position
current_week_QB = updated_ownership[updated_ownership$Position=='QB',]
current_week_WR = updated_ownership[updated_ownership$Position=='WR',] 
current_week_RB = updated_ownership[updated_ownership$Position=='RB',]



# QB
n=dim(own_QB)[1]
LOOCVpredictions = rep(NA,n)
cvgroups = 1:n
nfolds = n
x <- own_QB %>% select(Salary,Projection, OverUnder)
y <- own_QB$RelativeOwn

for(ii in 1:nfolds) {
  groupii = (cvgroups == ii)

  train_set = own_QB[!groupii,]
  
  # prepare the Y's
  train_set$Y <- DR_data(train_set[,'RelativeOwn'])
  #test_set$Y <- DR_data(test_set[,'RelativeOwn'])
  
  mod_qb <- DirichReg(Y ~ scale(Salary) + scale(Projection) + scale(OverUnder), train_set)
}


# WR
n=dim(own_WR)[1]
LOOCVpredictions = rep(NA,n)
cvgroups = 1:n
nfolds = n

for(ii in 1:nfolds) {
  groupii = (cvgroups == ii)
  train_set <- own_WR[-groupii,] # training Data
  #test_set <- own_WR[groupii,] # test Data
  
  # prepare the Y's
  train_set$Y <- DR_data(train_set[,'RelativeOwn'])
  #test_set$Y <- DR_data(test_set[,'RelativeOwn'])
  
  mod_wr <- DirichReg(Y ~ scale(Salary) + scale(Projection) + scale(OverUnder), train_set)
}

# RB
n=dim(own_RB)[1]
LOOCVpredictions = rep(NA,n)
cvgroups = 1:n
nfolds = n

for(ii in 1:nfolds) {
  groupii = (cvgroups == ii)
  train_set <- own_RB[-groupii,] # training Data
  #test_set <- own_RB[groupii,] # test Data
  
  # prepare the Y's
  train_set$Y <- DR_data(train_set[,'RelativeOwn'])
  #test_set$Y <- DR_data(test_set[,'RelativeOwn'])
  
  mod_rb <- DirichReg(Y ~ scale(Salary) + scale(Projection) + scale(OverUnder), train_set)
}

current_week_QB$pred = predict(mod_qb, newdata = current_week_QB)[,2]*2
current_week_WR$pred = predict(mod_wr, newdata = current_week_WR)[,2]*3.5
current_week_RB$pred = predict(mod_rb, newdata = current_week_RB)[,2]*2.5

all_own = rbind(current_week_QB, current_week_WR, current_week_RB)

write.csv(all_own, 'PredictedOwnership_Week7_Fri.csv', row.names = FALSE)

####### END OWNERSHIP #############


############ TEST NEW FEATURES #################

## RBS
predicted = predict(mod_new, newdata = RB_all)

RB_projections = RB_all[,c("Player", "Team", "Position")]
RB_projections$proj_points = predicted
RB_projections[order(desc(RB_projections$proj_points)),]

## WRS
predictedWR = predict(mod_WR, newdata = WR_all)

WR_projections = WR_all[,c("Player", "Team", "Position")]
WR_projections$proj_points = predictedWR
WR_projections[order(desc(WR_projections$proj_points)),]

## QBS
predictedQB = predict(mod_QB, newdata = QB_all)

QB_projections = QB_all[,c("Player", "Team", "Position")]
QB_projections$proj_points = predictedQB
QB_projections[order(desc(QB_projections$proj_points)),]

all_projections = rbind(RB_projections, WR_projections, QB_projections)

# Merge with salary
all_projections_salary = merge(all_projections, salary_slate, all.x = TRUE, by.x = c("Player", "Team"), 
                               by.y = c("Name", "Team"))

all_projections_salary = all_projections_salary %>% mutate(value = proj_points / (Salary/1000))
all_projections_salary = all_projections_salary %>% rename(Position = Position.y) %>% dplyr::select(-Position.x)

# Write to .csv 
write.csv(all_projections_salary, 'slate_projectionsWeek5.csv', row.names = FALSE)

###### END OF STATISTICAL MODELS ######

##### COMPARE PROJECTIONS TO ACTUAL POINTS #########

# Read in last week's actual stats
previous_week = 5
year = 2021
previous_week_df = data[data$Year==year & data$Week==previous_week,]
previous_week_df = previous_week_df %>% dplyr::select(Team, Player, Position, FP)
write.csv(previous_week_df, "previous_week.csv", row.names = FALSE)

# Previous week's projections
main <- read.csv('slate_projectionsWeek5.csv')
#afternoon <- read.csv('')
night <- read.csv('slate_projectionsWeek4Night.csv')

# Rbind main, afternoon, and/or night if applicable


# Merge to compare proj to actual
compare = merge(main, previous_week_df, all.x = TRUE, by.x = c("Player", "Team"), by.y = c("Player", "Team"))[,c("Player","Team", "Position.x", "proj_points", 'FP')]

# Add is.na indicator column
compare = compare %>% mutate(FP = replace_na(FP, 0))
compare = compare %>% mutate(Difference = proj_points - FP)

compare$Week = previous_week
compare$Year = year

archived_projections = read.csv("Archived_projections.csv")
archived_projections = rbind(archived_projections, compare)
write.csv(archived_projections, "Archived_projections.csv", row.names = FALSE)

##############################

# Quick look at some things

slate_df = data[data$Year==2021,]
slate_df = slate_df[slate_df$Team %in% unique(salary_slate$Team),]
slate_df %>% filter(CAR_rushing > 8) %>% ggplot(aes(CAR_rushing, FP)) + geom_text(aes(label=Player))
slate_df %>% ggplot(aes(REC_receiving, FP)) + geom_text(aes(label=Player))


#########################

#### Mean and standard deviation for every player ####

RB_distribution = RBs %>% dplyr::distinct(Player, .keep_all = TRUE) %>% group_by(Player, Year) %>% 
                    mutate(sd = sd(FP), mean = mean(FP), games = n()) %>% 
                      dplyr::select(Team, Player, Week, FP, sd, mean, games)

WR_distribution = WRs %>% dplyr::distinct(Player, .keep_all = TRUE) %>% group_by(Player, Year) %>% 
  mutate(sd = sd(FP), mean = mean(FP), games = n()) %>% 
  dplyr::select(Team, Player, Week, FP, sd, mean, games)

QB_distribution = QBs %>% dplyr::distinct(Player, .keep_all = TRUE) %>% group_by(Player, Year) %>% 
  mutate(sd = sd(FP), mean = mean(FP), games = n()) %>% 
  dplyr::select(Team, Player, Week, FP, sd, mean, games)

# Feed these numbers into projections/simulation


# Get all players
data_to_csv = data %>% dplyr::distinct(Player, Team) %>% 
  dplyr::select(Player, Team) %>% dplyr::filter(Year==2021)

write.csv(data_to_csv, 'Players_CFB.csv')


week6 <- read.csv("SimulatedProjections_Week6.csv")

week6_new <- week6 %>% group_by(Player) %>% dplyr::summarise(avg_points = mean(Proj_Points), Salary = mean(Salary))
write.csv(week6_new, "CFB_Projections_Week6.csv", row.names = FALSE)



#### Dirichlet Regression to Predict Ownership ####

own = read.csv('Ownership_CFB.csv')

own_by_pos = own %>% dplyr::group_by(Week, Position) %>% dplyr::summarise(Own = sum(Own))

# Relative ownership by position
own = own %>% dplyr::mutate(RelativeOwn = case_when(Position=='WR' & Week==6 ~ Own / 3.46,
                                                    Position=='RB' & Week==6 ~ Own / 2.34,
                                                    Position=='QB' & Week==6 ~ Own / 1.81,
                                                    Position=='WR' & Week==5 ~ Own / 3.56,
                                                    Position=='RB' & Week==5 ~ Own / 2.59,
                                                    Position=='QB' & Week==5 ~ Own / 1.73))

# split into position
own_QB = own[own$Position=='QB',]
own_RB = own[own$Position=='RB',]
own_WR = own[own$Position=='WR',]

