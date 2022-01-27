################ Packages ################
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
library(psych)
library(dplyr)
library(ggplot2)

################ Global Variables ################
N_TEAMS  = 20
N_SEASONS= 21
N_GAME_WEEKS =38
SEASONS <- c(2001:2021)
MODELTYPE = "KNN"
################ Load Files ################
source_dir <- paste(getwd(),"Raw_Data",sep = "/") # path location of files

xGNS_ovr_H_features_file <- paste(getwd(),MODELTYPE,"xGNS_ovr_H_features.csv",sep = "/")
xGNS_ovr_A_features_file <- paste(getwd(),MODELTYPE,"xGNS_ovr_A_features.csv",sep = "/")
xGNS_H_features_file <- paste(getwd(),MODELTYPE,"xGNS_H_features.csv",sep = "/")
xGNS_A_features_file <- paste(getwd(),MODELTYPE,"xGNS_A_features.csv",sep = "/")

file_names<-list.files(source_dir) # list of file names
raw_data <- read.csv(paste(source_dir,file_names[1],sep="/")) # read in CSV data
#columnsReq = c("HomeTeam","AwayTeam","FTHG","FTAG","FTR","HTHG","HTAG","HTR","HS","AS","HST","AST","HF","AF","HC","AC","HY","AY","HR","AR", "Date") # Required columns from the data

# Read remaining files and rbind them to data set
for (f in file_names[-1]) {
  raw_data <- bind_rows(raw_data,read.csv(paste(source_dir, f, sep="/")))
}

head(raw_data) # print out top rows of raw data
playing_statistics = raw_data#[columnsReq] # select required columns from raw data

################ Data Setup ################
playing_statistics$HP <- ifelse(playing_statistics$FTR == "H", 3,ifelse(playing_statistics$FTR == "A", 0, 1)) # home points
playing_statistics$AP <- ifelse(playing_statistics$FTR == "H", 0,ifelse(playing_statistics$FTR == "A", 3, 1)) # away points
playing_statistics$FTR <- ifelse(playing_statistics$FTR == "H", 3,ifelse(playing_statistics$FTR == "A", 0, 1))
playing_statistics$HD <-  playing_statistics$FTHG - playing_statistics$FTAG # Home Difference
playing_statistics$AD <-   playing_statistics$HD *-1 # Away Difference

playing_statistics$TP_H <- 0 # Total points home team
playing_statistics$TP_A <- 0 # Total points away team

playing_statistics$TGS_H <- 0 # Total goal scored home team
playing_statistics$TGS_A <- 0 # Total goal scored away team

playing_statistics$TGC_H <- 0 # Total goal conceded home team
playing_statistics$TGC_A <- 0 # Total goal conceded away team

playing_statistics$TGD_H <- 0 # Total goal difference home
playing_statistics$TGD_A <- 0 # Total goal difference away

playing_statistics$POS_H <- 0 # Total goal difference home
playing_statistics$POS_A <- 0 # Total goal difference away

playing_statistics$N_Home_Game <- 0 # Number of games home team
playing_statistics$N_Away_Game <- 0 # Number of games away team

playing_statistics$GW_A <- 0 # Game week for away team
playing_statistics$GW_H <- 0 # Game week for home team

playing_statistics$SZN <- rep(2001:2021, each = 380) # Assign season number
playing_statistics$GAME_NUM <- rep(1:7980) # Assign unique game number ID

playing_statistics$TP_Away_Table <- 0 # Total points away table
playing_statistics$TGS_Away_Table <- 0 # Total goals scored away table
playing_statistics$TGC_Away_Table <- 0 # Total points away table
playing_statistics$TGD_Away_Table <- 0 # Total goal difference away table
playing_statistics$TP_Home_Table <- 0 # Total points home table
playing_statistics$TGS_Home_Table <- 0 # Total goals scored home table
playing_statistics$TGC_Home_Table <- 0 # Total goals conceded home table
playing_statistics$TGD_Home_Table <- 0 # Total goal difference

playing_statistics$TS_Home <- 0 # Total shots home
playing_statistics$TST_Home <- 0 # Total shots on target home
playing_statistics$TSA_Home <- 0 # Total shots against against home
playing_statistics$TSTA_Home <- 0 # Total shots on target against home
playing_statistics$TS_Away <- 0 # Total shots away
playing_statistics$TST_Away <- 0 # Total shots on target away
playing_statistics$TSA_Away <- 0 # Total shots against against away
playing_statistics$TSTA_Away <- 0 # Total shots on target against away

# League Averages
playing_statistics$GS_LG_Avg <- 0 # Average goals scored in the league
playing_statistics$GC_LG_Avg <- 0 # Average goals conceded in the league
playing_statistics$GD_LG_Avg <- 0 # Average goals difference in the league

playing_statistics$N_HW_Home <- 0 # Number of home wins for home team
playing_statistics$N_HDraws_Home <- 0 # Number of home draws for home team
playing_statistics$N_HL_Home <- 0 # Number of home losses for home team
playing_statistics$N_AW_Home <- 0 # Number of away wins home team
playing_statistics$N_ADraws_Home <- 0 # Number of away draws home team
playing_statistics$N_AL_Home <- 0 # Number of away losses home team

playing_statistics$N_HW_Away <- 0 # Number of home wins for away team
playing_statistics$N_HDraws_Away <- 0 # Number of home draws for away team
playing_statistics$N_HL_Away <- 0 # Number of home losses for away team
playing_statistics$N_AW_Away <- 0 # Number of away wins for away team
playing_statistics$N_ADraws_Away <- 0 # Number of away draws for away team
playing_statistics$N_AL_Away <- 0 # Number of away losses for away team

playing_statistics$Wins_Ovr_LG <- 0 # League win rate
playing_statistics$Wins_Home_LG <- 0 # League home win rate
playing_statistics$Wins_Away_LG <- 0 # League away win rate
playing_statistics$Draws_Ovr_LG <- 0 # League draw rate
playing_statistics$Draws_Home_LG <- 0 # League home draw rate
playing_statistics$Draws_Away_LG <- 0 # League away draw rate
playing_statistics$Losses_Ovr_LG <- 0 # League loss rate
playing_statistics$Losses_Home_LG <- 0 # League home win rate
playing_statistics$Losses_Away_LG <- 0 # League away loss rate

playing_statistics$GS_LG_Avg_Home <- 0
playing_statistics$GC_LG_Avg_Home <- 0

playing_statistics$GS_LG_Avg_Away <- 0
playing_statistics$GC_LG_Avg_Away <- 0

league_table_values <- playing_statistics # Values for league table

### remove rows with null values -> if there are any? Shouldn't be at this point

################ Assign League Based Stats ################
season <- 2001 # Start season

# Loop through one season at a time
for (k in seq(from=1, to=7980, by=380)){
  a = k +379
  teams_list <- unique(playing_statistics$HomeTeam[k:a]) # all teams for a given season
  
  # Loop through one team at a time
  for (x in teams_list){
    tmp_gameweeks <- filter(playing_statistics, (HomeTeam == x | AwayTeam == x) & (SZN == season)) # All games for team 'x'
    tmp_gameweeks$GW <- rep(1:38) # Assign game week
    
    tmp_home <- filter(tmp_gameweeks, HomeTeam == x) # All home games
    
    tmp_home$TGS_H = cumsum(tmp_home$FTHG) # Total goals scored at home
    tmp_home$TGC_H = cumsum(tmp_home$FTAG) # Total goals conceded at home
    tmp_home$TP_H = cumsum(tmp_home$HP) # Total points at home
    tmp_home$TGD_H = cumsum(tmp_home$HD) # Total goal difference at home
    tmp_home$TP = tmp_home$HP
    tmp_home$GS = tmp_home$FTHG
    tmp_home$GC = tmp_home$FTAG
    tmp_home$N_Home_Game <- rep(1:nrow(tmp_home)) # Number of home games
    tmp_home$N_Away_Game <- 0 # Number of away games
    tmp_home$GW_H <- tmp_home$GW # Home game week
    tmp_home$GW_A <- 0 # Away game week
    tmp_home$N_HW <- ifelse(tmp_home$FTR == "H",1,0) # Number of home wins
    tmp_home$N_HDraws <- ifelse(tmp_home$FTR == "D",1,0) # Number of home draws
    tmp_home$N_HL <- ifelse(tmp_home$FTR == "A",1,0) # Number of home losses
    tmp_home$N_AW <- 0
    tmp_home$N_ADraws <- 0
    tmp_home$N_AL <- 0
    tmp_home$TS <- cumsum(tmp_home$HS) # Total shots home
    tmp_home$TST <- cumsum(tmp_home$HST) # Total shots on target home
    tmp_home$TSA <- cumsum(tmp_home$AS) # Total shots against against home
    tmp_home$TSTA <- cumsum(tmp_home$AST) # Total shots on target against home
    
    
    tmp_away <- filter(tmp_gameweeks, AwayTeam == x) # All away games
    
    tmp_away$TGS_A = cumsum(tmp_away$FTAG) 
    tmp_away$TGC_A = cumsum(tmp_away$FTHG)
    tmp_away$TP_A = cumsum(tmp_away$AP)
    tmp_away$TGD_A = cumsum(tmp_away$AD)
    tmp_away$TP = tmp_away$AP
    tmp_away$GS = tmp_away$FTAG
    tmp_away$GC = tmp_away$FTHG
    tmp_away$N_Home_Game <- 0
    tmp_away$N_Away_Game <- rep(1:nrow(tmp_away))
    tmp_away$GW_H <- 0
    tmp_away$GW_A <- tmp_away$GW
    tmp_away$N_AW <- ifelse(tmp_away$FTR == "A",1,0) # Number of away draws
    tmp_away$N_ADraws <- ifelse(tmp_away$FTR == "D",1,0) # Number of away draws
    tmp_away$N_AL <- ifelse(tmp_away$FTR == "H",1,0) # Number of away losses
    tmp_away$N_HW <- 0 # Number of home draws
    tmp_away$N_HDraws <- 0 # Number of home draws
    tmp_away$N_HL <- 0 # Number of home losses
    tmp_away$TS <- cumsum(tmp_away$AS) # Total shots home
    tmp_away$TST <- cumsum(tmp_away$AST) # Total shots on target home
    tmp_away$TSA <- cumsum(tmp_away$HS) # Total shots against against home
    tmp_away$TSTA <- cumsum(tmp_away$HST) # Total shots on target against home
    
    tmp_overall <- rbind(tmp_home, tmp_away) # combine home and away tables
    tmp_overall <- tmp_overall[order(tmp_overall$GW),] # order by game week
    #row.names(tmp_overall) <- NULL # resets index's
    
    tmp_overall$TP = cumsum(tmp_overall$TP) # Cummulative summation of total points
    tmp_overall$GS = cumsum(tmp_overall$GS)
    tmp_overall$GC = cumsum(tmp_overall$GC)
    tmp_overall$GD = tmp_overall$GS - tmp_overall$GC
    tmp_overall$N_HW = cumsum(tmp_overall$N_HW)
    tmp_overall$N_HDraws = cumsum(tmp_overall$N_HDraws)
    tmp_overall$N_HL = cumsum(tmp_overall$N_HL)
    tmp_overall$N_AW = cumsum(tmp_overall$N_AW)
    tmp_overall$N_ADraws = cumsum(tmp_overall$N_ADraws)
    tmp_overall$N_AL = cumsum(tmp_overall$N_AL)
    
    tmp_overall$TS = cumsum(tmp_overall$TS)
    tmp_overall$TS = cumsum(tmp_overall$TST)
    tmp_overall$TS = cumsum(tmp_overall$TSA)
    tmp_overall$TS = cumsum(tmp_overall$TSTA)
    
    col_names_home = c("TP_H", "TGS_H", "TGC_H", "TGD_H", "GW_H", "N_Home_Game", "N_HW_Home", "N_HDraws_Home", "N_HL_Home", "N_AW_Home", "N_ADraws_Home", 
                       "N_AL_Home", "TS_Home","TST_Home","TSA_Home","TSTA_Home")
    col_names_away = c("TP_A", "TGS_A", "TGC_A", "TGD_A", "GW_A", "N_Away_Game", "N_HW_Away", "N_HDraws_Away", "N_HL_Away", "N_AW_Away", "N_ADraws_Away",
                       "N_AL_Away","TS_Away","TST_Away","TSA_Away","TSTA_Away")
    
    # Assign values to league table data frame
    for (i in 1:38){
      col_names_vec <- if(tmp_overall$HomeTeam[i]==x) col_names_home else col_names_away
      
      home_vec <- c(tmp_overall$TP[i], tmp_overall$GS[i], tmp_overall$GC[i], tmp_overall$GD[i], tmp_overall$GW_H[i], tmp_overall$N_Home_Game[i],tmp_overall$N_HW[i],tmp_overall$N_HDraws[i],
                    tmp_overall$N_HL[i],tmp_overall$N_AW[i],tmp_overall$N_ADraws[i],tmp_overall$N_AL[i],tmp_overall$TS[i], tmp_overall$TST[i], tmp_overall$TSA[i], tmp_overall$TSTA[i])
      away_vec <- c(tmp_overall$TP[i], tmp_overall$GS[i], tmp_overall$GC[i], tmp_overall$GD[i], tmp_overall$GW_A[i], tmp_overall$N_Away_Game[i],tmp_overall$N_HW[i],tmp_overall$N_HDraws[i],
                    tmp_overall$N_HL[i],tmp_overall$N_AW[i],tmp_overall$N_ADraws[i],tmp_overall$N_AL[i],tmp_overall$TS[i], tmp_overall$TST[i], tmp_overall$TSA[i], tmp_overall$TSTA[i])
      vec <- if(tmp_overall$HomeTeam[i]==x) home_vec else away_vec
      
      ind <- match(tmp_overall$GAME_NUM[i], league_table_values$GAME_NUM)
      league_table_values[ind, col_names_vec] <- vec
    } 
    
    # Assign values to playing statistics data frame
    for (i in 1:37){
      col_names_vec <- if(tmp_overall$HomeTeam[i+1]==x) col_names_home else col_names_away
      
      home_vec <- c(tmp_overall$TP[i], tmp_overall$GS[i], tmp_overall$GC[i], tmp_overall$GD[i], tmp_overall$GW_H[i], tmp_overall$N_Home_Game[i],tmp_overall$N_HW[i],tmp_overall$N_HDraws[i],
                    tmp_overall$N_HL[i],tmp_overall$N_AW[i],tmp_overall$N_ADraws[i],tmp_overall$N_AL[i],tmp_overall$TS[i], tmp_overall$TST[i], tmp_overall$TSA[i], tmp_overall$TSTA[i])
      away_vec <- c(tmp_overall$TP[i], tmp_overall$GS[i], tmp_overall$GC[i], tmp_overall$GD[i], tmp_overall$GW_A[i], tmp_overall$N_Away_Game[i],tmp_overall$N_HW[i],tmp_overall$N_HDraws[i],
                    tmp_overall$N_HL[i],tmp_overall$N_AW[i],tmp_overall$N_ADraws[i],tmp_overall$N_AL[i],tmp_overall$TS[i], tmp_overall$TST[i], tmp_overall$TSA[i], tmp_overall$TSTA[i])
      vec <- if(tmp_overall$HomeTeam[i]==x) home_vec else away_vec
      
      ind <- match(tmp_overall$GAME_NUM[i+1], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_vec] <- vec
    }
    
    # home and away league table values
    tmp_home$TP = cumsum(tmp_home$TP)
    tmp_home$GS = cumsum(tmp_home$GS)
    tmp_home$GC = cumsum(tmp_home$GC)
    tmp_home$GD = tmp_home$GS - tmp_home$GC
    
    tmp_away$TP = cumsum(tmp_away$TP)
    tmp_away$GS = cumsum(tmp_away$GS)
    tmp_away$GC = cumsum(tmp_away$GC)
    tmp_away$GD = tmp_away$GS - tmp_away$GC
    
    col_names_home = c("TP_Home_Table", "TGS_Home_Table", "TGC_Home_Table", "TGD_Home_Table") # added custom game weeks per team for later calcs
    col_names_away = c("TP_Away_Table", "TGS_Away_Table", "TGC_Away_Table", "TGD_Away_Table") # game week is total number of games home and away count separate
    
    # home and away tables values for league table data frame
    for (i in 1:19){
      # home
      home_vec <- c(tmp_home$TP[i], tmp_home$GS[i], tmp_home$GC[i], tmp_home$GD[i])
      
      ind <- match(tmp_home$GAME_NUM[i], league_table_values$GAME_NUM)
      league_table_values[ind, col_names_home] <- home_vec
      
      # away
      away_vec <- c(tmp_away$TP[i], tmp_away$GS[i], tmp_away$GC[i], tmp_away$GD[i])
      
      ind <- match(tmp_away$GAME_NUM[i], league_table_values$GAME_NUM)
      league_table_values[ind, col_names_away] <- away_vec
    } 
    
    # home and away tables values for playing statistics
    for (i in 1:18){
      # home
      home_vec <- c(tmp_home$TP[i], tmp_home$GS[i], tmp_home$GC[i], tmp_home$GD[i])
      
      ind <- match(tmp_home$GAME_NUM[i], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_home] <- home_vec
      
      # away
      away_vec <- c(tmp_away$TP[i], tmp_away$GS[i], tmp_away$GC[i], tmp_away$GD[i])
      
      ind <- match(tmp_away$GAME_NUM[i], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_away] <- away_vec
    }
  }
  season = season + 1
}

################ Assign League Table values ################
# Loop through one season at a time
for (k in seq(from=1, to=7980, by=380)){
  a = k +379
  date_groups <- unique(playing_statistics$Date[k:a]) # create groups by date for a given season
  
  # Create empty league tables to update
  Team <- unique(playing_statistics$HomeTeam[k:a])
  TP <- 0
  GD <- 0
  GS <- 0
  GC <- 0
  league_table <- data.frame(Team, TP, GD, GS, GC)
  
  # Loop through date groups
  for (x in 1:(length(date_groups)-1)){
    tmp_league_table_values <- filter(league_table_values, Date ==date_groups[x])
    columnsReqLeague = c("HomeTeam","AwayTeam","TP_H", "TP_A", "TGS_H", "TGS_A", "TGC_H", "TGC_A", "TGD_H", "TGD_A", "N_Home_Game", "N_Away_Game", "Date")
    tmp_league_table_values[columnsReqLeague]
    
    tmp_home <- tmp_league_table_values[c("HomeTeam","TP_H", "TGS_H", "TGC_H","TGD_H", "N_Home_Game", "N_HW_Home", "N_HDraws_Home", "N_HL_Home", "N_AW_Home", "N_ADraws_Home", "N_AL_Home", "Date")]
    tmp_away <- tmp_league_table_values[c("AwayTeam", "TP_A", "TGS_A", "TGC_A","TGD_A", "N_Away_Game","N_Away_Game", "N_HW_Away", "N_HDraws_Away", "N_HL_Away", "N_AW_Away", "N_ADraws_Away", "N_AL_Away", "Date")]
    
    # Rename Home Cols
    names(tmp_home)[names(tmp_home) == 'HomeTeam'] <- 'Team'
    names(tmp_home)[names(tmp_home) == 'TP_H'] <- 'TP'
    names(tmp_home)[names(tmp_home) == 'TGD_H'] <- 'GD'
    names(tmp_home)[names(tmp_home) == 'TGS_H'] <- 'GS'
    names(tmp_home)[names(tmp_home) == 'TGC_H'] <- 'GC'
    names(tmp_home)[names(tmp_home) == 'N_Home_Game'] <- 'GP' # games played
    names(tmp_home)[names(tmp_home) == 'N_HW_Home'] <- 'Home Wins'
    names(tmp_home)[names(tmp_home) == 'N_HDraws_Home'] <- 'Home Draws'
    names(tmp_home)[names(tmp_home) == 'N_HL_Home'] <- 'Home Losses'
    names(tmp_home)[names(tmp_home) == 'N_AW_Home'] <- 'Away Wins'
    names(tmp_home)[names(tmp_home) == 'N_ADraws_Home'] <- 'Away Draws'
    names(tmp_home)[names(tmp_home) == 'N_AL_Home'] <- 'Away Losses'
    
    # Rename Away Cols
    names(tmp_away)[names(tmp_away) == 'AwayTeam'] <- 'Team'
    names(tmp_away)[names(tmp_away) == 'TP_A'] <- 'TP'
    names(tmp_away)[names(tmp_away) == 'TGD_A'] <- 'GD'
    names(tmp_away)[names(tmp_away) == 'TGS_A'] <- 'GS'
    names(tmp_away)[names(tmp_away) == 'TGC_A'] <- 'GC'
    names(tmp_away)[names(tmp_away) == 'N_Away_Game'] <- 'GP'
    names(tmp_away)[names(tmp_away) == 'N_HW_Away'] <- 'Home Wins'
    names(tmp_away)[names(tmp_away) == 'N_HDraws_Away'] <- 'Home Draws'
    names(tmp_away)[names(tmp_away) == 'N_HL_Away'] <- 'Home Losses'
    names(tmp_away)[names(tmp_away) == 'N_AW_Away'] <- 'Away Wins'
    names(tmp_away)[names(tmp_away) == 'N_ADraws_Away'] <- 'Away Draws'
    names(tmp_away)[names(tmp_away) == 'N_AL_Away'] <- 'Away Losses'
    
    tmp_home <- tmp_home[c("Team","TP","GD", "GS", "GC", "GP", "Home Wins", "Home Draws", "Home Losses", "Away Wins", "Away Draws", "Away Losses")]
    tmp_away <- tmp_away[c("Team","TP","GD", "GS", "GC", "GP", "Home Wins", "Home Draws", "Home Losses", "Away Wins", "Away Draws", "Away Losses")]
    
    league_table_update <- rbind(tmp_home,tmp_away)
    
    # Assign league table values
    for (z in 1:nrow(league_table_update)){
      ind <- match(league_table_update$Team[z], league_table$Team)
      
      league_table$TP[ind] <- league_table_update$TP[z]
      league_table$GD[ind] <- league_table_update$GD[z]
      league_table$GS[ind] <- league_table_update$GS[z]
      league_table$GC[ind] <- league_table_update$GC[z]
      league_table$GP[ind] <- league_table_update$GP[z]
      league_table$`Home Wins`[ind] <- league_table_update$`Home Wins`[z]
      league_table$`Home Draws`[ind] <- league_table_update$`Home Draws`[z]
      league_table$`Home Losses`[ind] <- league_table_update$`Home Losses`[z]
      league_table$`Away Wins`[ind] <- league_table_update$`Away Wins`[z]
      league_table$`Away Draws`[ind] <- league_table_update$`Away Draws`[z]
      league_table$`Away Losses`[ind] <- league_table_update$`Away Losses`[z]
    }
    league_table <- league_table[order(-league_table$TP,-league_table$GD,-league_table$GS,-league_table$GC),] # order the league table
    
    league_table$POS <- c(1:20) # Assign league position
    
    # League values for date groups
    Game_Count <- sum(league_table$GP)
    
    GS_LG_Avg <- sum(league_table$GS) / Game_Count
    GC_LG_Avg <- sum(league_table$GC) / Game_Count
    GD_LG_Avg <- sum(league_table$GD) / Game_Count
    
    Wins_Ovr_LG <- (sum(league_table$`Home Wins`) +  sum(league_table$`Away Wins`)) / Game_Count
    Draws_Ovr_LG <- (sum(league_table$`Home Draws`) +  sum(league_table$`Away Draws`)) / Game_Count
    Losses_Ovr_LG <- (sum(league_table$`Home Losses`) +  sum(league_table$`Away Losses`)) / Game_Count
    
    Wins_Home_LG <- sum(league_table$`Home Wins`) / Game_Count    
    Wins_Away_LG <- sum(league_table$`Away Wins`) / Game_Count
    
    Draws_Home_LG <- sum(league_table$`Home Draws`) / Game_Count
    Draws_Away_LG <- sum(league_table$`Away Draws`) / Game_Count
    
    Losses_Home_LG <- sum(league_table$`Home Losses`) / Game_Count
    Losses_Away_LG <- sum(league_table$`Away Losses`) / Game_Count
    
    col_names_home2 = c("POS_H","GS_LG_Avg", "GC_LG_Avg", "GD_LG_Avg", "Wins_Ovr_LG", "Wins_Home_LG","Wins_Away_LG","Draws_Ovr_LG",
                        "Draws_Home_LG","Draws_Away_LG","Losses_Ovr_LG","Losses_Home_LG","Losses_Away_LG")
    col_names_away2 = c("POS_A","GS_LG_Avg", "GC_LG_Avg", "GD_LG_Avg", "Wins_Ovr_LG", "Wins_Home_LG","Wins_Away_LG","Draws_Ovr_LG",
                        "Draws_Home_LG","Draws_Away_LG","Losses_Ovr_LG","Losses_Home_LG","Losses_Away_LG")
    
    tmp_league_table_values2 <- filter(playing_statistics, Date == date_groups[x+1]) # Select rows from next date group
    
    # Assign values to playing statistics data frame
    for(i in 1:nrow(tmp_league_table_values2)){
      home_team <- tmp_league_table_values2$HomeTeam[i] # get home team
      away_team <- tmp_league_table_values2$AwayTeam[i] # get away team
      
      home_team_values <- league_table[league_table$Team == home_team,] # values for the home team
      away_team_values <- league_table[league_table$Team == away_team,] # values for the away team
      
      ind <- match(tmp_league_table_values2$GAME_NUM[i], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_home2] <- c(home_team_values$POS, GS_LG_Avg, GC_LG_Avg, GD_LG_Avg, Wins_Ovr_LG, Wins_Home_LG,Wins_Away_LG,
                                                    Draws_Ovr_LG,Draws_Home_LG,Draws_Away_LG,Losses_Ovr_LG,Losses_Home_LG,Losses_Away_LG)
      playing_statistics[ind, col_names_away2] <- c(away_team_values$POS, GS_LG_Avg, GC_LG_Avg, GD_LG_Avg, Wins_Ovr_LG, Wins_Home_LG,Wins_Away_LG,
                                                    Draws_Ovr_LG,Draws_Home_LG,Draws_Away_LG,Losses_Ovr_LG,Losses_Home_LG,Losses_Away_LG)
    }
  }
}

################ Assign Home League Table values ################
for (k in seq(from=1, to=7980, by=380)){
  a = k +379
  
  date_groups <- unique(playing_statistics$Date[k:a])
  
  # create empty league table to update
  Team <- unique(playing_statistics$HomeTeam[k:a])
  TP <- 0
  GD <- 0
  GS <- 0
  GC <- 0
  GP <- 0
  
  league_table <- data.frame(Team, TP, GD, GS, GC, GP)
  
  for (x in 1:(length(date_groups)-1)){
    tmp_league_table_values <- filter(league_table_values, Date ==date_groups[x])
    columnsReqLeague = c("HomeTeam","TP_Home_Table", "TGS_Home_Table", "TGC_Home_Table", "TGD_Home_Table",  "N_Home_Game", "Date")
    tmp_league_table_values[columnsReqLeague]
    
    tmp_home <- tmp_league_table_values[c("HomeTeam","TP_Home_Table", "TGS_Home_Table", "TGC_Home_Table", "TGD_Home_Table",  "N_Home_Game", "Date")]
    
    names(tmp_home)[names(tmp_home) == 'HomeTeam'] <- 'Team'
    names(tmp_home)[names(tmp_home) == 'TP_Home_Table'] <- 'TP'
    names(tmp_home)[names(tmp_home) == 'TGD_Home_Table'] <- 'GD'
    names(tmp_home)[names(tmp_home) == 'TGS_Home_Table'] <- 'GS'
    names(tmp_home)[names(tmp_home) == 'TGC_Home_Table'] <- 'GC'
    names(tmp_home)[names(tmp_home) == 'N_Home_Game'] <- 'GP'
    
    tmp_home <- tmp_home[c("Team","TP","GD", "GS", "GC", "GP")]
    
    for (z in 1:nrow(tmp_home)){
      ind <- match(tmp_home$Team[z], league_table$Team)
      
      league_table$TP[ind] <- tmp_home$TP[z]
      league_table$GD[ind] <- tmp_home$GD[z]
      league_table$GS[ind] <- tmp_home$GS[z]
      league_table$GC[ind] <- tmp_home$GC[z]
      league_table$GP[ind] <- tmp_home$GP[z]
    }
    
    league_table <- league_table[order(-league_table$TP,-league_table$GD,-league_table$GS,-league_table$GC),]
    
    league_table$POS <- c(1:20)
    
    Game_Count <- sum(league_table$GP)
    
    GS_LG_Avg_Home <- sum(league_table$GS) / Game_Count
    GC_LG_Avg_Home <- sum(league_table$GS) / Game_Count
    
    col_names_home2 = c("GS_LG_Avg_Home","GC_LG_Avg_Home")
    
    # Now check the next date group and assign position values
    tmp_league_table_values2 <- filter(playing_statistics, Date == date_groups[x+1])
    
    for(i in 1:nrow(tmp_league_table_values2)){
      # get home team
      home_team <- tmp_league_table_values2$HomeTeam[i]
      
      home_team_values <- league_table[league_table$Team == home_team,] 
      
      ind <- match(tmp_league_table_values2$GAME_NUM[i], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_home2] <- c(GS_LG_Avg_Home, GC_LG_Avg_Home)
    }
  }
}

################ Assign Away League Table values ################
for (k in seq(from=1, to=7980, by=380)){
  a = k +379
  
  date_groups <- unique(playing_statistics$Date[k:a])
  
  # create empty league table to update
  Team <- unique(playing_statistics$AwayTeam[k:a])
  TP <- 0
  GD <- 0
  GS <- 0
  GC <- 0
  GP <- 0
  
  league_table <- data.frame(Team, TP, GD, GS, GC, GP)
  
  for (x in 1:(length(date_groups)-1)){
    tmp_league_table_values <- filter(league_table_values, Date ==date_groups[x])
    columnsReqLeague = c("AwayTeam","TP_Away_Table", "TGS_Away_Table", "TGC_Away_Table", "TGD_Away_Table", "N_Away_Game", "Date")
    tmp_league_table_values[columnsReqLeague]
    
    tmp_Away <- tmp_league_table_values[c("AwayTeam","TP_Away_Table", "TGS_Away_Table", "TGC_Away_Table", "TGD_Away_Table", "N_Away_Game", "Date")]
    
    names(tmp_Away)[names(tmp_Away) == 'AwayTeam'] <- 'Team'
    names(tmp_Away)[names(tmp_Away) == 'TP_Away_Table'] <- 'TP'
    names(tmp_Away)[names(tmp_Away) == 'TGD_Away_Table'] <- 'GD'
    names(tmp_Away)[names(tmp_Away) == 'TGS_Away_Table'] <- 'GS'
    names(tmp_Away)[names(tmp_Away) == 'TGC_Away_Table'] <- 'GC'
    names(tmp_Away)[names(tmp_Away) == 'N_Away_Game'] <- 'GP'
    
    
    tmp_Away <- tmp_Away[c("Team","TP","GD", "GS", "GC","GP")]
    
    for (z in 1:nrow(tmp_Away)){
      ind <- match(tmp_Away$Team[z], league_table$Team)
      
      league_table$TP[ind] <- tmp_Away$TP[z]
      league_table$GD[ind] <- tmp_Away$GD[z]
      league_table$GS[ind] <- tmp_Away$GS[z]
      league_table$GC[ind] <- tmp_Away$GC[z]
      league_table$GP[ind] <- tmp_Away$GP[z]
    }
    
    league_table <- league_table[order(-league_table$TP,-league_table$GD,-league_table$GS,-league_table$GC),]
    
    league_table$POS <- c(1:20)
    
    Game_Count <- sum(league_table$GP)
    
    GS_LG_Avg_Away <- sum(league_table$GS) / Game_Count
    GC_LG_Avg_Away <- sum(league_table$GC) / Game_Count

    col_names_Away2 <- c("GS_LG_Avg_Away","GC_LG_Avg_Away")
    
    # Now check the next date group and assign position values
    tmp_league_table_values2 <- filter(playing_statistics, Date == date_groups[x+1])
    
    for(i in 1:nrow(tmp_league_table_values2)){
      # get Away team
      Away_team <- tmp_league_table_values2$AwayTeam[i]
      
      Away_team_values <- league_table[league_table$Team == Away_team,] 
      
      ind <- match(tmp_league_table_values2$GAME_NUM[i], playing_statistics$GAME_NUM)
      playing_statistics[ind, col_names_Away2] <- c(GS_LG_Avg_Away, GC_LG_Avg_Away)
    }
  }
}

################ Assign form against a particular team and general form ################
for (k in 1:7980){
  home_team <- playing_statistics$HomeTeam[k]
  away_team <- playing_statistics$AwayTeam[k]
  
  # identical fixture last 5
  same_fixtures <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & (playing_statistics$HomeTeam == home_team & playing_statistics$AwayTeam == away_team)])[1:5]
  # all fixtures matching teams last 5
  all_fixtures <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & ((playing_statistics$HomeTeam == home_team & playing_statistics$AwayTeam == away_team) | (playing_statistics$HomeTeam == away_team & playing_statistics$AwayTeam == home_team))])[1:5]
  
  all_fixtures_table <- playing_statistics[all_fixtures,c("HomeTeam","AwayTeam","AD","HD")]
  
  #Form against specific opponent
  fixture_home_form <- sum(all_fixtures_table$HD[all_fixtures_table$HomeTeam == home_team],all_fixtures_table$AD[all_fixtures_table$AwayTeam == home_team])/nrow(all_fixtures_table)
  fixture_away_form <- fixture_home_form*-1
  
  playing_statistics$FRM_VO_H[k] <- ifelse(is.na(fixture_home_form),0,fixture_home_form)
  playing_statistics$FRM_VO_A[k] <- ifelse(is.na(fixture_away_form),0,fixture_away_form)
  
  #last five games for home team regardless of opposition
  all_fixtures_home_team <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & (playing_statistics$HomeTeam == home_team | playing_statistics$AwayTeam == home_team)])[1:5]
  #last five home games for home team regardless of opposition
  home_fixtures_home_team <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & (playing_statistics$HomeTeam == home_team)])[1:5]
  
  #last five games for away team regardless of opposition
  all_fixtures_away_team <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & (playing_statistics$HomeTeam == away_team | playing_statistics$AwayTeam == away_team)])[1:5]
  #last five away games for away team regardless of opposition
  away_fixtures_away_team <- rev(playing_statistics$GAME_NUM[playing_statistics$GAME_NUM < k & (playing_statistics$AwayTeam == away_team)])[1:5]
  
  ovr_form_home <- playing_statistics[all_fixtures_home_team,c("HomeTeam","AwayTeam","AD","HD")] # home team form overall
  ovr_form_away <- playing_statistics[all_fixtures_away_team,c("HomeTeam","AwayTeam","AD","HD")] # away team form overall
  home_form_home <- playing_statistics[home_fixtures_home_team,c("HomeTeam","AwayTeam","AD","HD")] # home team form overall
  away_form_away <- playing_statistics[away_fixtures_away_team,c("HomeTeam","AwayTeam","AD","HD")] # away team form overall
  
  #Form in last five games for home team regardless of opposition
  ovr_form_home_val <-sum(c(ovr_form_home$HD[which(ovr_form_home$HomeTeam==home_team)], ovr_form_home$AD[which(ovr_form_home$AwayTeam==home_team)]))/nrow(ovr_form_home)
  #Form in last five games for away team regardless of opposition
  ovr_form_away_val <-sum(c(ovr_form_away$HD[which(ovr_form_away$HomeTeam==away_team)], ovr_form_away$AD[which(ovr_form_away$AwayTeam==away_team)]))/nrow(ovr_form_away)
  #Form in last five home games for home team regardless of opposition
  home_form_val <-sum(c(home_form_home$HD[which(home_form_home$HomeTeam==home_team)], home_form_home$AD[which(home_form_home$AwayTeam==home_team)]))/nrow(home_form_home)
  #Form in last five away games for away team regardless of opposition
  away_form_val <-sum(c(away_form_away$HD[which(away_form_away$HomeTeam==away_team)], away_form_away$AD[which(away_form_away$AwayTeam==away_team)]))/nrow(away_form_away)
  
  playing_statistics$OFRM_H[k] <- ovr_form_home_val
  playing_statistics$OFRM_A[k] <- ovr_form_away_val
  
  playing_statistics$HFRM_H[k] <- home_form_val
  playing_statistics$AFRM_A[k] <- away_form_val
}

########### new values ################
playing_statistics$PD_H <- playing_statistics$TP_H - playing_statistics$TP_A # points difference home team
playing_statistics$PD_A <- playing_statistics$TP_A - playing_statistics$TP_H # points difference away team

playing_statistics$POSD_H <- playing_statistics$POS_H - playing_statistics$POS_A # place difference home team
playing_statistics$POSD_A <- -playing_statistics$POSD_H # place difference away team

playing_statistics$P90_Ovr_H <- playing_statistics$TP_H / playing_statistics$GW_H # points per 90 overall home team
playing_statistics$P90_Ovr_A <- playing_statistics$TP_A / playing_statistics$GW_A # points per 90 overall away team
playing_statistics$P90_home_H <- playing_statistics$TP_H / playing_statistics$N_Home_Game # points per 90 in home games for home team
playing_statistics$P90_away_A <- playing_statistics$TP_A / playing_statistics$N_Away_Game # points per 90 in away games for away team

playing_statistics$GS90_Over_H <- playing_statistics$TGS_H / playing_statistics$GW_H # goals scored per 90 home team
playing_statistics$GS90_Over_A <- playing_statistics$TGS_A / playing_statistics$GW_A # goals scored per 90 away team
playing_statistics$GS90_Home <- playing_statistics$TGS_Home_Table / playing_statistics$N_Home_Game # goals scored per 90 at home home team
playing_statistics$GS90_Away <- playing_statistics$TGS_Away_Table / playing_statistics$N_Away_Game # goals scored per 90 away, away team

playing_statistics$GC90_Over_H <- playing_statistics$TGC_H / playing_statistics$GW_H # goals conceded per 90 home team
playing_statistics$GC90_Over_A <- playing_statistics$TGC_A / playing_statistics$GW_A # goals conceded per 90 away team
playing_statistics$GC90_Home <- playing_statistics$TGC_Home_Table / playing_statistics$N_Home_Game # goals conceded per 90 at home, home team
playing_statistics$GC90_Away <- playing_statistics$TGC_Away_Table / playing_statistics$N_Away_Game # goals conceded per 90 away, away team

# ATT & DEF power ratings, used for XG check the website for calculation
# Home team
playing_statistics$ATT_H_ovr <- playing_statistics$GS90_Over_H / playing_statistics$GS_LG_Avg # overall
playing_statistics$DEF_H_ovr <- playing_statistics$GC90_Over_H / playing_statistics$GC_LG_Avg # overall
playing_statistics$ATT_H <- playing_statistics$GS90_Home / playing_statistics$GS_LG_Avg_Home # home
playing_statistics$DEF_H <- playing_statistics$GC90_Home / playing_statistics$GC_LG_Avg_Home # home

# Away
playing_statistics$ATT_A_ovr <- playing_statistics$GC90_Over_A / playing_statistics$GS_LG_Avg # overall
playing_statistics$DEF_A_ovr <- playing_statistics$GC90_Over_A / playing_statistics$GC_LG_Avg # overall
playing_statistics$ATT_A <- playing_statistics$GS90_Away / playing_statistics$GS_LG_Avg_Away # away
playing_statistics$DEF_A <- playing_statistics$GC90_Away / playing_statistics$GC_LG_Avg_Away # away

# XG and XCG
# Home
playing_statistics$XG_OVR_H <- playing_statistics$GS_LG_Avg * playing_statistics$ATT_H_ovr * playing_statistics$DEF_A_ovr # overall
playing_statistics$XGC_OVR_H <- playing_statistics$GC_LG_Avg * playing_statistics$ATT_A_ovr * playing_statistics$DEF_H_ovr # overall
playing_statistics$XG_H <- playing_statistics$GS_LG_Avg_Home * playing_statistics$ATT_H * playing_statistics$DEF_A # Home def_a
playing_statistics$XGC_H <- playing_statistics$GC_LG_Avg_Home * playing_statistics$ATT_A * playing_statistics$DEF_H # Home Att_a

# Away
playing_statistics$XG_OVR_A <- playing_statistics$GS_LG_Avg * playing_statistics$ATT_A_ovr * playing_statistics$DEF_H_ovr # overall
playing_statistics$XGC_OVR_A <- playing_statistics$GC_LG_Avg * playing_statistics$ATT_H_ovr * playing_statistics$DEF_A_ovr # overall
playing_statistics$XG_A <- playing_statistics$GS_LG_Avg_Away * playing_statistics$ATT_A * playing_statistics$DEF_H # away
playing_statistics$XGC_A <- playing_statistics$GC_LG_Avg_Away * playing_statistics$ATT_H * playing_statistics$DEF_A # away

playing_statistics$ELO_H <- 1000 # Home ELO
playing_statistics$ELO_A <- 1000 # Away ELO
  
playing_statistics$HOR_H <- 1000 # home off rating 
playing_statistics$AOR_A <- 1000 # away off rating

playing_statistics$HDR_H <- 1000 # home def rating
playing_statistics$ADR_A <- 1000 # away def rating

playing_statistics[playing_statistics == 'Inf'] <- 0
playing_statistics[is.na(playing_statistics)] <- 0


cols_xGNS_ovr_H <- c("FTHG","P90_Ovr_H","POSD_H","PD_H","HC","HF","OFRM_H")#,"HO","HBP","FRM_VO_H") # feature selection column names
cols_xGNS_ovr_A <- c("FTAG","P90_Ovr_A","POSD_A","PD_A","AC","AF","OFRM_A")#,"AO","ABP","FRM_VO_A") # feature selection column names
cols_xGNS_H <- c("FTHG","P90_home_H","HC","HF","HFRM_H")#,"HO","HBP","FRM_VO_H") # feature selection column names
cols_xGNS_A <- c("FTAG","P90_away_A","AC","AF","AFRM_A")#,"AO","ABP","FRM_VO_A") # feature selection column names


important_cols <- unique(c(cols_xGNS_ovr_H,cols_xGNS_ovr_A,cols_xGNS_H,cols_xGNS_A,"SZN"))
summarised_table <- playing_statistics[,important_cols]

#Z-scaling
for (season_n in 2001:2021){
  season_games <- summarised_table[which(summarised_table$SZN==season_n),]
  indexes <- which(summarised_table$SZN==season_n)
  summarised_table$P90_Ovr_H[indexes] <- ((season_games$P90_Ovr_H-mean(season_games$P90_Ovr_H))/sd(season_games$P90_Ovr_H))
  summarised_table$POSD_H[indexes] <- ((season_games$POSD_H-mean(season_games$POSD_H))/sd(season_games$POSD_H))
  summarised_table$PD_H[indexes] <- ((season_games$PD_H-mean(season_games$PD_H))/sd(season_games$PD_H))
  summarised_table$HC[indexes] <- ((season_games$HC-mean(season_games$HC))/sd(season_games$HC))
  summarised_table$HF[indexes] <- ((season_games$HF-mean(season_games$HF))/sd(season_games$HF))
  summarised_table$OFRM_H[indexes] <- ((season_games$OFRM_H-mean(season_games$OFRM_H))/sd(season_games$OFRM_H))
 # summarised_table$FRM_VO_H[indexes] <- ((season_games$FRM_VO_H-mean(season_games$FRM_VO_H))/sd(season_games$FRM_VO_H))

  summarised_table$P90_Ovr_A[indexes] <- ((season_games$P90_Ovr_A-mean(season_games$P90_Ovr_A))/sd(season_games$P90_Ovr_A))
  summarised_table$POSD_A[indexes] <- ((season_games$POSD_A-mean(season_games$POSD_A))/sd(season_games$POSD_A))
  summarised_table$PD_A[indexes] <- ((season_games$PD_A-mean(season_games$PD_A))/sd(season_games$PD_A))
  summarised_table$AC[indexes] <- ((season_games$AC-mean(season_games$AC))/sd(season_games$AC))
  summarised_table$AF[indexes] <- ((season_games$AF-mean(season_games$AF))/sd(season_games$AF))
  #summarised_table$FRM_VO_A[indexes] <- ((season_games$FRM_VO_A-mean(season_games$FRM_VO_A))/sd(season_games$FRM_VO_A))
}


#Non shot features

xGNS_ovr_H_features <- summarised_table[cols_xGNS_ovr_H]
xGNS_ovr_A_features <- summarised_table[cols_xGNS_ovr_A]
xGNS_H_features <- summarised_table[cols_xGNS_H]
xGNS_A_features <- summarised_table[cols_xGNS_A]

#Write Non shot features

write.csv(xGNS_ovr_H_features,xGNS_ovr_H_features_file, row.names = FALSE)
write.csv(xGNS_ovr_A_features,xGNS_ovr_A_features_file, row.names = FALSE)
write.csv(xGNS_H_features,xGNS_H_features_file, row.names = FALSE)
write.csv(xGNS_A_features,xGNS_A_features_file, row.names = FALSE)


################ Pre processing functions ######################

source("KNN/knn_reg.R")
predict_xGNS_ovr_H <- knn_reg( xGNS_ovr_H_features, TRUE)
predict_xGNS_H <- knn_reg( xGNS_H_features, TRUE)
predict_xGNS_ovr_A <- knn_reg( xGNS_ovr_A_features, FALSE)
predict_xGNS_A <- knn_reg(xGNS_A_features, FALSE)

playing_statistics$xGNS_ovr_H <- predict_xGNS_ovr_H$matrix.unlist.pred_y...nrow...length.pred_y...byrow...TRUE.
playing_statistics$xGNS_H <- predict_xGNS_H$matrix.unlist.pred_y...nrow...length.pred_y...byrow...TRUE.
playing_statistics$xGNS_ovr_A <- predict_xGNS_ovr_A$matrix.unlist.pred_y...nrow...length.pred_y...byrow...TRUE.
playing_statistics$xGNS_A <- predict_xGNS_A$matrix.unlist.pred_y...nrow...length.pred_y...byrow...TRUE.
##################### ELO calculations #################



# weighting formula for xG and NSxG
playing_statistics$weight_att_p_H <- 0
playing_statistics$weight_att_p_H = ((2*playing_statistics$FTHG)-playing_statistics$xGNS_H)/(playing_statistics$XG_H-playing_statistics$xGNS_H) # home weight att
playing_statistics$weight_att_p_A = ((2*playing_statistics$FTAG)-playing_statistics$xGNS_A)/(playing_statistics$XG_A-playing_statistics$xGNS_A) # Away weight att

playing_statistics$weight_def_p_H = ((2*playing_statistics$FTAG)-playing_statistics$xGNS_A)/(playing_statistics$XGC_H-playing_statistics$xGNS_A) # home weight def
playing_statistics$weight_def_p_A = ((2*playing_statistics$FTHG)-playing_statistics$xGNS_H)/(playing_statistics$XGC_A-playing_statistics$xGNS_H) # Away weight def

# ELO xG's rating
playing_statistics$weight_ELO_H = ((2*playing_statistics$FTHG)-playing_statistics$xGNS_ovr_H)/(playing_statistics$XG_OVR_H-playing_statistics$xGNS_ovr_H) # home weight
playing_statistics$Weight_ELO_A = ((2*playing_statistics$FTHG)-playing_statistics$xGNS_ovr_A)/(playing_statistics$XG_OVR_A-playing_statistics$xGNS_ovr_A) # Away weight

match_importance = 50

for (k in seq(from=1, to=7980, by=380)){
  a = k +379
  
  # set values to 1000 for k to a
  playing_statistics$ELO_H[k:a] <- 1000 # Home ELO
  playing_statistics$ELO_A[k:a] <- 1000 # Away ELO
  
  playing_statistics$HOR_H[k:a] <- 1000 # home off rating 
  playing_statistics$AOR_A[k:a] <- 1000 # away off rating
  
  playing_statistics$HDR_H[k:a] <- 1000 # home def rating
  playing_statistics$ADR_A[k:a] <- 1000 # away def rating
  
  # from a to k
  for (j in k:a) {
  
    home_team <- playing_statistics$HomeTeam[j]
    away_team <- playing_statistics$AwayTeam[j]
    
    col_names_home <- c("ELO_H","HOR_H","HDR_H") # added custom game weeks per team for later calcs
    col_names_away <- c("ELO_A","AOR_A","ADR_A") # game week is total number of games home and away count separate
    
    # Home
    playing_statistics$HOR_H[j] <- playing_statistics$HOR_H[j] + (playing_statistics$FTHG[j] - (playing_statistics$XG_H[j] + (playing_statistics$xGNS_H[j]/2))) # off
    playing_statistics$HDR_H[j] <- playing_statistics$HDR_H[j] + (playing_statistics$FTAG[j] - (playing_statistics$XGC_H[j] + (playing_statistics$xGNS_A[j]/2))) # def
    
    # away
    playing_statistics$AOR_A[j] <- playing_statistics$AOR_A[j] + (playing_statistics$FTAG[j] - (playing_statistics$XG_A[j] + (playing_statistics$xGNS_A[j]/2))) # off
    playing_statistics$ADR_A[j] <- playing_statistics$AOR_A[j] + (playing_statistics$FTHG[j] - (playing_statistics$XGC_A[j] + (playing_statistics$xGNS_H[j]/2))) # def
    
    # weights
    playing_statistics$Weight_H[j] <- (playing_statistics$HOR_H[j] - playing_statistics$ADR_A[j]) + (playing_statistics$HDR_H[j] - playing_statistics$AOR_A[j])
    playing_statistics$Weight_A[j] <- (playing_statistics$AOR_A[j] - playing_statistics$HDR_H[j]) + (playing_statistics$ADR_A[j] - playing_statistics$HOR_H[j])
    
    playing_statistics$ELO_A[j] <- playing_statistics$ELO_A[j] + match_importance*(playing_statistics$FTAG[j] - playing_statistics$Weight_A[j]*(playing_statistics$XG_OVR_A[j] + (playing_statistics$xGNS_ovr_A[j]/2))) # overall
    playing_statistics$ELO_H[j] <- playing_statistics$ELO_H[j] + match_importance*(playing_statistics$FTHG[j] - playing_statistics$Weight_H[j]*(playing_statistics$XG_OVR_H[j] + (playing_statistics$xGNS_ovr_H[j]/2))) # overall
    
    # Home
    home_vec <- c(playing_statistics$ELO_H[j], playing_statistics$HOR_H[j], playing_statistics$HDR_H[j])
    
    # find the next fixture when team == home or away
    idx <- which((playing_statistics$HomeTeam == home_team | playing_statistics$AwayTeam == home_team) & playing_statistics$GAME_NUM > k)[1]
    
    if(!is.na(idx)){
      col_names_vec <- if(playing_statistics$HomeTeam[idx]==home_team) col_names_home else col_names_away
      
      playing_statistics[idx, col_names_vec] <- home_vec
    }
  
    # away
    
    away_vec <- c(playing_statistics$ELO_A[j], playing_statistics$HOR_A[j], playing_statistics$HDR_A[j])
    
    # find the next fixture when team == home or away
    idx <- which((playing_statistics$HomeTeam == away_team | playing_statistics$AwayTeam == away_team) & playing_statistics$GAME_NUM > k)[1]  
    
    if(!is.na(idx)){
      col_names_vec <- if(playing_statistics$HomeTeam[idx]==away_team) col_names_home else col_names_away
      
      playing_statistics[idx, col_names_vec] <- away_vec
    }
  }
}


class_feature_cols <- c("FTR","ELO_H","ELO_A") # feature selection column names
reg_H_features_cols <- c("FTHG","ELO_H","ELO_A") # feature selection column names
reg_A_features_cols <- c("FTAG","ELO_H","ELO_A") # feature selection column names


summarised_class_feat_table <- playing_statistics[,unique(c("HomeTeam","AwayTeam","SZN","GAME_NUM",class_feature_cols,reg_H_features_cols,reg_A_features_cols))]

# Delete first 5 games used to calculate form for each team to avoid inaccuracies due to form calculations
remove_first_five <- function(table){
  deleted_rows <- c()
  team_list <- unique(c(table$HomeTeam,table$AwayTeam))
  for(team in team_list){
    deleted_rows<- c(deleted_rows,table$GAME_NUM[which(table$HomeTeam == team | table$AwayTeam== team)][1:5])
  }
  unique(deleted_rows)
}
all_deletions <- c()
for (i in SEASONS){
  all_deletions<- c(all_deletions,remove_first_five(summarised_class_feat_table[which(summarised_class_feat_table$SZN==i),]))
}
all_deletions <- unique(all_deletions)

summarised_class_feat_table <- summarised_class_feat_table[-all_deletions,]

# Delete first season used to calculate form for each team to avoid inaccuracies due to form calculations

summarised_class_feat_table <- summarised_class_feat_table[-which(summarised_class_feat_table$SZN==2001),]
class_feature <- summarised_class_feat_table[class_feature_cols]
reg_H_features <- summarised_class_feat_table[reg_H_features_cols]
reg_A_features <- summarised_class_feat_table[reg_A_features_cols]

#Graph Code

# 
#view first six rows of normalized iris dataset
chelsea <- playing_statistics[which(playing_statistics$HomeTeam=="Chelsea" | playing_statistics$AwayTeam=="Chelsea"),important_cols[1:10]]
team_h <- playing_statistics[which(playing_statistics$HomeTeam=="Charlton"& playing_statistics$SZN==2001),unique(c("ELO_H","ELO_A","GAME_NUM"))]

goals <- c()
percentage_wins_h <- c()
percentage_draws_h <- c()
percentage_loss_h <- c()
form_h <- c()
for(i in SEASONS){
  goals <- c(goals, mean(playing_statistics$FTHG[which(playing_statistics$SZN==i)]))
  game_spread <- playing_statistics$HP[which(playing_statistics$SZN==i)]
  percentage_wins_h <- c(percentage_wins_h, sum(game_spread==3)/length(game_spread))
  percentage_draws_h <- c(percentage_draws_h, sum(game_spread==1)/length(game_spread))
  percentage_loss_h <- c(percentage_loss_h, sum(game_spread==0)/length(game_spread))
  form_h <- c(form_h, mean(playing_statistics$OFRM_H[which(playing_statistics$SZN==i)]))
}


mean_ns <- data.frame(season=SEASONS,percent_wins = percentage_wins_h,rating = form_h,percent_draws = percentage_draws_h,percent_loss = percentage_loss_h )

test <- data.frame(season = rep(c(SEASONS),3),Result= rep(c("Win","Loss","Draw"),each =21), percentage= c(percentage_wins_h,percentage_loss_h,percentage_draws_h))
ggplot(test, aes(x=season,percentage)) + 
  geom_line(aes(color = Result))+scale_color_manual(values = c("orange", "blue","red"))+xlab("Season")+ylab("Percentage")


ggplot(data=mean_ns, aes(x= rating, y=percent_wins)) +geom_point()+geom_smooth()

ggplot(playing_statistics, aes(x = PD_H))+geom_histogram()
ggplot(team_h,aes(x=GAME_NUM)) +geom_line(aes(y=ELO_H),color = "red") +geom_line(aes(y=ELO_A),color = "blue") +xlab("Game Number")+ylab("ELO Rating")+ylim(0,2000)
pairs.panels(chelsea,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)
test <- data.frame(pos_diff = playing_statistics$POSD_H, goals = playing_statistics$GS90_Over_H)
pairs.panels(test,
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)

h_goals <- c()
a_goals <- c()
t_goals <- c()
x_goals_h <- c()
x_goals_a <- c()
avg_form_h <- c()
avg_form_a <- c()
t_ac <- c()
t_hc <- c() 
t_ho <- c()
for (i in 2001:2021){
  sum_h_goals<- sum(playing_statistics$FTHG[playing_statistics$SZN == i])
  sum_a_goals <- sum(playing_statistics$FTAG[playing_statistics$SZN == i])
  sum_xh_goals<- sum(playing_statistics$XG_OVR_H[playing_statistics$SZN == i])
  sum_xa_goals <- sum(playing_statistics$XG_OVR_A[playing_statistics$SZN == i])
  
  sum_h_form<- mean(playing_statistics$OFRM_H[playing_statistics$SZN == i])
  sum_a_form <- mean(playing_statistics$OFRM_A[playing_statistics$SZN == i])
  
  sum_ac <- sum(playing_statistics$AC[playing_statistics$SZN == i])
  sum_hc <- sum(playing_statistics$HC[playing_statistics$SZN == i])
  sum_ho <- sum(playing_statistics$HO[playing_statistics$SZN == i])
  
  h_goals <- c(h_goals,sum_h_goals)
  a_goals <- c(a_goals,sum_a_goals)
  t_ac <- c(t_ac,sum_ac)
  t_hc <- c(t_hc,sum_hc)
  t_ho <- c(t_ho,sum_ho)
  avg_form_h <-c(avg_form_h,sum_h_form)
  avg_form_a <-c(avg_form_a,sum_a_form)
  x_goals_h <- c(x_goals_h,sum_xh_goals)
  x_goals_a <- c(x_goals_a,sum_xa_goals)
}

seasons <-c(2001:2021)
sum_table <- data.frame(seasons = seasons,place=rep(c("Home", "Away"), each=21),goals = c(h_goals,a_goals),xgoals_h=rep(x_goals_h,2),xgoals_a=rep(x_goals_a,2),form=c(sum_h_form,sum_h_form))


ggplot(sum_table, aes(fill=place, y=goals, x=seasons)) + 
  geom_bar(position="stack", stat="identity")

ggplot(sum_table[1:20,], aes(x=xgoals_h, y=goals, label=seasons)) + geom_point(colour='salmon')+labs(x = "Home Expected Goals",y="Home Goals")+ geom_smooth(method = "lm",  colour="salmon")
ggplot(sum_table[22:42,], aes(x=x_goals_a, y=goals)) + geom_point(colour='#00bfc4') +geom_smooth(method = "lm",  colour="#00bfc4")+labs(x = "Away Expected Goals",y="Away Goals")

A = sum_table[1:21,]
B = sum_table[22:42,]
ggplot(A,aes(x=x_goals_h,y=goals)) +geom_point()+geom_smooth(method=lm) +geom_point(data=B,colour='red')+ geom_smooth(method=lm)

#Form Home histogram
ggplot(playing_statistics, aes(x=OFRM_H)) +  geom_histogram(fill="blue", alpha=0.5, position="identity")+labs(x = "Overall Form for Home Team",y="Games") +xlim(-3, 3)
#Form Away histogram
ggplot(playing_statistics, aes(x=OFRM_A)) +  geom_histogram(fill="red", alpha=0.5, position="identity")+labs(x = "Overall Form for Away Team",y="Games") +xlim(-3, 3)


h_goals_vs_ac <- data.frame(home_goals = h_goals, ac = t_ac)
ggplot(h_goals_vs_ac,aes(x=t_ac,y=home_goals))+geom_point()+geom_smooth(method=lm)+xlab("Total Away Corners")+ylab("Home Goals")

#File names
class_feature_file <- paste(getwd(),MODELTYPE,"class_feature.csv",sep = "/")
reg_H_features_file <- paste(getwd(),MODELTYPE,"reg_H_features.csv",sep = "/")
reg_A_features_file <- paste(getwd(),MODELTYPE,"reg_A_features.csv",sep = "/")


#Write class features for KNN
write.csv(class_feature,class_feature_file, row.names = FALSE)
write.csv(reg_H_features,reg_H_features_file, row.names = FALSE)
write.csv(reg_A_features,reg_A_features_file, row.names = FALSE)