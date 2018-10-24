#draw required libraries
library(Rsymphony)
library(data.table)
library(plyr)
library(stringi)
library(rvest)
library(rstudioapi)
library(XML)
library(xml2)
library(dplyr)
library(magrittr)
library(neuralnet)
library(compare)
library(gtools)
library(fuzzyjoin)
require(compiler)
library(stringdist)
library(rlist)
library(utils)
enableJIT(3)

#setwd
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#~~~~~~~~STEP 1: BUILD DATAFRAME TO FEED INTO LINEUP BUILDER~~~~~~~~##

#write DK file to R

dk_file <- read.csv(file = '../Historical Data and Projections/DKSalaries.csv', 
                    header=TRUE, 
                    sep=",",
                    stringsAsFactors = FALSE)

#write team abbreviation table to R

abbrev_table <- read.csv(file = '../Historical Data and Projections/Team Abbreviation Table.csv', 
                         header=TRUE, 
                         sep=",",
                         stringsAsFactors = FALSE)

#Update dk file team names. Can be used to confirm starters later.
#***Confirm all Vegas abbreviations are correct***
dk_file <- merge(dk_file, abbrev_table, by = 'TeamAbbrev')
dk_file <- dk_file[,c('Name', 'Salary', 'Position', 'AvgPointsPerGame', 'Team')]
colnames(dk_file)[colnames(dk_file) == 'Team'] <- 'teamAbbrev'

#Import necessary webscraping sources
#Currently using rotogrinders and rotowire for this
#Could switch to DailyFaceoff or LeftWingLock in the future if necessary
#DailyFaceoff <- read_html("http://www.dailyfaceoff.com/teams/")
#LeftWingLock - NEED TO UPDATE IF WE WANT TO BE ABLE TO AUTOMATE CONFIRMING LINEUPS 
#lwl_starters <- read_html("https://leftwinglock.com/line-combinations/[TEAM NAME]/?team=colorado-avalanche&strength=EV&gametype=GD)

#RotoGrinders - Goalies, Full Strength lines, Vegas
rotogrinders <- read_html("https://rotogrinders.com/lineups/nhl?site=draftkings")
#Rotowire - Power Play lines
rotowire <- read_html("https://www.rotowire.com/hockey/nhl_lineups.htm")
#NumberFire - Player Projections
numberfire <- read_html("https://www.numberfire.com/nhl/daily-fantasy/daily-hockey-projections")
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Skater Work~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##


NF_skaters <- html_text(html_nodes(numberfire, ".player-info .full"))
NF_position <- html_text(html_nodes(numberfire, ".player-info--position"))
#Couldn't get the below projection pull to work; need to figure out how to change drop down on a webpage.
#NF_proj <- html_text(html_nodes(numberfire, ".fpts"))
NF_shots_pg <- html_text(html_nodes(numberfire, ".s"))
NF_goals_pg <- html_text(html_nodes(numberfire, ".g"))
NF_assists_pg <- html_text(html_nodes(numberfire, ".ast"))
NF_ppgoals_pg <- html_text(html_nodes(numberfire, ".ppg"))
NF_ppassists_pg <- html_text(html_nodes(numberfire, ".ppa"))
NF_blocks_pg <- html_text(html_nodes(numberfire, ".blk"))
NF_mins_pg <- html_text(html_nodes(numberfire, ".min"))
NF_skaters_team <- html_text(html_nodes(numberfire, ".team-player__team.active"))
#NF_[insert] <- html_text(html_nodes(numberfire, "[insert]"))

#Build NumberFire dataframe
NF_df <- data.frame(Player = as.character(NF_skaters),
                    Position = as.character(NF_position),
                    Team = as.character(NF_skaters_team),
                    MinsPG = as.numeric(as.character(NF_mins_pg)),
                    ShotsPG = as.numeric(as.character(NF_shots_pg)),
                    GoalsPG = as.numeric(as.character(NF_goals_pg)),
                    PPGoalsPG = as.numeric(as.character(NF_ppgoals_pg)),
                    AssistsPG = as.numeric(as.character(NF_assists_pg)),
                    PPAsistsPG = as.numeric(as.character(NF_ppassists_pg)),
                    BlocksPG = as.numeric(as.character(NF_blocks_pg))
                    )

DK2NF.dist.name <- sapply(dk_file$Name, stringdist, NF_df$Player, method = "lcs")


#Rotogrinders data
skaters <- html_text(html_nodes(rotogrinders, ".pname .player-popup"))
skater_pos <- html_text(html_nodes(rotogrinders, ".position"))

teams <- html_text(html_nodes(rotogrinders, ".mascot"))
teams_list <- rep(teams[1:length(teams)], each = 18)

goalies <- html_text(html_nodes(rotogrinders, ".pitcher .player-popup"))
goalie_pos <- rep(c("G"), length(goalies))

all_players <- data.frame(Player = append(skaters, goalies), 
                          Position = append(skater_pos,goalie_pos))

#Write RG players list to .csv 
#and adjust manually if necessary to make it 100% correct
write.csv(all_players, file = 'all_players.csv')

##~~~~~~STOP~STOP~STOP~STOP~~~~~~~FIRST STOP HERE~~~~~~~STOP~STOP~STOP~STOP~~~~~~~##

##Need to:
##    --Open the csv file saved in the Dropbox directory
##    --Add players who's name are null (unclickable on 'rotogrinders' link page)
##    --Make sure all positions are either C, W, D, or G (all must be capitalized)
##    --Save the file and close it

#Re-uploads players list
all_players <- read.csv(file = 'all_players.csv')

#Add Teams
all_players <- data.frame(Player = all_players$Player, 
                          Position = all_players$Position,
                          Team = append(teams_list, teams))

#Fuzzy match names between DK and Rotogrinders as well as DK and Rotowire
all_players_list <- all_players$Player

DK2RG.dist.name <- sapply(dk_file$Name, stringdist, all_players_list, method = "lcs")

pp_skaters <- html_text(html_nodes(rotowire, ":nth-child(13) a , .lineup__player:nth-child(12) a, .lineup__player:nth-child(11) a, .lineup__player:nth-child(10) a, .lineup__player:nth-child(9) a, .lineup__player:nth-child(7) a, .lineup__player:nth-child(6) a, .lineup__player:nth-child(5) a, .lineup__player:nth-child(4) a, .lineup__player:nth-child(3) a"))
pp_skaters <- data.frame(Skater = pp_skaters)
#pp_skaters <- tail(pp_skaters, -1)
#pp_skaters <- head(pp_skaters, -4)

write.csv(pp_skaters, file = 'pp_skaters.csv')

##~~~~~~STOP~STOP~STOP~STOP~~~~~~~SECOND STOP HERE~~~~~~~STOP~STOP~STOP~STOP~~~~~~~##

##ONLY NEED TO STOP IF ROTOWIRE IS INCORRECT WHICH IT RARELY IS
##    --Open the csv file saved in the Dropbox directory
##    --Add players who's name are null (unclickable on 'rotogrinders' link page)
##    --Save the file and close it

#Re-uploads pp_players list
pp_skaters <- read.csv(file = 'pp_skaters.csv')

DK2RW.dist.name <- sapply(dk_file$Name, stringdist, pp_skaters$Skater, method = "lcs")

#Read in additional webscrapings to be used to create a combined dataframe

line_list <- c("Line1", "Line1", "Line1", 
               "Line2", "Line2", "Line2", 
               "Line3", "Line3", "Line3", 
               "Line4", "Line4", "Line4", 
               "D1", "D1", "D2", "D2", "D3", "D3")
line_list <- rep(line_list, length(teams_list) / 18)

pp_list <- c("PP1", "PP1", "PP1", "PP1", "PP1", 
             "PP2", "PP2", "PP2", "PP2", "PP2")
pp_list <- rep(pp_list, length(pp_skaters$Skater) / 10)

implied_goals <- html_text(html_nodes(rotogrinders, ".ou a"))

team_goals <- data.frame(Team = teams,
                         Implied_goals = implied_goals)

#Create Rotowire dataframe (Power Play data)
DK2RW.match.name <- data.frame(DK_Name = colnames(DK2RW.dist.name)[apply(DK2RW.dist.name,1,which.min)],
                               PP_Line = pp_list)

#Create Rotogrinders dataframe (Full Strength Line data)
DK2RG.match.name <- data.frame(Name = colnames(DK2RG.dist.name)[apply(DK2RG.dist.name,1,which.min)], 
                            RG_Position = all_players$Position,
                            Team = all_players$Team,
                            Line = append(line_list, goalie_pos))

#Create NumberFire dataframe (Player Stats data)
DK2NF.match.name <- data.frame(Name = colnames(DK2NF.dist.name)[apply(DK2NF.dist.name,1,which.min)], 
                               MinsPG = NF_df$MinsPG,
                               ShotsPG = NF_df$ShotsPG,
                               GoalsPG = NF_df$GoalsPG,
                               PPGoalsPG = NF_df$PPGoalsPG,
                               AssistsPG = NF_df$AssistsPG,
                               PPAssistsPG = NF_df$PPAsistsPG,
                               BlocksPG = NF_df$BlocksPG)


#Merge DK and Rotogrinders dataframes
player_file <- merge(dk_file, DK2RG.match.name, by = "Name")

#Merge player_file and team implied goal dataframe
player_file <- merge(player_file, team_goals, by.x = "teamAbbrev", by.y = "Team")

#Remove rows where DK Team and RG Team don't match
player_file <- player_file[player_file$teamAbbrev==player_file$Team,]

#Merge player_file with Rotowire dataframe
player_file <- merge(player_file, 
                     DK2RW.match.name, 
                     by.x="Name", 
                     by.y="DK_Name", 
                     all.x=TRUE)

#Merge player_file with NumberFire dataframe
player_file <- merge(player_file, DK2NF.match.name, by = "Name")

#Delete DK_Position and RG_Name columns 
player_file <- player_file[,c("Name",
                              "Salary",
                              "RG_Position", 
                              "Line", 
                              "PP_Line", 
                              "Team",
                              "Implied_goals",
                              "AvgPointsPerGame",
                              "MinsPG",
                              "ShotsPG",
                              "GoalsPG",
                              "PPGoalsPG",
                              "AssistsPG",
                              "PPAssistsPG",
                              "BlocksPG")]

#Select only skaters who are on Line1 or Line2 
#                             or D1 or D2 
#                             or on a PP Line
#                             or a Goalie
#Currently turned off, but an option if we want to use. 
#Guessing that ~95% of players we want will be in this group for cash games
#player_file <- player_file[(player_file$Line=='Line1' | 
#                               player_file$Line=='Line2' |
#                               player_file$Line=='D1' |
#                               player_file$Line=='D2') | 
#                              (player_file$PP_Line=='PP1' | 
#                                 player_file$PP_Line=='PP2') |
#                             player_file$Line=='G',]
#Remove rows where Name is NA
player_file <- player_file[!is.na(player_file$Name),]

#Remove rows with duplicated names
player_file <- player_file[!duplicated(player_file$Name),]

#Add metric calculations
player_file[,'PlayerVal'] <- ((player_file$AvgPointsPerGame**1.3) / as.numeric(as.character(player_file$Salary))) * (as.numeric(as.character(player_file$Implied_goals))**(1/2))

#Order by value calculation
player_file <- player_file[order(-player_file[,'PlayerVal']),]


#Export file to Dropbox
save_date = format(Sys.time(),"%m-%d-%Y")
write.csv(player_file, file = paste('Player_Data -', save_date, '.csv'))

#Use some group by's to understand which team/line has best value of the night
#Full Strength Lines
line_file <- player_file %>%
              group_by(Team, Line) %>%
              summarize("AvgLinePts" = sum(AvgPointsPerGame, na.rm=T),
                        "TotalSalary" = sum(Salary, na.rm=T),
                        "AvgPtsPerPlayer" = mean(AvgPointsPerGame, na.rm = T),
                        "AvgSalaryPerPlayer" = mean(Salary, na.rm = T)) %>%
              filter(Line %in% c("Line1", "Line2", "Line3", "Line4"))
line_file <- merge(line_file, team_goals, by="Team")
line_file[,'LineVal'] <- (line_file$AvgLinePts**1.5) / line_file$TotalSalary
line_file <- line_file[order(-line_file[,'LineVal']),]

#PP Lines
PP_line_file <- player_file %>%
  group_by(Team, PP_Line) %>%
  summarize("AvgLinePts" = sum(AvgPointsPerGame, na.rm=T),
            "TotalSalary" = sum(Salary, na.rm=T),
            "AvgPtsPerPlayer" = mean(AvgPointsPerGame, na.rm = T),
            "AvgSalaryPerPlayer" = mean(Salary, na.rm = T)) %>%
  filter(!is.na(PP_Line))
PP_line_file <- merge(PP_line_file, team_goals, by="Team")
PP_line_file[,'PPLineVal'] <- (PP_line_file$AvgLinePts**1.5) / PP_line_file$TotalSalary
PP_line_file <- PP_line_file[order(-PP_line_file[,'PPLineVal']),]

#write.csv(line_file, file = paste('line_file -', save_date, '.csv'))
#write.csv(PP_line_file, file = paste('PP_line_file -', save_date, '.csv'))

##~~~~~~~~~~~~~~~~~~~~~~STEP 2: CREATE LINEUPS~~~~~~~~~~~~~~~~~~~~~~~~##
#Notes from what I understand:
#   -Don't understand the matrix creation or the while loop optimization script. Need to talk through
#   -Shouldn't use the projection step method for finding lineups in this case
#     -> Want to make lineups with all unique combinations of 3 skater full strength lines (plus 2 D and 1 G)
#     -> Don't need to use RSymphony (optimization)? 
#     -> Really just need to iterate over a list of lines, plugging in the correct players

#Order by Team, then Line, then Position
player_file <- player_file[order(player_file[,'Team'], player_file[,'Line'], player_file[,'Position']),]

#Create all possible 2 line combinations of 6 players each




