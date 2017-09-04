# Team attributes data

#Combining the Team and Team_Attributes datasets
Team_Attributes <- inner_join(Team, Team_Attributes, by = 'team_api_id')

#Deleting repeating and unecessary columns
Team_Attributes <- Team_Attributes[, -c(6:7)]

#Combining Matches with their respective leagues
Match <- inner_join(League, Match , by = 'country_id')

#Removing repeating and uncessesary columns
Match <- Match[, -c(86:97)]

#Make a table for EPL games for the 2015-16 season
EPL15_16 <- Match %>% filter(League == 'England Premier League' & season == '2015/2016')

#Matching Team_api_id with the respective team names in the Team_Attributes table for teams that participate in the EPL 2015-16 season
Team_id <- as.data.frame(unique(EPL15_16$home_team_api_id)) 
colnames(Team_id) <- "team_api_id"

EPLTeams <- inner_join(Team_id, Team_Attributes)
EPLTeams$year <- year(EPLTeams$date)
EPLTeams <- EPLTeams %>% filter(year == 2015) 

#Radar chart for the EPL teams
EPLRadar <- EPLTeams[, c(4, 7, 9, 11, 14, 16, 18, 21, 23, 25)]
EPLRadar <- EPLRadar %>% gather(key = 'Attribute', value = 'Rating', -team_long_name) %>% spread(key = team_long_name, value = Rating)

#Radar chart of EPL teams for the 2015-16 season
chartJSRadar(EPLRadar, maxScale = 80, scaleStartValue = 20, main = 'EPL Teams 2015/16')
