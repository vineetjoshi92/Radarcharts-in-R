#Import libraries
#install.packages('RSQLite')
#install.packages('lubridate')
#install.packages('radarchart')
library(RSQLite)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(radarchart)

#Connect to the database
con <- dbConnect(SQLite(), dbname = "database.sqlite")

#List of tables
all_tables <- dbListTables(con)

#Getting the tables
Country <- dbGetQuery(con, 'Select * from Country')
League <- dbGetQuery(con, 'Select * from League')
Match <- dbGetQuery(con, 'Select * from Match')
Player <- dbGetQuery(con, 'Select * from Player')
Player_Attributes <- dbGetQuery(con, 'Select * from Player_Attributes')
Team <- dbGetQuery(con, 'Select * from Team')
Team_Attributes <- dbGetQuery(con, 'Select * from Team_Attributes')

#Disconnecting from the database
dbDisconnect(con)

#Combining League and Country and deleting repeated columns
colnames(League)[3] = "League"
League <- inner_join(League, Country)
League <- League[, -1]

#Combining Player and their attributes into a single df
Player <- left_join(Player, Player_Attributes, by='player_api_id')

#Deleting repeated columns
Player <- Player[,-c(8:9)]

#Removing rows with NA
Player <- na.omit(Player)

#Converting date columns into datetime
Player$birthday <- as.Date(Player$birthday)
Player$date <- as.Date(Player$date)

#Cristiano Ronaldo vs Lionel Messi
cr7 <- Player %>% filter(player_name == 'Cristiano Ronaldo') 
cr7$year <- year(cr7$date)
lm10 <- Player %>% filter(player_name == 'Lionel Messi')
lm10$year <- year(lm10$date)

#Selecting the last entry from each year
cr7 <- cr7 %>% group_by(year) %>% top_n(n = 1, wt = date)
lm10 <- lm10 %>% group_by(year) %>% top_n(n = 1, wt = date)

#Combining the above two tables
the_top_two <- rbind.data.frame(cr7, lm10)

#Some visuals comparing Ronaldo and Messi

ggplot(the_top_two, aes(x = year, y = penalties)) + 
  geom_line(aes(color = player_name))

ggplot(the_top_two, aes(x=year, y=free_kick_accuracy, fill = player_name)) + 
  geom_col(position='dodge') + 
  coord_cartesian(ylim = c(50,100))

ggplot(the_top_two, aes(x=year, y=stamina, fill = player_name)) + 
  geom_col(alpha = 0.4, position='identity') + 
  coord_cartesian(ylim = c(50,100))


# Let's make a radar chart with the attributes of these two players for the year 2015.

# To begin, we will just filter in only the necessary columns(the player_name and their attacking attributes). Then we will inlcude 
# statistics only for the year 2015. Then we will tidy our data and end up with just 3 columns (player_name, attributes and rating; 
# the select function before the final spread function does this) after removing the year column as well. 
# We are almost there. Now we just need to tidy up our data and make two new columns, one for Ronaldo and the other for Messi.

chart_radar <- the_top_two %>% as.data.frame() %>% select(player_name, crossing:penalties, year) %>% filter(year==2015) %>% 
  gather(key = attributes, value = rating, -player_name, -year) %>% select(c(3,4,1)) %>% spread(key = player_name, value = rating)

# We are here. Now let's build the radar chart!!!

chartJSRadar(scores = chart_radar, maxScale = 100, scaleStartValue = 0, main = '2015')



