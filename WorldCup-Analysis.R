# read data in .csv files (WorldCupMatches.csv, WorldCupPlayers.csv, WorldCups.csv)
wc_matches <- read.csv("WorldCupMatches.csv", header = TRUE)
wc_players <- read.csv("WorldCupPlayers.csv", header = TRUE)
wc <- read.csv("WorldCups.csv", header = TRUE)
#########################################################################################

#start exploring "wc" table
#Display table structure
str(wc)
#########################################################################################

# Settings and libraries
library("RColorBrewer")
display.brewer.all()
library(ggplot2)
#########################################################################################
#How many times did each country win a world cup?
table(wc$Winner)
barplot(sort(table(wc$Winner), decreasing = TRUE), las=2, main = "WC-Winners", ylab = "Frequency", col = brewer.pal(n = 9, name = "YlOrRd"))
#########################################################################################
#What's the total goals scored during every WC ?
data = data.frame(x=wc$Year, y=wc$GoalsScored)
#TODO: Fix labels on x and y axises
#Red and blue points
ggplot(data, aes(x=x, y=y)) + 
  geom_segment(aes(x=x, xend=x, y=0, yend=y)) + 
  geom_point(size=5, color="red", fill=alpha("blue", 0.3), alpha=0.2, shape=21, stroke=2)

#Black points
ggplot(data, aes(x=x, y=y)) + 
  geom_segment(aes(x=x, xend=x, y=0, yend=y) , size=1, colour="black", linetype="solid") + 
  geom_point() + 
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

#Connencted scattered plot
y <- wc$GoalsScored
x <- wc$Year
plot(x, y, xlim = range(x), ylim = range(y), pch=16, xlab = "WC-Year", ylab = "Scored-Goals", main = "WC-Total-Goals")
lines(x, y, xlim = range(x), ylim = range(y), pch=16, col="blue")
####################################################################################################

#Where did each WC was hosted?

barplot(sort(table(wc$Country), decreasing = TRUE), las=2, main = "WC-Hosts", ylab = "Frequency", col = brewer.pal(n = 9, name = "YlOrRd"))
####################################################################################################
#Attendance during each WC?
# Fixing format of wc$attendance
str(wc$Attendance)  #It is factor and '.' instead of ','
x <- wc$Year  # store data to be plotted on x-axis
ytemp <- gsub("\\.", "", wc$Attendance)  # replace '.' by ','
y <- as.numeric(ytemp)  # cast data in ytemp from factor to numeric and store it in variable y
str(ytemp)
str(y)

# Graph using plot()
plot(x, y, xlim = range(x), ylim = c(ymin, ymax), pch=16, xlab = "WC-Year", ylab = "Attendants", main = "WC-Attendants")
lines(x, y, xlim = range(x), ylim = range(y), pch=16, col="blue")

#Attendance during each WC? (ggplot)
ggplot(wc, aes(x,y)) + 
  geom_point() +
  geom_line() +
  theme_bw() +
  theme_linedraw() +
  xlab("Year") +
  ylab("Attendants") +
  theme_classic(base_size = 15) +
  scale_x_continuous(breaks = round(seq(min(x), max(x), by = 8),1)) +
  scale_y_continuous(breaks = round(seq(min(y), max(y), by = 500000))) 
####################################################################################################

#wc_matches table
matches <- as.data.frame(wc_matches)
groups <- grep("Group",matches$Stage)
matches$Stage <- as.character(matches$Stage)
for (group in groups){
  matches$Stage[group] = "Groups"
}
ggplot(matches, aes(x = matches$Home.Team.Initials, fill = matches$Stage)) +
  geom_bar(width = 0.5) + 
  xlab("Home_Team") + 
  ylab("Total_Number") + 
  labs(fill = "Stage") +
  theme_bw(base_size = 12) +
  coord_flip()

str(matches$Year)

# #Another method
# #select columns you are working on:
# #1) wc_matches$Home.Team.Intials
# homeIntials <- wc_matches$Home.Team.Initials
# 
# #2)Stages:
# stages <- wc_matches$Stage
# 
# #replace "Group 1", "Group 2", "Group A" , ...etc to "Groups"
# stages <- as.character(stages)
# groups <- grep("Group",matches$Stage)
# for (group in groups){
#   stages[group] = "Groups"
# }
# 
# countryVSstage <- table(homeIntials, stages)
# countryVSstage <- as.data.frame(countryVSstage)
# ggplot(countryVSstage, aes(x = countryVSstage$homeIntials, fill = matches$Stage )) +
#   geom_bar(width = 0.5) + 
#   xlab("Home_Team") + 
#   ylab("Total_Number") + 
#   labs(fill = "Stage") +
#   theme_bw(base_size = 12) +
#   coord_flip()
# 
# str(countryVSstage)
# table(matches$Stage)
#######################################################################################################

# HOW MANY TIMES EACH COUNTRY PARTICIPATED IN WC ?
# use matches$Home.Team.Name and matches$Away.Team.Name columns ...
# ... to create list of countries-names participated in WCs

tmpHomes <- matches$Home.Team.Name   # store matches$Home.Team.Name in tmpHomes variable
tmpAways <- matches$Away.Team.Name   # store matchesAway.Team.Name in tmpAways variable 

countries <- c()          # The list of all countries-names participated in WC.

# From tmpHomes append country-name if it doesn't exist in countries list
for (country in tmpHomes) {
  if (country %in% countries == FALSE){
    countries <- c(countries, country)
  }
}

# From tmpAways append country-name if it doesn't exist in countries list
for (country in tmpAways){
  if(country %in% countries == FALSE){
    countries <- c(countries, country)
  }
}

str(countries)      # There are 84 countries participated in WC.

# Create participation list to store frequency of participation for each country.
participation <- vector(length = 83)

# Intialize it with 0
for(i in 1:83){
  participation[i] = 0
}

# Connect participation list with contries name to have structure similar to dictionary
# frequency-dictionary --> Country : Participation
names(participation) <- countries

tmpYears <- matches$Year
years <- c()

for (year in tmpYears){
  if (year %in% years == FALSE){
    years <- c(years, year)
  }
}

str(matches)

countriesOfYear <- c() 
for (year in years){
  rm(countriesOfYear)
  countriesOfYear <- c()
  for (i in 1:852){
    if(matches$Year[i] > year){
      break
    }
    else if (matches$Year[i] < year){
      next()
    }
    else{
      if(matches$Home.Team.Name[i] %in% countriesOfYear == FALSE){
        countriesOfYear <- c(countriesOfYear, as.character(matches$Home.Team.Name[i]))
      }
      if(matches$Away.Team.Name[i] %in% countriesOfYear == FALSE){
        countriesOfYear <- c(countriesOfYear,as.character(matches$Away.Team.Name[i]))
      }
    }
  }
  for (country in countriesOfYear){
    participation[[country]] <- participation[[country]] + 1
  }
}

participation <-  unname(participation)
df <- data.frame(countries, participation)
y <- df[order(participation, decreasing = TRUE),]
str(df)
display.brewer.pal(n = 11, name = 'RdBu')
barplot(y$participation, names.arg = y$countries, las=2, main = "Participation Frequency", ylab = "Frequency",col = brewer.pal(n = 11, name = "RdBu"))

################################################################################################
# How many goals did each country score in WC ?
goalsOfCountry <- vector(length = 84) 
for (i in 1:84){
  goalsOfCountry[i] = 0
}

matches$Home.Team.Name <- as.character(matches$Home.Team.Name)
matches$Away.Team.Name <- as.character(matches$Away.Team.Name)

names(goalsOfCountry) <- countries
for(i in 1:852){
  goalsOfCountry[[matches$Home.Team.Name[i]]] <- goalsOfCountry[[matches$Home.Team.Name[i]]] + matches$Home.Team.Goals[i]
  goalsOfCountry[[matches$Away.Team.Name[i]]] <- goalsOfCountry[[matches$Away.Team.Name[i]]] + matches$Away.Team.Goals[i]
}

goalsOfCountry <-  unname(goalsOfCountry) 
goalsOfCountry <- goalsOfCountry[-c(84)]  # remove NA value
goalsOfCountryDF <- data.frame(countries, goalsOfCountry)
yaxis <- goalsOfCountryDF[order(goalsOfCountry, decreasing = TRUE),]
barplot(yaxis$goalsOfCountry, names.arg = yaxis$countries, ylim = c(0,250),las=2, main = "Total Scored Goals", ylab = "Scored_Goals",col = brewer.pal(n = 11, name = "Paired"))

##################################################################################################################################
# How many against-goals scored in each country?
againstGoals <- vector(length = 84)
for (i in 1:84){
  againstGoals[i] = 0
}

names(againstGoals) <- countries
for (i in 1:852){
  againstGoals[[matches$Home.Team.Name[i]]] <- againstGoals[[matches$Home.Team.Name[i]]] + matches$Away.Team.Goals[i]
  againstGoals[[matches$Away.Team.Name[i]]] <- againstGoals[[matches$Away.Team.Name[i]]] + matches$Home.Team.Goals[i]
  
}

againstGoals <- unname(againstGoals)
againstGoals <- againstGoals[-c(84)]
againstGoalsDF <- data.frame(countries, againstGoals)
yaxis <- againstGoalsDF[order(againstGoals, decreasing = TRUE),]
barplot(yaxis$againstGoals, names.arg = yaxis$countries, ylim = c(0,120),las=2, main = "Total Against Goals", ylab = "Against_Goals",col = brewer.pal(n = 11, name = "Spectral"))
###############################################################################################################################################################################################



