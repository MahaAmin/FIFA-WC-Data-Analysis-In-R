# read data in .csv files (WorldCupMatches.csv, WorldCupPlayers.csv, WorldCups.csv)
wc_matches <- read.csv("WorldCupMatches.csv", header = TRUE)
wc_players <- read.csv("WorldCupPlayers.csv", header = TRUE)
wc <- read.csv("WorldCups.csv", header = TRUE)

#start exploring "wc" table
#Display table structure
str(wc)

#How many times did each country win a world cup?
table(wc$Winner)
#Visualize the result
barplot(table(wc$Winner), las=2)

