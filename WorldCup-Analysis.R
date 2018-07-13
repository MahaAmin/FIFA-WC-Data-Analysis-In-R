# read data in .csv files (WorldCupMatches.csv, WorldCupPlayers.csv, WorldCups.csv)
wc_matches <- read.csv("WorldCupMatches.csv", header = TRUE)
wc_players <- read.csv("WorldCupPlayers.csv", header = TRUE)
wc <- read.csv("WorldCups.csv", header = TRUE)

#start exploring "wc" table
#Display table structure
str(wc)

#How many times did each country win a world cup?
table(wc$Winner)
barplot(table(wc$Winner), las=2)

#What's the total goals scored during every WC ?
library(ggplot2)
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

#COnnencted scattered plot
y <- wc$GoalsScored
x <- wc$Year
plot(x, y, xlim = range(x), ylim = range(y), pch=16, xlab = "WC-Year", ylab = "Scored-Goals")
lines(x, y, xlim = range(x), ylim = range(y), pch=16, col="blue")


#How many times each country hosted the WC?
barplot(table(wc$Country), las=2)

#How many teams qualified each WC ?

  
  
  
  
  
  
  
  







