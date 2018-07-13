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

#Connencted scattered plot
y <- wc$GoalsScored
x <- wc$Year
plot(x, y, xlim = range(x), ylim = range(y), pch=16, xlab = "WC-Year", ylab = "Scored-Goals")
lines(x, y, xlim = range(x), ylim = range(y), pch=16, col="blue")


#Where did each WC was hosted?
barplot(table(wc$Country), las=2)

#Attendance during each WC?
str(wc$Attendance)  #It is factor and '.' instead of ','
x <- wc$Year
ytemp <- gsub("\\.", "", wc$Attendance)  # replace '.' by ','
y <- as.numeric(ytemp)
str(ytemp)
str(y)

str(wc$Attendance)  #It is factor and '.' instead of ','
x <- wc$Year
ytemp <- gsub("\\.", "", wc$Attendance)  # replace '.' by ','
y <- as.numeric(ytemp)
str(ytemp)
str(y)
plot(x, y, xlim = range(x), ylim = range(y), pch=16, xlab = "WC-Year", ylab = "Attendants")
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
  scale_x_continuous(breaks = round(seq(min(x), max(x), by = 4),1)) +
  scale_y_continuous(breaks = round(seq(min(y), max(y), by = 100000))) 

  

  







