## Basic Data Analysis on New Nominate

library(foreign)
library(stargazer)
library(ggplot2)
library(plyr)
library(reshape2)

# Import Data
NokkenPooleHouseScores <- read.csv("C:/Users/Olson/Dropbox/ideas/Tea Party Does Not Exist/NokkenPooleHouse.csv")

# Only Use Data for 110th onward
dav110 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 110), ]
# Only Post War
dav80 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 80), ]
# Only 20th Century
dav56 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 56), ]


### Part One
### Prelim Analysis
### Dec 15 update: Turns out my data was broken so a bunch of data was not being plotted correctly.
### Some of the first dimension nominate had letters first so it was messing stuff up.


# Rough Graph It.
# This doesn't work and is way too messy. Dropping data
p1gv1all <- ggplot(NokkenPooleHouseScores, aes(Congress, FirstDimension))
p1gv1all + geom_point()
p1gv1all + geom_point(aes(colour = factor(Party)))
dev.copy(png,'p1gv1all.png')
dev.off()

# For fun, look at second dimension which is racial data.
p1gv3all <- ggplot(NokkenPooleHouseScores, aes(Congress, SecondDimension))
p1gv3all + geom_point()
p1gv3all + geom_point(aes(colour = factor(Party)))

# Second Graph Attempt
p1gv2 <- ggplot(dav110, aes(Congress, FirstDimension))
p1gv2 + geom_point(aes(colour = factor(Party)))

### Part Two
### For something totally different now, need to examine average ideoology
### trends historically. For this I have to figure out how to extract averages
### per congress in order to find the pivots. Eventually I'll also need Senate
### data.
### Additionally, I had the idea of looking at extremism over all Congresses to see 
### if there has been a period where the parties were this extreme. Compare and 
### contrast. My thought is that we can compare contemporary Republicans to TP
### Republicans

## This is an extremely clumsy but working way of getting the average dw nominate
## scores for a given party in a given congress. I do it for both the post war
## and also the 20th century
# Post War
Agg80 <-aggregate(dav80$FirstDimension, by=list(dav80$Congress,dav80$Party), 
                  FUN=mean, na.rm=TRUE)
Agg80 <- rename(Agg80, c(Group.1="Congress"))
Agg80 <- rename(Agg80, c(Group.2="Party"))
Agg80 <- rename(Agg80, c(x="AverageIdeo"))

Agg80Final <- dcast(Agg80, Congress ~ Party)
Agg80Final <- rename(Agg80Final, c("100"="Democrat"))
Agg80Final <- rename(Agg80Final, c("200"="Republican"))

# 20th century
Agg56 <-aggregate(dav56$FirstDimension, by=list(dav56$Congress,dav56$Party), 
                  FUN=mean, na.rm=TRUE)
Agg56 <- rename(Agg56, c(Group.1="Congress"))
Agg56 <- rename(Agg56, c(Group.2="Party"))
Agg56 <- rename(Agg56, c(x="AverageIdeo"))

Agg56Final <- dcast(Agg56, Congress ~ Party)
Agg56Final <- rename(Agg56Final, c("100"="Democrat"))
Agg56Final <- rename(Agg56Final, c("200"="Republican"))



## Plots of Aggregate Data
## These pplots aren't working because the party shit is so diffused. 

## That does not work. Aggregate ideology scores not compiling. Need to drop Pres ahead of time. Let's plot it anyway.
## For some reason can't plot by party. Do not know why. 
p2g1 <- ggplot(Agg56Final, aes(Agg56Final$Democrat, Agg56Final$Republican))
p2g1 + geom_point()
p2g1 + facet_grid(p2daAgg2$party)

# Let's try using a regular line plot
p2g2 <- ggplot(p2daAgg, aes((x=p2daAgg$Congress, y=p2daAgg$FirstDimension, color=color = as.factor(p2daAgg$party)))

               
