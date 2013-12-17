## Basic Data Analysis on New Nominate

library(foreign)
library(stargazer)
library(ggplot2)

# Import Data
NokkenPooleHouseScores <- read.csv("C:/Users/Olson/Dropbox/ideas/Tea Party Does Not Exist/NokkenPooleHouse.csv")

# Only Use Data for 110th onward
dav110 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 110), ]
# Only Post War
dav80 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 80), ]

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
## Requires the Reshape package
library(reshape)
p2daAgg <-aggregate(dav80, by=list(dav80$Congress, dav80$Party), FUN=mean, na.rm=TRUE)

## For sake of temporary clenliness, drop non gophers or dems.
## This is broken
p2daAgg2 <- p2daAgg[ which(p2daAgg$party == 100 & p2daAgg$party == 200), ]

## Plots of Aggregate Data
## These pplots aren't working because the party shit is so diffused. 

## That does not work. Aggregate ideology scores not compiling. Need to drop Pres ahead of time. Let's plot it anyway.
## For some reason can't plot by party. Do not know why. 
p2g1 <- ggplot(p2daAgg2, aes(p2daAgg2$Congress, p2daAgg2$FirstDimension))
p2g1 + geom_point()
p2g1 + facet_grid(p2daAgg2$party)

# Let's try using a regular line plot
p2g2 <- ggplot(p2daAgg, aes((x=p2daAgg$Congress, y=p2daAgg$FirstDimension, color=color = as.factor(p2daAgg$party)))
