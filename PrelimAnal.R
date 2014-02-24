## Basic Data Analysis on New Nominate

library(foreign)
library(stargazer)
library(ggplot2)
library(plyr)
library(reshape2)

# Import Data
NokkenPooleHouseScores <- read.csv("C:/Users/Olson/Dropbox/ideas/Tea Party Does Not Exist/NokkenPooleHouse.csv")

#### This section does all the variable creation / preliminary moves so that I can create
#### graphs and analysis
### These change the data for my purposes. One thing to note is that they drop all third
### party members. I have recoded the party variables by hand in the .csv. I'll need to change
### the functions below to compensate for this. The treatment of third party members is something
### needs to be considered eventually. Also need to drop Presidents. 
## Only Use Data for 110th onward
dav110 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 110 & NokkenPooleHouseScores$Party <=2), ]
## Only Post World War Two
dav80 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 80 & NokkenPooleHouseScores$Party <=2), ]
## Only 20th Century
dav56 <- NokkenPooleHouseScores[ which(NokkenPooleHouseScores$Congress >= 56 & NokkenPooleHouseScores$Party <=2), ]

### This section labels both the regular party variable and the TParty variable for visualization
### and ease of use. For the record, TPartyAll is if a member has ever called themself a TP member
### and TPartyNew is if they called themself one in the 112th Congress. Only use the TParty*
### variables for visualizaton, not actual analysis.
## Assign Party Labels
# Post 110, regular party
dav110$Party <- factor(dav110$Party,
                       levels = c(0,1),
                       labels = c("Democrat", "Republican"))
# Post WW2, regular party
dav80$Party <- factor(dav80$Party,
                      levels = c(0,1),
                      labels = c("Democrat", "Republican"))
# 20th century, regular party
dav56$Party <- factor(dav56$Party,
                      levels = c(0,1),
                      labels = c("Democrat", "Republican"))
# 110th, TP
dav110$TPartyAll <- factor(dav110$TPartyAll,
                           levels = c(1,2,3),
                           labels = c("Democrat", "Republican", "Tea Party"))

dav110$TPartyNew <- factor(dav110$TPartyNew,
                           levels = c(1,2,3),
                           labels = c("Democrat", "Republican", "Tea Party"))
# Post ww2, TP
dav80$TPartyAll <- factor(dav80$TPartyAll,
                          levels = c(1,2,3),
                          labels = c("Democrat", "Republican", "Tea Party"))

dav80$TPartyNew <- factor(dav80$TPartyNew,
                          levels = c(1,2,3),
                          labels = c("Democrat", "Republican", "Tea Party"))
# Post 20th century, TP
dav56$TPartyAll <- factor(dav56$TPartyAll,
                          levels = c(1,2,3),
                          labels = c("Democrat", "Republican", "Tea Party"))

dav56$TPartyNew <- factor(dav56$TPartyNew,
                          levels = c(1,2,3),
                          labels = c("Democrat", "Republican", "Tea Party"))

#### Part One
#### Prelim Analysis
#### Dec 15 update: Turns out my data was broken so a bunch of data was not being plotted correctly.
#### Some of the first dimension nominate had letters first so it was messing stuff up.
## Rough Graph It.
# This doesn't work and is way too messy. Dropping data
p1gv1all <- ggplot(NokkenPooleHouseScores, aes(Congress, FirstDimension))
p1gv1all + geom_point()
p1gv1all + geom_point(aes(colour = Party))
dev.copy(png,'p1gv1all.png')
dev.off()

# For fun, look at second dimension which is racial data.
p1gv3all <- ggplot(NokkenPooleHouseScores, aes(Congress, SecondDimension))
p1gv3all + geom_point()
p1gv3all + geom_point(aes(colour = Party))

# Second Graph Attempt | This looks dumb
p1gv2 <- ggplot(dav110, aes(Congress, FirstDimension))
p1gv2 + geom_point(aes(colour = Party))

# Here is a pretty good one examining the shift of Republicans in post war America
p1gv4ww2 <- ggplot(dav80, aes(Congress, FirstDimension))
p1gv4ww2 + geom_point()
p1gv4ww2 + geom_point(aes(colour = Party))
dev.copy(png,'p1gv4all.png')
dev.off()

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

#### Part Three
#### I need to look at microlevel changes for individual members.
         
### Predict TP membership
               
### Look at Avg Ideology of TP vs AVG ideology of 1SD or 2SD Republicans in previous years
               
### Predict where their ideology should have been.
### This is a big deal. I probably will end up running I_t1 = I_t-1 types
### of models but eventually I will need to run a time series model that predicts
### normal GOP folks against extreme conservatives. Additionally, I should just look @ linear trend. 
### How do means between the TP folks compare with just normal Republicans. Another way to look at this
### is to I think that I want one of my DVs to be change in ideology as a function of TP membership.
### To accomplish this, I have to figure out a way to subtract T-1's ideology for a given member
### from T's ideology. First steps first, put the data in a version that allows us to calculate
### the difference
dav112Dif<- dcast(dav110, ICPSR + Name + State + Party + TPartyAll + TPartyNew + tptwo ~ Congress, value.var="FirstDimension")
dav112Dif$Difference112111 <- dav112Dif$"112" - dav112Dif$"111"
quickmodel <- lm(Difference112111 ~ Party, data=dav112Dif)
summary(quickmodel)
               
               