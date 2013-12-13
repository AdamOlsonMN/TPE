## Basic Data Analysis on New Nominate

library(foreign)
library(stargazer)
library(ggplot2)

# Import Data
NokkenPooleScores <- read.csv("C:/Users/Olson/Dropbox/ideas/Tea Party Does Not Exist/NokkenPooleScores.csv")

# Rough Graph It.
# This doesn't work and is way too messy. Dropping data
v1all <- ggplot(NokkenPooleScores, aes(First.Dimension, Congress))
v1all + geom_point()

# Only Use Data for 110th onward
v2110 <- NokkenPooleScores[ which(NokkenPooleScores$Congress >= 110), ]