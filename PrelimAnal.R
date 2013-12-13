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

# Second Graph Attempt
v2 <- ggplot(v2110, aes(First.Dimension, Second.Dimension))
v2 + geom_point(aes(colour = factor(Party)))

# Second Graph Attempt looks okay but need to differeniate by Congress
# This doesn't work right
v3 <- ggplot(v2110, aes(First.Dimension, Second.Dimension))
v3 + geom_point(aes(colour = factor(Party)))
v3 + facet_grid(Congress)

# Try this subtle difference
# THIS WORKS, though the scale is ugly.
V4 <- ggplot(v2110, aes(Second.Dimension, First.Dimension))
V4 <- V4 + geom_point()
V4 <- V4 + geom_point(aes(colour = factor(Party)))
V4 <- V4 + facet_grid(. ~ Congress)