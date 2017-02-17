# Lin's rscript for exercise 4

# access the packages
library(MASS)
library(dplyr)
library(ggplot2)
library(GGally)
install.packages("corrplot", repos="http://R-Forge.R-project.org")
# load the data
data("Boston")

# explore the dataset
str(Boston)
summary(Boston)

# plot matrix of the variables
pairs(Boston)


# calculate the correlation matrix and round it
cor_matrix<-cor(Boston)%>%round(2)

# print the correlation matrix
cor_matrix

# visualize the correlation matrix
corrplot(cor_matrix, method="circle", type= "upper", cl.pos = "b", tl.cex = 0.6)

# center and standardize variables
boston_scaled <- scale(Boston)
# summaries of the scaled variables
summary(boston_scaled)
# class of the boston_scaled object
class(boston_scaled)
# change the object to data frame
boston_scaled <- as.data.frame(boston_scaled)


