#Name: Tzu-lin 
#Date created: 17 Feb, 2017
#Data wrangling in exercise 4 for the next week's data

#read the "Human development" and "Gender inequality" datas into R
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

#explore the datasets
str(hd)
dim(hd)
summary(hd)
str(gii)
dim(hd)
summary(gii)

library(dplyr)
#rename the variables to shorten them
names(hd)[1] <- 'hdi.r'
names(hd)[2] <- 'country'
names(hd)[3] <- 'hdi'
names(hd)[4] <- 'life.exp'
names(hd)[5] <- 'exp.y.edu'
names(hd)[6] <- 'mean.y.edu'
names(hd)[7] <- 'gni.cap'
names(hd)[8] <- 'cap.rank.minus.hdi.rank'
names(gii)[1] <- 'g.r'
names(gii)[2] <- 'country'
names(gii)[3] <- 'gii'
names(gii)[4] <- 'mmr'
names(gii)[5] <- 'ad.b.r'
names(gii)[6] <- 'par%'
names(gii)[7] <- 'edu2f'
names(gii)[8] <- 'edu2m'
names(gii)[9] <- 'labf'
names(gii)[10] <- 'labm'

#mutate gender inequality data and create two new variables
gii <- mutate(gii, ratioedu2.fm = (edu2f/edu2m))
gii <- mutate(gii, ratiolab.fm = (labf/labm))

#join together the two datasets using country
human <- inner_join(hd, gii, by= 'country')

#save the new joined data to "data"
write.table(human, file="human.csv", sep= "\t", col.names = TRUE)

#the joined "human" table has 195 obs. and 19 variables. 

#Continue with data wrangling, excercise 5

#access package
library(stringr)

#mutate the data: transform the Gross National Income variable numeric
str_replace(human$gni.cap, pattern=",", replace ="") %>%as.numeric
#exclude unneeded variables
human <- select(human, one_of('country', 'ratioedu2.fm', 'ratiolab.fm', 'exp.y.edu', 'life.exp','gni.cap', 'mmr', 'ad.b.r', 'par%'  ))

#remove all rows with missing values
human <- na.omit(human)
#Remove the observations which relate to regions instead of countries
last <- nrow(human) - 7
human <- human[1:last,]

#Define the row names of the data by the country names and remove the country name column from the data.
rownames(human_) <- human$country
human<- select(human, -country)

#overwrite
write.table(human, file = "human.csv", sep= "\t", col.names = TRUE)

#end of data wrangling

str(human)
dim(human)
summary(human)
