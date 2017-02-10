#Tzu-lin Su
#Date: February 8, 2017
#Week 3: Logistic regression
#Data source: UCI Machine Learning Reposiory - Student Alcohol Consumption 
#P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, EUROSIS, ISBN 978-9077381-39-7.

#set working directory
setwd("D:/Sydney Uni/forth semester units of study/Intro to Open Data Science/IODS-project/data")

#access the dplyr library
library(dplyr)

#Read student-mat and student-por into R
#Explore the structure and dimensions of the data
math <- read.table("student-mat.csv", sep = ';', header = TRUE)
por <- read.table("student-por.csv", sep = ';', header = TRUE)
glimpse(math)
glimpse(por)

#Join the two data sets using the common columns to use as identifiers
join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

#join the two datasets by the selected identifiers
#see structure and dimention of the new table math-por
math_por <- inner_join(math, por, by = join_by, suffix = c(".math",".por"))
glimpse(math_por)

#copy from the if-else datacamp exercise
# dplyr, math_por, join_by are available

# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))

# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

# print out the columns not used for joining
print(notjoined_columns)

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

# glimpse at the new combined data
glimpse(alc)

# access the 'tidyverse' packages dplyr and ggplot2
library(dplyr)
library(ggplot2)

# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)

# define a new logical column 'high_use'
alc <- mutate(alc, high_use = alc_use > 2)

#glimpse at the joined and modified data. the joined data should now have 382 obs and 35 variables.
glimpse(alc)

#save the new dataset in the 'data' folder as 'alc.txt'
write.table(alc, file = "alc.csv", sep = "\t", col.names = TRUE)
#end of data wrangling part

alc <- read.table("alc.csv", sep ="\t", header = TRUE)

#Taking a glimpse at the combined dataset
glimpse(alc)

library(ggplot2)

# initialize a plot of high_use and G3
g1 <- ggplot(alc, aes(x = high_use, y = G3, col=sex)) + geom_boxplot() + ylab('final grade') + xlab('high_use')
# define the plot as a boxplot and draw it
g1 + geom_boxplot() + ylab("grade")+ ggtitle("Student final grades by alcohol consumption and sex")

# initialise a plot of high_use and absences
g2 <- ggplot(alc, aes(x= high_use, y = absences, col = sex))
# define the plot as a boxplot and draw it
g2 +geom_boxplot() + ggtitle("Student absences by alcohol consumption and sex")

#initialise a plot of high_use and freetime
g4 <- ggplot(alc, aes(freetime)) + geom_bar(aes(fill = high_use), position = "dodge", stat="count") 
#define the plot as a barplot and draw it
g4  + ggtitle("Student freetime by alcohol consumption")

#initialise a plot of high_use and romantic
g5 <- ggplot(alc, aes(romantic)) + geom_bar(aes(fill = high_use), position = "dodge", stat="count") 
#define the plot as a barplot and draw it
g5  + ggtitle("Student romantic by alcohol consumption")


# find the model with glm()
m <- glm(high_use ~ G3 + absences + freetime + romantic, data = alc, family = "binomial")
# print out a summary of the model
summary(m)
# compute odds ratios (OR)
OR <- coef(m) %>%exp
#compute confidence intervals(CI)
CI <- exp(confint(m))

# find the new model with glm()
m <- glm(high_use ~ absences + freetime, data = alc, family = "binomial")
# print out a summary of the model
summary(m)

# predict() the probability of high_use
probabilities <- predict(m, type = "response")
# add the predicted probabilities to 'alc'
alc <- mutate(alc, probability = probabilities)
# use the probabilities to make a prediction of high_use
alc <- mutate(alc, prediction = probability>0.5)
# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)

# initialize a plot of 'high_use' versus 'probability' in 'alc'
g <- ggplot(alc, aes(x = probability, y = high_use, col = prediction))
# define the geom as points and draw the plot
g + geom_point()
# tabulate the target variable versus the predictions
table(high_use = alc$high_use, prediction = alc$prediction)%>% prop.table()%>% addmargins

# define a loss function (mean prediction error)
loss_func <- function(class, prob) {
  n_wrong <- abs(class - prob) > 0.5
  mean(n_wrong)
}

# call loss_func to compute the average number of wrong predictions in the (training) data
loss_func(class = alc$high_use, prob = alc$probability)


