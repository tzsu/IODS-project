#Tzu-lin Su, Jan 30
#Week 2 Rstudio, Regression and model validation 

#Read the data into memory
#There are 183 rows and 60 columns in the learning 2014 table.
lrn14 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
dim(lrn14)
str(lrn14)

#access the dplyr library
library(dplyr)

#questions related to deep/surface/strategic learning
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")

#select the columns related to deep/surface/strategic and create columns 'deep'/'surf'/stra'
deep_columns <- select(lrn14, one_of(deep_questions))
lrn14$deep <- rowMeans(deep_columns)
surface_columns <- select(lrn14, one_of(surface_questions))
lrn14$surf <- rowMeans(surface_columns)
strategic_columns <- select(lrn14, one_of(strategic_questions))
lrn14$stra <- rowMeans(strategic_columns)

#Rescale attitude
lrn14$Attitude / 10
lrn14$attitude <- lrn14$Attitude / 10

#select the columns that we want to analyse
keep_columns <- c("gender","Age","attitude", "deep", "stra", "surf", "Points")
learning2014 <- select(lrn14, one_of(keep_columns))
#change column names, make them all lowercase
colnames(learning2014)
colnames(learning2014)[2] <- "age"
colnames(learning2014)[7] <- "points"

#exclude obs where point is 0
learning2014 <- filter(learning2014, points > 0)




#Set the working directory
setwd("D:/Sydney Uni/forth semester units of study/Intro to Open Data Science/IODS-project")

#save the new dataset in the 'data' folder as 'learning2014.txt'
write.table(learning2014, "learning2014.txt", row.names = FALSE)

#read 'learning2014' into R 
#show the structure and first 6 obs. of the data
read.table("learning2014.txt", header = TRUE)
str(learning2014)
head(learning2014)

#The learning2014 dataset is prepared to help us analyse the relationship between learning approaches and students achievements in an introductory statistics course in Finland. 
#The dataset includes 7 variables (gender, age, attitude, deep, strategic, and surface), and 166 observations where students have positive points.


# a scatter plot matrix of the variables in learning2014.
# [-1] excludes the first column (gender)
pairs(learning2014[-1], col = learning2014$gender)

# access the GGally and ggplot2 libraries
library(GGally)
library(ggplot2)

# create a more advanced plot matrix with ggpairs()
p <- ggpairs(learning2014, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))

# draw the plot
# The graphical overview show summaries of the variables in the data.
# There are aprroximately double the amount of female students than male students. Most of the students are under 30 years old. And attitude seems to correlate with points.
p

# create a regression model with three explanatory variables, attitude, stra and deep. Let points be the target variable.
my_model2 <- lm(points ~ attitude + stra + deep, data = learning2014)
# print out a summary of the model
summary(my_model2)
# From the summary of the output, it shows that the multiple regression equation to be around 'points = 11.39 + 3.53(attitude) + 0.96(stra) - 0.75(deep)'.
# Which means if the value attitude increase by one, points will increase by 3.53 (if other variables stay the same), and so on. When all three explanatory variables are 0, points is predicted to be 11.39.(This may be because we have exclude points=0) 
# From F test for overall significance, we can conclude that at least one explanatory variable affects 'points'.
# The t test shows that there is evidence that 'attitude' affects 'points', however, it is also quite clear that 'deep' has no linear relationship with 'points'. 


# Remove 'deep' and create a new regression model.
my_model2 <- lm(points ~ attitude + stra, data = learning2014)
# print out the summary of the new model
summary(my_model2)
# The new summary shows that the regression equation 'points = 8.97 + 3.47(attitude) + 0.91(stra)'. Both the explanatory variables have positive affects on the target variable.
# Multiple R-squared here indicates that 20.5% of the variation in points is explained by the variation in attitude and stra.

# create a regression model with multiple explanatory variables
my_model2 <- lm(points ~ attitude + stra, data = learning2014)
?plot.lm
# draw diagnostic plots using the plot() function. Choose the plots 1, 2 and 5
par(mfrow = c(2,2))
plot(my_model2, which = c(1,2,5))
# The linear regression model assumes that:
# 1. Errors are normally distributed.
# 2. Errors are not correlated.
# 3. Errors have constant variance.
# 4. The size of a given error does not depend on the explanatory variables

# The Residuals vs Fitted values plot here has no pattern, which supports the assumption of constant variance of errors and the size of the errors do not depend on the explanatory variables.
# The Normal QQ plot here shows that a resonable fit with the line, which means the errors are normally distributed.
# The Residuals vs Leverage plot can measure how much impact a single observation has on the model. Here it is quite scattered, but all are withen 0.06, which means no outliers. (We have already eliminate the observations where points = 0) 













