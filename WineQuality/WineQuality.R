#This example shows how to estimate quality of wine with
#regression trees. will do this with two types of decision trees
#regressiontrees and and model trees

#note these examples are from Machine Learning with R book, not original work

#**********************************
#load libraries
#install.packages(rpart) #rpart has a good implementation of regression trees
library("rpart")

if (Sys.getenv("JAVA_HOME")!="") #cannot load rjava if JAVA_HOME is set
  Sys.setenv(JAVA_HOME="")
library(rJava)
#install.packages("rpart.plot")
library("rpart.plot")

#Load RWEKA which has the M5 algorithm for model trees
#install.packages('RWeka')
library('RWeka')

#**********************************




#create Mean Square Error function to find average difference between our modes's
#prediction and true quality score
MAE <- function(actual,predicted){
  mean(abs(actual - predicted))
}


wine <- read.csv("whitewines.csv")

#str(wine)

hist(wine$quality)

#wine is alread sorted in random order, so we can break into two datasets
wine_train <- wine[1:3750,]
wine_test <- wine [3751:4898,]

m.rpart <- rpart(quality ~ .,data = wine_train)

#visualize the decision tree
rpart.plot(m.rpart,digits = 3)
rpart.plot(m.rpart,digits =4, fallen.leaves = TRUE, type = 3, extra= 101)

#evaluating the model
p.rpart <-predict(m.rpart,wine_test)
summary(p.rpart)
cor(p.rpart, wine_test$quality)
MAE(p.rpart, wine_test$quality)

#now do Model tree alogorithm
m.m5p <-M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)
p.m5p <- predict(m.m5p,wine_test)
cor(p.m5p, wine_test$quality)
MAE(p.m5p, wine_test$quality)



