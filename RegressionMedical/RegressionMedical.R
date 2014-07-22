#Read in sample data.  Charges is the dependent variable
insurance <- read.csv("insurance.csv",stringsAsFactors=TRUE)

#take a look at distribution of charges
#Linear regression assumes normal distribution, which this is not
#so this not ideal dataset.  but using anyways
hist(insurance$charges)

#take a look to see correlation between independent variables
cor(insurance[c("age","bmi","children","charges")])

#also can visualize the relationships between features
#instead of creating multiple scatterplots us pairs() to create scatterplot matrix
pairs(insurance[c("age","bmi","children","charges")])

#event more info  can be done can be gotten from pairs.panels()
#This is imported from "psych" package
#install.packages("psych")
library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

#training the model
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model # this shows inf about the model
summary(ins_model) #more info on the model