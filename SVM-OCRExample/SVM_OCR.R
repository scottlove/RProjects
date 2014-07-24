#Example of using SVM algorithm for OCR
#This example is from book Machine Learning algorithms with R

#install.packages("kernlab")
library("kernlab")

letters <- read.csv("letterdata.csv")

#Normally data would need to be converted to all numeric and normalized
#this dataset is already normalized and R packaged does scaling altomatically


#data is already ramdomly distributed, split into train, test
letters_train <- letters[1:16000,]
letters_test <- letters [16001:20000,]

letter_classifier <- ksvm(letter ~ ., data = letters_train, kernel = "vanilladot")

#evaluate performance
letter_predictions <- predict(letter_classifier, letters_test)
head(letter_predictions)

#compare predictions to projections. create a table 
table(letter_predictions,letters_test$letter)
agreement <- letter_predictions == letters_test$letter
#get how many trues and false
table(agreement)
#this gets percentage
prop.table(table(agreement))




#now improve the performance
#try athe Gaussian RBF kernael to map to higher dimensional space
letter_classifier_rbfr <- ksvm(letter ~ ., data = letters_train, kernel = "rbfdot")

#evaluate performance
letter_predictions_rbfr <- predict(letter_classifier_rbfr, letters_test)

#compare predictions to projections. create a table 
table(letter_predictions_rbfr,letters_test$letter)
agreement_rbfr <- letter_predictions_rbfr == letters_test$letter
#get how many trues and false
table(agreement_rbfr)
#this gets percentage
prop.table(table(agreement_rbfr))






