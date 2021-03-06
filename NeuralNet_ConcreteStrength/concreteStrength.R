#Sample of using a multilayer feedforward neural networks to predict strength of concrete
#This sample is from Machine Learning with R book

#install.packages("neuralnet")
library("neuralnet")


normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

concrete <- read.csv("concrete.csv")
str(concrete)

#normalize the data to prevent squashing
concrete_norm <- as.data.frame(lapply(concrete,normalize))

#split data into train and test data
concrete_train <- concrete_norm[1:773,]
concrete_test <- concrete_norm[774:1030,]

concrete_model <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age, data = concrete_train)

plot(concrete_model)

model_results <- compute(concrete_model, concrete_test[1:8])

predicted_strength <- model_results$net.result

cor(predicted_strength, concrete_test$strength)

#repeat again but with 5 hidden layers

concrete_model2 <- neuralnet(strength ~ cement + slag + ash + water + superplastic +
                              coarseagg + fineagg + age, data = concrete_train, hidden = 5)

plot(concrete_model2)

model_results2 <- compute(concrete_model, concrete_test[1:8])

predicted_strength2 <- model_results2$net.result

cor(predicted_strength2, concrete_test$strength)