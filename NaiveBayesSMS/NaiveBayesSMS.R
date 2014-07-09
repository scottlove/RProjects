
library (tm)
library(wordcloud)
library(e1071)
library(gmodels)
library(NaiveBayesUtils) #my custom  package





#source("NaiveBayesFunctions.R")

#read in raw data
sms_raw <- read.csv("sms_spam.csv",stringsAsFactors = FALSE)
#str(sms_raw)
# type variable from character vector (spam, ham), to factor 
sms_raw$type <- factor(sms_raw$type)


?
#build a corpus of text documents (in this case each sms message is corpus)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
#print(sms_corpus)
#inspect(sms_corpus[1:3])



corpus_clean <- clean_corpus(sms_corpus)

#now perform tokinization.  This will create sparce matrix. count
#of each word in each document (sms message)
sms_dtm <- DocumentTermMatrix(corpus_clean)

#split data into training and test datasets
#in this case we use 75% for training, 25% for test
#first do raw data fram then document term matrix then corpus
sms_raw_train <-sms_raw[1:4169,]
sms_raw_test <-sms_raw[4170:5559,]
sms_dtm_train <-sms_dtm[1:4169,]
sms_dtm_test <-sms_dtm[4170:5559,]
sms_corpus_train <-corpus_clean[1:4169]
sms_corpus_test <-corpus_clean[4170:5559]

#create a wordcloud to look at the data
wordcloud(sms_corpus_train,min.freq = 40,random.order = FALSE)
#also look at clouds of spam and ham subsets
spam <- subset(sms_raw_train,type == "spam")
ham <-subset(sms_raw_train,type == "ham")
wordcloud(spam$text,max.words=40,scale = c(3,0.5))
wordcloud(ham$text,max.words=40,scale = c(3,0.5))

#reduce size of sparce matrix by removing words that are in less than 5 sms messages
sms_dict <- findFreqTerms(sms_dtm_train,5)
sms_train <- DocumentTermMatrix(sms_corpus_train,list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test,list(dictionary = sms_dict))



sms_train <-apply(sms_train, MARGIN = 2, convert_counts)
sms_test <-apply(sms_test, MARGIN = 2, convert_counts)

#train the model
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)

#test the model
sms_test_pred <- predict(sms_classifier,sms_test)
CrossTable(sms_test_pred,sms_raw_test$type,prop.chisq= FALSE, prop.t = FALSE, dnn=c('predicted','actual'))

#train again with lapcle estimator. if a word appeared in zero spam or ham
#not using laplace causes it to have to big a say

sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type,laplace = 1)
sms_test_pred2 <- predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_raw_test$type,prop.chisq= FALSE, prop.t = FALSE, dnn=c('predicted','actual'))


save(sms_classifier,sms_classifier2,file ="classifiers.RData")
load(file="classifiers.RData")






