

#Change sparce matrix to be yes/no for word appears instead of count
clean_corpus<- function(corpus){
  #run transformations to clean the corpus
  #cleas up to remove stopwords, remove numbers, to lower case.
  #also remove extra whitespace created in cleaning
  corpus_clean<- tm_map(sms_corpus,tolower)
  corpus_clean<- tm_map(corpus_clean,removeNumbers)
  corpus_clean<- tm_map(corpus_clean,removeWords, stopwords())
  corpus_clean<- tm_map(corpus_clean,stripWhitespace)
  #change in tm version 0.6.0 does not automatically convert to required TextDocument
  #run following before unning DocumentTermMatrix
  corpus_clean<- tm_map(corpus_clean,PlainTextDocument)
  return (corpus_clean)
}

#Change sparce matrix to be yes/no for word appears instead of count
convert_counts<- function(x){
  x <- ifelse(x > 0, 1, 0) 
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
  return (x)
}





