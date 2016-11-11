setwd("D:/Machine Learning/R Books")
sms_raw<-read.csv("sms_spam.csv",stringsAsFactors=F)
str(sms_raw)
sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)
#Data preparation – cleaning and standardizing text data
install.packages("tm")
require(tm)
sms_coprus<-VCorpus(VectorSource(sms_raw$text))
print(sms_coprus)
inspect(sms_coprus[1:2])
as.character(sms_coprus[[1]])
lapply(sms_coprus[1:2],as.character)
sms_corpus_clean<-tm_map(sms_coprus, content_transformer(tolower))
as.character(sms_corpus_clean[[1]])
sms_corpus_clean<-tm_map(sms_corpus_clean,removeNumbers)
sms_corpus_clean<-tm_map(sms_corpus_clean,removeWords,stopwords())
sms_corpus_clean<-tm_map(sms_corpus_clean, removePunctuation)
install.packages("SnowballC")
require(SnowballC)
sms_corpus_clean<-tm_map(sms_corpus_clean,stemDocument)
sms_corpus_clean<-tm_map(sms_corpus_clean,stripWhitespace)
as.character(sms_corpus_clean[[1:2]])

# Data preparation – splitting text documents into words
sms_dtm<-DocumentTermMatrix(sms_corpus_clean)
sms_dtm

# Data preparation – creating training and test datasets
sms_dtm_train<-sms_dtm[1:4169,]
sms_dtm_test<-sms_dtm[4170:5574,]
sms_dtm_train_labels<-sms_raw[1:4169,]$type
sms_dtm_test_lables<-sms_raw[4170:5574,]$type
prop.table(table(sms_dtm_train_labels))
prop.table(table(sms_dtm_test_lables))

# Visualizing text data – word clouds
install.packages("wordcloud")
require(wordcloud)
wordcloud(sms_corpus_clean,min.freq = 50,random.order = F)
spam<-subset(sms_raw,type=="spam")
ham<-subset(sms_raw,type=="ham")
wordcloud(spam$text,max.words = 40,scale = c(3,0.5))
wordcloud(ham$text,max.words = 40,scale = c(3,0.5))

# Data preparation – creating indicator features for frequent words
findFreqTerms(sms_dtm_train,5)
sms_freq_words<-findFreqTerms(sms_dtm_train,5)
str(sms_freq_words)
sms_dtm_freq_train<-sms_dtm_train[,sms_freq_words]
sms_dtm_freq_test<-sms_dtm_test[,sms_freq_words]
convert_counts<-function(x){x<- ifelse(x>0,"Yes","No") }
sms_train<-apply(sms_dtm_freq_train,MARGIN=2,convert_counts)
sms_test<-apply(sms_dtm_freq_test,MARGIN=2,convert_counts)

# Step 3 – training a model on the data
install.packages("e1071")
require(e1071)
sms_classifier<-naiveBayes(sms_train,sms_dtm_train_labels)

# Step 4 – evaluating model performance
sms_test_pred<-predict(sms_classifier,sms_test)
require(gmodels)
CrossTable(sms_test_pred,sms_dtm_test_lables,prop.chisq = F,prop.t = F,dnn=c('predicted','actual'))

# Step 5 – improving model performance
sms_classifier2<-naiveBayes(sms_train,sms_dtm_train_labels,laplace =1)
sms_test_pred2<-predict(sms_classifier2,sms_test)
CrossTable(sms_test_pred2,sms_dtm_test_lables,prop.chisq = F,prop.t = F,dnn=c('predicted','actual'))
