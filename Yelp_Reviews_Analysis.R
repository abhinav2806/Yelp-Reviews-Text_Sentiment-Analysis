
library(dplyr)
library(ggplot2)
library(tidytext)
library(stringr)
library(gridExtra)
library(textstem)
library(textdata)
library(tidyr)
library(caret)
library(SnowballC)
library(tidyverse)
library(e1071) 
library(ranger)
library(rsample)
library(pROC)
library(magrittr)
library(RColorBrewer)
library(viridis)


#### Data Exploration: 



df = read.csv2("/Users/abhinavram/Documents/IDS572 Data Mining/Assignment 3/yelpRestaurantReviews_sample_f22.csv", sep =';')
glimpse(df)




#### Distribution of star ratings


head(df)
#How are star ratings distributed?
tbl = df %>% group_by(starsReview) %>% count() %>% ungroup() %>% mutate(per=`n`/sum(`n`)) %>% arrange(desc(starsReview))
tbl$label = paste(round(tbl$per*100,2),"%")
dist_chart=ggplot(data=tbl)+geom_bar(aes(x="", y=per, fill=starsReview), stat="identity", width = 1)+ geom_text(aes(x=1, y = cumsum(per) - per/2, label=label),color="white") + xlab("")+ylab("")+ggtitle("Distribution of Ratings in the data")
dist_chart



df %>% group_by(state) %>% tally()


ggplot(df, aes(x = starsReview)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
#Most of the reviews are 5 star, almost 38%
ggplot(df, aes(x=state)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)



#1,2 : Negative
#4,5 : Positive
rrDF = df %>% filter(str_detect(postal_code, "^[0-9]{1,5}"))
#tokenize the text of the reviews in the column named 'text'
df_Tokens = rrDF %>% unnest_tokens(word, text)
df_Tokens %>% distinct(word) %>% dim()
#remove stopwords
df_Tokens = df_Tokens %>% anti_join(stop_words)
df_Tokens %>% distinct(word) %>% dim()
#lets remove the words which are not present in at least 10 reviews
rareWords = df_Tokens %>% count(word, sort=TRUE) %>% filter(n<10)
rareWords %>% distinct(word) %>% dim()
df_Tokens = anti_join(df_Tokens, rareWords) %>% filter(str_detect(word,"[0-9]")==FALSE)
#Term-frequency, tf-idf
df_Tokens = df_Tokens %>%  mutate(word = textstem::lemmatize_words(word))
df_Tokens = df_Tokens %>% filter(str_length(word)>=3 & str_length(word)<=15)
df_Tokens = df_Tokens %>% group_by(review_id, starsReview) %>% count(word)
df_Tokens = df_Tokens %>% bind_tf_idf(word, review_id, n)

#### Question 2
## Top words in 5-star reviews

#Which words are related to higher/lower star raings in general
df_Tokens %>% filter(starsReview==5) %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(word) %>% count(word, sort=TRUE)

#### Top words in 4-star reviews

df_Tokens%>% filter(starsReview==4) %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(word) %>% count(word, sort=TRUE)

#### Top words in 3-star reviews

df_Tokens%>% filter(starsReview==3) %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(word) %>% count(word, sort=TRUE)

#### Top words in 2-star reviews

df_Tokens%>% filter(starsReview==2) %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(word) %>% count(word, sort=TRUE)

#### Top words in 1-star reviews


df_Tokens%>% filter(starsReview==1) %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(word) %>% count(word, sort=TRUE)




head(df_Tokens)




plots_review = list()
plots_review[[1]] = ggplot(df, aes(x=funny , y=starsReview)) +geom_point() + ggtitle(paste("For funny")) 
plots_review[[2]] = ggplot(df, aes(x=cool , y=starsReview)) +geom_point() + ggtitle(paste("For cool"))
plots_review[[3]] = ggplot(df, aes(x=useful , y=starsReview)) +geom_point() + ggtitle(paste("For useful"))
do.call(grid.arrange,plots_review)



plots_review = list()
plots_review[[1]] = ggplot(df, aes(x= useful, y=funny)) +geom_point()
plots_review[[2]] = ggplot(df, aes(x= useful, y=cool)) +geom_point()
plots_review[[3]] = ggplot(df, aes(x= cool, y=funny)) +geom_point()
do.call(grid.arrange,plots_review)


ggplot(df %>% group_by(starsReview) %>% summarize(AvgFunny_votes = mean(funny))) + aes(x=starsReview, y=AvgFunny_votes, fill=starsReview) + geom_line() + xlab("starsReview") + ylab("Average of Funny Votes")
ggplot(df %>% group_by(starsReview) %>% summarize(AvgCool_votes = mean(cool))) + aes(x=starsReview, y=AvgCool_votes, fill=starsReview) + geom_line() + xlab("starsReview") + ylab("Average of Cool Votes")
ggplot(df %>% group_by(starsReview) %>% summarize(AvgUseful_votes = mean(useful))) + aes(x=starsReview, y=AvgUseful_votes, fill=starsReview) + geom_line() + xlab("starsReview") + ylab("Average of Useful Votes")


#How does star ratings for reviews relate to the star-rating given in the dataset for business (attribute ‘businessStars’)?
p = list()
i = 1
for (r in c(1.5,2,2.5,3,3.5,4,4.5,5)){
  tbl_ = df[df$starsBusiness==r,]
  p[[i]] = ggplot(tbl_, aes(x = starsReview)) + geom_bar() + ggtitle(paste("starsBusiness:",r))
  i = i+1
}
do.call(grid.arrange,p)
  




tbl2 = df_Tokens%>% group_by(starsReview) %>% count(word, sort=TRUE) %>% mutate(prop=n/sum(n))
xx = tbl2 %>% group_by(word) %>% summarise(totWS=sum(starsReview*prop))
xx %>% top_n(20)
xx %>% top_n(-100)
tbl2 %>% filter(!word %in% c('food', 'time', 'restaurant', 'service')) %>% group_by(starsReview) %>% arrange(starsReview, desc(prop))%>% filter(row_number()<=20) %>% ggplot(aes(word, prop))+geom_col()+coord_flip()+facet_wrap((~starsReview))


#### Lexicon Dictionaries

#How many matching terms are there for each of the dictionaries?
from_bing_dict = inner_join(get_sentiments("bing"),df_Tokens, by="word")
from_nrc_dict = inner_join(get_sentiments("nrc"),df_Tokens, by="word")
from_afin_dict = inner_join(get_sentiments("afinn"),df_Tokens, by="word") 
binn = nrow(from_bing_dict)
nrcn = nrow(from_nrc_dict)
affn = nrow(from_afin_dict)
table_terms = matrix(c(binn,nrcn,affn),ncol=3,byrow=TRUE)
colnames(table_terms) = c("Bing","NRC","afin")
rownames(table_terms) = c("Number of matching terms")
table_terms = as.table(table_terms)
table_terms
barplot(table_terms, main =" Matching terms in each dictionary")


#### Using Bing Dictionary


## Dictionary 1 - Bing
#Analyze Which words contribute to positive/negative sentiment - we can count the occurrences of positive/negative sentiment words in the reviews
xx = from_bing_dict %>% group_by(word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
#negate the counts for the negative sentiment words
xx = xx %>% mutate (totOcc=ifelse(sentiment=="positive", totOcc, -totOcc))
#the most positive and most negative words
# ungrouping is important because we have grouped by word and sentiment together in the code above
xx = ungroup(xx)   
#top_n(xx, 25) %>% arrange(sentiment, desc(totOcc))
#top_n(xx, -25)  %>% arrange(sentiment, desc(totOcc))
orderw = rbind(top_n(xx, 25), top_n(xx, -25)) %>% mutate(word=reorder(word,totOcc)) 
# Review Sentiment Analysis
#summarise positive/negative sentiment words per review
rev_senti_bing = from_bing_dict %>% group_by(review_id, starsReview) %>% summarise(nwords=n(),posSum=sum(sentiment=='positive'), negSum=sum(sentiment=='negative'))

ggplot(orderw,aes(word, totOcc, fill= sentiment)) +geom_col()+coord_flip() #SHOULD I INCLUDE COLOR
#calculate sentiment score based on proportion of positive, negative words
rev_senti_bing = rev_senti_bing %>% mutate(posProp=posSum/nwords, negProp=negSum/nwords)
rev_senti_bing = rev_senti_bing %>% mutate(sentiScore=posProp-negProp)
rev_senti_bing %>% group_by(starsReview) %>% summarise(avgPos=mean(posProp), avgNeg=mean(negProp), avgSentiSc=mean(sentiScore))
#considering reviews with 1 & 2 starsReview as negative, and this with 4 & 5 starsReview as positive
rev_senti_bing = rev_senti_bing %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
rev_senti_bing = rev_senti_bing %>% mutate(pred_hiLo=ifelse(sentiScore >0, 1, -1))
xx_bing = rev_senti_bing %>% filter(hiLo!=0)
confusion_matrix_bing = table(actual=xx_bing$hiLo, predicted=xx_bing$pred_hiLo )


#calculating the accuracy of the predictions
confusionMatrix(confusion_matrix_bing)


#### Using NRC Dictionary


## Dictionary 2 - NRC
senti_nrc = from_nrc_dict %>% group_by (word, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(sentiment, desc(totOcc))
#top few words for different sentiments
senti_nrc %>% group_by(sentiment) %>% arrange(sentiment, desc(totOcc)) %>% top_n(10)
# we have got total 10 sentiments
#considering  {anger, disgust, fear sadness, negative} to denote 'bad' reviews, and {positive, joy, anticipation, trust,surprise} to denote 'good' reviews
# Geting the GoodBad score for each word
xx1 = senti_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust','surprise'), totOcc, 0)))
xx1 = ungroup(xx1)

nrcwords = rbind(top_n(xx1, 25), top_n(xx1, -25)) %>% mutate(word=reorder(word,goodBad)) 
##Analysis by Review 
rev_senti_nrc = from_nrc_dict %>% group_by (review_id, starsReview, sentiment) %>% summarise(totOcc=sum(n)) %>% arrange(starsReview, sentiment, desc(totOcc))
## Geting the GoodBad score for each review
xx2 = rev_senti_nrc %>% mutate(goodBad=ifelse(sentiment %in% c('anger', 'disgust', 'fear', 'sadness', 'negative'), -totOcc, ifelse(sentiment %in% c('positive', 'joy', 'anticipation', 'trust','surprise'), totOcc, 0)))
xx2 = ungroup(xx2)

rev_senti_nrc = xx2 %>% group_by(review_id, starsReview) %>% summarise(nwords=n(),sentiGoodBad =sum(goodBad))

ggplot(nrcwords,aes(word, goodBad, fill=goodBad)) +geom_col()+coord_flip()
rev_senti_nrc %>% group_by(starsReview)%>%summarise(avgLen=mean(nwords),avgSenti=mean(sentiGoodBad))
#considering reviews with 1 & 2 starsReview as negative, and this with 4 & 5 starsReview as positive
rev_senti_nrc = rev_senti_nrc %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
rev_senti_nrc = rev_senti_nrc %>% mutate(pred_hiLo=ifelse(sentiGoodBad >0, 1, -1))
xx_nrc1 = rev_senti_nrc %>% filter(hiLo!=0)
confusion_matrix_nrc = table(actual=xx_nrc1$hiLo, predicted=xx_nrc1$pred_hiLo )

#### Question 4



#calculating the accuracy of the predictions using afinn dictionary
#accuracy on training & test data
confusionMatrix(confusion_matrix_nrc)


#### Using AFINN Dictionary

# With Dictionary 3 -  Afin - Review Sentiment Analysis
#Analysis by Review Sentiment
rev_senti_afinn = from_afin_dict %>% group_by(review_id, starsReview) %>% summarise(nwords=n(), sentiSum =sum(value))

rev_senti_afinn %>% group_by(starsReview) %>% summarise(avgLen=mean(nwords),avgSenti=mean(sentiSum))
#considering reviews with 1 & 2 starsReview as negative, and this with 4 & 5 starsReview as positive
rev_senti_afinn = rev_senti_afinn %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))
rev_senti_afinn = rev_senti_afinn %>% mutate(pred_hiLo=ifelse(sentiSum >0, 1, -1))
xx<-rev_senti_afinn %>% filter(hiLo!=0)
confusion_matrix_afinn = table(actual=xx$hiLo, predicted=xx$pred_hiLo )

#calculating the accuracy of the predictions using afin dictionary
#accuracy on training & test data
confusionMatrix(confusion_matrix_afinn)


col1 = c('BING','NRC','AFINN')
col2 = c(83.42,78.19,83.68)
colna = c('Dictionary','Accuracy (%)')
metric_df = data.frame(cbind(col1,col2))
names(metric_df) = colna
metric_df

#### Question 5


#### Prediction models using each dictionary


#### Random Forest for Bing Dictionary 


senti_bing_data = from_bing_dict %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()
#filter out the reviews with starsReview=3, and calculate hiLo sentiment 'class'
senti_bing_data = senti_bing_data %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)



#how many review with 1, -1  'class'
senti_bing_data %>% group_by(hiLo) %>% tally()
#replace all the NAs with 0
senti_bing_data = senti_bing_data %>% replace(., is.na(.), 0)
senti_bing_data$hiLo = as.factor(senti_bing_data$hiLo)
#Create Dataset of 37,000 records
set.seed(213)
senti_bing_data_37k = senti_bing_data[sample(nrow(senti_bing_data),37000),]
set.seed(213)
senti_bing_data_37k_split = initial_split(senti_bing_data_37k, 0.5)
senti_bing_data_37k_trn = training(senti_bing_data_37k_split)
senti_bing_data_37k_tst = testing(senti_bing_data_37k_split)

set.seed(213)
rev_senti_bing_dat = rev_senti_bing[sample(nrow(rev_senti_bing),40000),]
set.seed(213)
rev_senti_bing_dat_split = initial_split(rev_senti_bing_dat, 0.5)
rev_senti_bing_dat_trn = training(rev_senti_bing_dat_split)
rev_senti_bing_dat_tst = testing(rev_senti_bing_dat_split)



## bing - ranger
rfModel_bing = ranger(dependent.variable.name = "hiLo", data=senti_bing_data_37k_trn %>% select(-review_id), num.trees = 200, importance = 'permutation', probability = TRUE)
#Obtain predictions, and calculate performance
bing_rf_trn_preds = predict(rfModel_bing, senti_bing_data_37k_trn %>% select(-review_id))$predictions
bing_rf_tst_preds = predict(rfModel_bing, senti_bing_data_37k_tst %>% select(-review_id))$predictions
#The optimal threshold from the ROC analyses
rocTrn = roc(senti_bing_data_37k_trn$hiLo, bing_rf_trn_preds[,2], levels=c(-1, 1))
rocTst = roc(senti_bing_data_37k_tst$hiLo, bing_rf_tst_preds[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')


#### Naive-Bayes for Bing Dictionary



nb_Bing <- naiveBayes(hiLo ~ ., data=rev_senti_bing %>% select(-review_id))
rev_senti_Bing_NBTrn <- predict(nb_Bing, rev_senti_bing_dat_trn, type = "raw") 
rev_senti_Bing_NBTst <- predict(nb_Bing, rev_senti_bing_dat_tst, type = "raw")
table(actual= rev_senti_bing_dat_trn$hiLo, predicted= rev_senti_Bing_NBTrn[,2]>0.5) 
table(actual= rev_senti_bing_dat_tst$hiLo, predicted= rev_senti_Bing_NBTst[,2]>0.5)
auc(as.numeric(rev_senti_bing_dat_trn$hiLo), rev_senti_Bing_NBTrn[,2]) 
auc(as.numeric(rev_senti_bing_dat_tst$hiLo), rev_senti_Bing_NBTst[,2]) 

rocTrn_nb = roc(rev_senti_bing_dat_trn$hiLo, rev_senti_Bing_NBTrn[,2], levels=c(-1, 1))
rocTst_nb = roc(rev_senti_bing_dat_tst$hiLo, rev_senti_Bing_NBTst[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

confusion_matrix_bing_nb = table(actual=rev_senti_bing_dat_trn$hiLo, predicted=rev_senti_bing_dat_tst$hiLo )
confusionMatrix(confusion_matrix_bing_nb)


#Best threshold from ROC analyses
bThr = coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr = as.numeric(bThr)
bThr
#Confusion Matrix at bThr for Trn and Tst dataset
confusionMatrix(table(actual=senti_bing_data_37k_trn$hiLo, preds=if_else(bing_rf_trn_preds[,2]>bThr,1,-1)))
confusionMatrix(table(actual=senti_bing_data_37k_tst$hiLo, preds=if_else(bing_rf_tst_preds[,2]>bThr,1,-1)))


#### Logistic Regression for Bing Dictionary




library(glmnet)
Log_Reg_Bing_x <- rev_senti_bing_dat_trn %>% select(-hiLo,-review_id)  
Log_Reg_Bing_y <- rev_senti_bing_dat_trn$hiLo

Log_Reg_Bing_x_Tst <- rev_senti_bing_dat_tst %>% select(-hiLo,,-review_id) 
Log_Reg_Bing_y_Tst <- rev_senti_bing_dat_tst$hiLo


set.seed(231)
cvglmnet_com <- cv.glmnet(data.matrix(Log_Reg_Bing_x), Log_Reg_Bing_y, 
                        family = "multinomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_com)


########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_com)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_com_train_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_Bing_x),s=cvglmnet_com$lambda.1se,type="class")
glm_com_train_pred <- factor(glm_com_train_pred, levels=c(1,-1))
Log_Reg_Bing_y_Tst2 <- factor(Log_Reg_Bing_y, levels=c(1,-1))                         
confusionMatrix (glm_com_train_pred,Log_Reg_Bing_y_Tst2, positive="1")                           

#confusion_matrix for Tst
glm_com_test_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_Bing_x_Tst),s=cvglmnet_com$lambda.1se,type="class")
glm_com_test_pred <- factor(glm_com_test_pred, levels=c(1,-1))
Log_Reg_Bing_y_Tst2 <- factor(Log_Reg_Bing_y_Tst, levels=c(1,-1))                         
confusionMatrix (glm_com_test_pred,Log_Reg_Bing_y_Tst2, positive="1")





#### Random Forest for NRC Dictionary 



## nrc
from_nrc_dict_1 = from_nrc_dict[,-2]
from_nrc_dict_1 = from_nrc_dict_1[!duplicated(from_nrc_dict_1), ]
#create Document Term Matrix
senti_nrc_data = from_nrc_dict_1 %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()
senti_nrc_data = senti_nrc_data %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)
senti_nrc_data = senti_nrc_data %>% replace(., is.na(.), 0)
senti_nrc_data$hiLo = as.factor(senti_nrc_data$hiLo)
senti_nrc_data %>% group_by(hiLo) %>% tally()
set.seed(213)
senti_nrc_data_38k = senti_nrc_data[sample(nrow(senti_nrc_data),38000),]
set.seed(213)
senti_nrc_data_38k_split = initial_split(senti_nrc_data_38k, 0.5)
senti_nrc_data_38k_trn = training(senti_nrc_data_38k_split)
senti_nrc_data_38k_tst = testing(senti_nrc_data_38k_split)

set.seed(213)
rev_senti_nrc_dat = senti_nrc_data[sample(nrow(senti_nrc_data),38000),]
set.seed(213)
rev_senti_nrc_dat_split = initial_split(rev_senti_nrc_dat, 0.5)
rev_senti_nrc_dat_trn = training(rev_senti_nrc_dat_split)
rev_senti_nrc_dat_tst = testing(rev_senti_nrc_dat_split)


## nrc - ranger
rfModel_nrc = ranger(dependent.variable.name = "hiLo", data=senti_nrc_data_38k_trn %>% select(-review_id), num.trees = 200, importance ='permutation', probability = TRUE)
#Obtain predictions, and calculate performance
nrc_rf_trn_preds = predict(rfModel_nrc, senti_nrc_data_38k_trn %>% select(-review_id))$predictions
nrc_rf_tst_preds = predict(rfModel_nrc, senti_nrc_data_38k_tst %>% select(-review_id))$predictions
#The optimal threshold from the ROC analyses
rocTrn = roc(senti_nrc_data_38k_trn$hiLo, nrc_rf_trn_preds[,2], levels=c(-1, 1))
rocTst = roc(senti_nrc_data_38k_tst$hiLo, nrc_rf_tst_preds[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

#### Naive Bayes for NRC Dictionary


nb_nrc <- naiveBayes(hiLo ~ ., data=rev_senti_nrc_dat %>% select(-review_id))
rev_senti_nrc_NBTrn <- predict(nb_nrc, rev_senti_nrc_dat_trn, type = "raw") 
rev_senti_nrc_NBTst <- predict(nb_nrc, rev_senti_nrc_dat_tst, type = "raw")
table(actual= rev_senti_nrc_dat_trn$hiLo, predicted= rev_senti_nrc_NBTrn[,2]>0.5) 
table(actual= rev_senti_nrc_dat_tst$hiLo, predicted= rev_senti_nrc_NBTst[,2]>0.5)
auc(as.numeric(rev_senti_nrc_dat_trn$hiLo), rev_senti_nrc_NBTrn[,2]) 
auc(as.numeric(rev_senti_nrc_dat_tst$hiLo), rev_senti_nrc_NBTst[,2]) 

rocTrn_nb = roc(rev_senti_nrc_dat_trn$hiLo, rev_senti_nrc_NBTrn[,2], levels=c(-1, 1))
rocTst_nb = roc(rev_senti_nrc_dat_tst$hiLo, rev_senti_nrc_NBTst[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

confusion_matrix_nrc_nb = table(actual=rev_senti_nrc_dat_trn$hiLo, predicted=rev_senti_nrc_dat_tst$hiLo )
confusionMatrix(confusion_matrix_nrc_nb)






#Best threshold from ROC analyses
bThr = coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr = as.numeric(bThr)
bThr
#Confusion Matrix at bThr for Trn and Tst dataset
confusionMatrix(table(actual=senti_nrc_data_38k_trn$hiLo, preds=if_else(nrc_rf_trn_preds[,2]>bThr,1,-1)))
confusionMatrix(table(actual=senti_nrc_data_38k_tst$hiLo, preds=if_else(nrc_rf_tst_preds[,2]>bThr,1,-1)))

#### Logistic Regression for NRC Dictionary



library(glmnet)
Log_Reg_nrc_x <- rev_senti_nrc_dat_trn %>% select(-hiLo,-review_id)  
Log_Reg_nrc_y <- rev_senti_nrc_dat_trn$hiLo

Log_Reg_nrc_x_Tst <- rev_senti_nrc_dat_tst %>% select(-hiLo,,-review_id) 
Log_Reg_nrc_y_Tst <- rev_senti_nrc_dat_tst$hiLo


set.seed(231)
cvglmnet_com <- cv.glmnet(data.matrix(Log_Reg_nrc_x), Log_Reg_nrc_y, 
                        family = "multinomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_com)


########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_com)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_com_train_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_nrc_x),s=cvglmnet_com$lambda.1se,type="class")
glm_com_train_pred <- factor(glm_com_train_pred, levels=c(1,-1))
Log_Reg_nrc_y_Tst2 <- factor(Log_Reg_nrc_y, levels=c(1,-1))                         
confusionMatrix (glm_com_train_pred,Log_Reg_nrc_y_Tst2, positive="1")                           

#confusion_matrix for Tst
glm_com_test_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_nrc_x_Tst),s=cvglmnet_com$lambda.1se,type="class")
glm_com_test_pred <- factor(glm_com_test_pred, levels=c(1,-1))
Log_Reg_nrc_y_Tst2 <- factor(Log_Reg_nrc_y_Tst, levels=c(1,-1))                         
confusionMatrix (glm_com_test_pred,Log_Reg_nrc_y_Tst2, positive="1")



#### Random Forest for AFINN Dictionary 


##afin
senti_afin_data = from_afin_dict %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()
#filter out the reviews with starsReview=3, and calculate hiLo sentiment 'class'
senti_afin_data = senti_afin_data %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)
#how many review with 1, -1  'class'
senti_afin_data %>% group_by(hiLo) %>% tally()
#replace all the NAs with 0
senti_afin_data = senti_afin_data %>% replace(., is.na(.), 0)
senti_afin_data$hiLo = as.factor(senti_afin_data$hiLo)

set.seed(213)
senti_afin_data_36k = senti_afin_data[sample(nrow(senti_afin_data),36000),]
set.seed(213)
senti_afin_data_36k_split = initial_split(senti_afin_data_36k, 0.5)
senti_afin_data_36k_trn = training(senti_afin_data_36k_split)
senti_afin_data_36k_tst = testing(senti_afin_data_36k_split)

set.seed(213)
rev_senti_afin_dat = senti_afin_data[sample(nrow(senti_afin_data),36000),]
set.seed(213)
rev_senti_afin_dat_split = initial_split(rev_senti_afin_dat, 0.5)
rev_senti_afin_dat_trn = training(rev_senti_afin_dat_split)
rev_senti_afin_dat_tst = testing(rev_senti_afin_dat_split)


## afin - ranger
rfModel_afin = ranger(dependent.variable.name = "hiLo", data=senti_afin_data_36k_trn %>% select(-review_id), num.trees = 200, importance='permutation', probability = TRUE)
#Obtain predictions, and calculate performance
afin_rf_trn_preds = predict(rfModel_afin, senti_afin_data_36k_trn %>% select(-review_id))$predictions
afin_rf_tst_preds = predict(rfModel_afin, senti_afin_data_36k_tst %>% select(-review_id))$predictions
#The optimal threshold from the ROC analyses
rocTrn = roc(senti_afin_data_36k_trn$hiLo, afin_rf_trn_preds[,2], levels=c(-1, 1))
rocTst = roc(senti_afin_data_36k_tst$hiLo, afin_rf_tst_preds[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

#Best threshold from ROC analyses
bThr = coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr = as.numeric(bThr)
bThr
#Confusion Matrix at bThr for Trn and Tst dataset
confusionMatrix(table(actual=senti_afin_data_36k_trn$hiLo, preds=if_else(afin_rf_trn_preds[,2]>bThr,1,-1)))
confusionMatrix(table(actual=senti_afin_data_36k_tst$hiLo, preds=if_else(afin_rf_tst_preds[,2]>bThr,1,-1)))


#### Naive Bayes for AFINN Dictionary


nb_afin <- naiveBayes(hiLo ~ ., data=rev_senti_afin_dat %>% select(-review_id))
rev_senti_afin_NBTrn <- predict(nb_nrc, rev_senti_afin_dat_trn, type = "raw") 
rev_senti_afin_NBTst <- predict(nb_nrc, rev_senti_afin_dat_tst, type = "raw")

table(actual= rev_senti_afin_dat_trn$hiLo, predicted= rev_senti_afin_NBTrn[,2]>0.5) 
table(actual= rev_senti_afin_dat_tst$hiLo, predicted= rev_senti_afin_NBTst[,2]>0.5)
auc(as.numeric(rev_senti_afin_dat_trn$hiLo), rev_senti_afin_NBTrn[,2]) 
auc(as.numeric(rev_senti_afin_dat_tst$hiLo), rev_senti_afin_NBTst[,2]) 

rocTrn_nb = roc(rev_senti_afin_dat_trn$hiLo, rev_senti_afin_NBTrn[,2], levels=c(-1, 1))
rocTst_nb = roc(rev_senti_afin_dat_tst$hiLo, rev_senti_afin_NBTst[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),
        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')

confusion_matrix_afin_nb = table(actual=rev_senti_afin_dat_trn$hiLo, predicted=rev_senti_afin_dat_tst$hiLo )
confusionMatrix(confusion_matrix_afin_nb)


#### Logistic Regression for AFINN Dictionary



library(glmnet)
Log_Reg_afin_x <- rev_senti_afin_dat_trn %>% select(-hiLo,-review_id)  
Log_Reg_afin_y <- rev_senti_afin_dat_trn$hiLo

Log_Reg_afin_x_Tst <- rev_senti_afin_dat_tst %>% select(-hiLo,,-review_id) 
Log_Reg_afin_y_Tst <- rev_senti_afin_dat_tst$hiLo


set.seed(231)
cvglmnet_com <- cv.glmnet(data.matrix(Log_Reg_afin_x), Log_Reg_afin_y, 
                        family = "multinomial",
                        nfolds = 5,
                        alpha = 1)
plot(cvglmnet_com)


########## variable importance glmnet
library(vip)
tb1 <- vi_model(cvglmnet_com)
arrange(tb1,desc(Importance),Variable)
sort(tb1$Importance, decreasing = TRUE) %>% view()

library(caret)
#confusion_matrix for Trn
glm_com_train_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_afin_x),s=cvglmnet_com$lambda.1se,type="class")
glm_com_train_pred <- factor(glm_com_train_pred, levels=c(1,-1))
Log_Reg_afin_y_Tst2 <- factor(Log_Reg_afin_y, levels=c(1,-1))                         
confusionMatrix (glm_com_train_pred,Log_Reg_afin_y_Tst2, positive="1")                           

#confusion_matrix for Tst
glm_com_test_pred <- predict(cvglmnet_com, data.matrix(Log_Reg_afin_x_Tst),s=cvglmnet_com$lambda.1se,type="class")
glm_com_test_pred <- factor(glm_com_test_pred, levels=c(1,-1))
Log_Reg_afin_y_Tst2 <- factor(Log_Reg_afin_y_Tst, levels=c(1,-1))                         
confusionMatrix (glm_com_test_pred,Log_Reg_afin_y_Tst2, positive="1")





#### Combined Dictionaries



#Combined Dict.
names(from_afin_dict)[names(from_afin_dict) == "value"] = "sentiment"
#Dimensions for matched words from all three dictionaries

#Converting the sentiment variable in AFINN dictionary to character
from_afin_dict = from_afin_dict %>% mutate(sentiment = as.character(sentiment))
#combine matched words from the three dictionaries
comb_dict = rbind(from_bing_dict, from_nrc_dict, from_afin_dict)
#comb_dict %>% dim()
#Dimensions for the distinct word tokens in comb_dict
#comb_dict %>% distinct(word) %>% dim()
#remove duplicates from comb_dict
comb_dict_1 = comb_dict[,-2]
comb_dict_1 = comb_dict_1[!duplicated(comb_dict_1), ]
#Dimensions for rrSenti_combo 
#comb_dict_1 %>% dim()
#Dimensions for the distinct word tokens in comb_dict_1
#comb_dict_1 %>% distinct(word) %>% dim()
#create Document Term Matrix
senti_comb_data = comb_dict_1 %>%  pivot_wider(id_cols = c(review_id,starsReview), names_from = word, values_from = tf_idf)  %>% ungroup()
#filter out the reviews with starsReview=3
#calculate hiLo sentiment(1 is assigned to 4 and 5/-1 is assigned to 1 and 2)
senti_comb_data = senti_comb_data %>% filter(starsReview!=3) %>% mutate(hiLo=ifelse(starsReview<=2, -1, 1)) %>% select(-starsReview)
#replace all NAs with zero
senti_comb_data = senti_comb_data %>% replace(., is.na(.), 0)
#convert hiLo from num to factor
senti_comb_data$hiLo = as.factor(senti_comb_data$hiLo)
#no of reviews with 1, -1 class
senti_comb_data %>% group_by(hiLo) %>% tally()
set.seed(213)
senti_comb_data_16k = senti_comb_data[sample(nrow(senti_comb_data),16000),]
senti_comb_data_16k_split = initial_split(senti_comb_data_16k, 0.5)
senti_comb_data_16k_trn = training(senti_comb_data_16k_split)
senti_comb_data_16k_tst  = testing(senti_comb_data_16k_split)




rfModel_comb = ranger(dependent.variable.name = "hiLo", data=senti_comb_data_16k_trn %>% select(-review_id), num.trees = 200, importance='permutation', probability = TRUE)
#Obtain predictions, and calculate performance
comb_rf_trn_preds = predict(rfModel_comb, senti_comb_data_16k_trn %>% select(-review_id))$predictions
comb_rf_tst_preds = predict(rfModel_comb, senti_comb_data_16k_tst %>% select(-review_id))$predictions
#The optimal threshold from the ROC analyses
library(pROC)
rocTrn = roc(senti_comb_data_16k_trn$hiLo, comb_rf_trn_preds[,2], levels=c(-1, 1))
rocTst = roc(senti_comb_data_16k_tst$hiLo, comb_rf_tst_preds[,2], levels=c(-1, 1))
plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
plot.roc(rocTst, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),col=c("blue", "red"), lwd=2, cex=0.8, bty='n')



#Best threshold from ROC analyses
bThr = coords(rocTrn, "best", ret="threshold", transpose = FALSE)
bThr = as.numeric(bThr)
bThr
#Confusion Matrix at bThr for Trn and Tst dataset
confusionMatrix(table(actual=senti_comb_data_16k_trn$hiLo, preds=if_else(comb_rf_trn_preds[,2]>bThr,1,-1)))
confusionMatrix(table(actual=senti_comb_data_16k_tst$hiLo, preds=if_else(comb_rf_tst_preds[,2]>bThr,1,-1)))



col1 = c('BING','NRC','AFINN','Combined')
col2 = c(96.52,97.57,94.84,98.65)
col3 = c(88.54,87.12,85.93,89.56)
colna = c('Dictionary','Accuracy (%) on Training Data','Accuracy (%) on Test Data')
metric_df = data.frame(cbind(col1,col2,col3))
names(metric_df) = colna
metric_df

#### Question 6


x<- df %>% select (review_id, attributes)
paste(x[1,2])

x2<-x %>% mutate (atts=str_split( attributes,'\\|')) %>% unnest(atts)

x3<-x2 %>% cbind (str_split_fixed ( x2$atts, ":", 2))

colnames(x3)[4] <- 'attName'
colnames(x3)[5] <- 'attValue'
colnames(x3)

x3<-x3 %>% select (-c(attributes ,atts))
x3<-x3 %>% filter(str_length(x3$attName) > 0)

x4<-x3 %>% pivot_wider(names_from = attName, values_from = attValue)

dim(x4)
glimpse(x4)

#Now we analyze 'Ambience'

paste(x4[1,3])
x5<-x4 %>% mutate (amb = str_split(Ambience, ","))

dim(x4)
dim(x5)

typeof(x5$amb)
x5$amb[1]
x5$amb[1000]

#creating the function

extractAmbience<-function(q)
{  sub(":.*","", q[which(str_extract(q,"True") == "True")])
}

x6<-x5 %>% mutate (amb = lapply (amb,extractAmbience ) ) 

#how many examples by different values for 'Ambience'

x6 %>% group_by(amb) %>% tally() %>% view()
y <- df %>% select(review_id, starsReview)



x7 <- merge(x6,y)
x7 %>% filter(str_detect (amb,'casual')) %>% summarise(n(),AvgStar = mean(starsReview))
x7 %>% filter(str_detect (amb,'classy')) %>% summarise(n(),AvgStar = mean(starsReview))

#Now we analyze 'GoodForMeal'

paste(x4[1,7])
x5<-x4 %>% mutate (GdFrMl = str_split (GoodForMeal, ","))

dim(x4)
dim(x5)

typeof(x5$GdFrMl)

x5$GdFrMl[1]
x5$GdFrMl[1000]

#creating the function

extractgood4meal<-function(q) 
{  sub(":.*","", q[which(str_extract(q,"True") == "True")])
}

x6<-x5 %>% mutate (GdFrMl = lapply (GdFrMl, extractgood4meal ) ) 

#how many examples by different values for 'Good For Meal'

x6%>%group_by(GdFrMl) %>% tally() %>% view()

x7 <- merge(x6,y)
x7%>%filter(str_detect (GdFrMl,'lunch'))  %>% summarise(n(),AvgStar = mean(starsReview))
x7%>%filter(str_detect (GdFrMl,'dinner')) %>% summarise(n(),AvgStar = mean(starsReview))

#Now we analyze 'BusinessParking'

paste(x4[1,5])
x5 <- x4 %>% mutate( bsnsPrk = str_split( BusinessParking, ","))

dim(x4)
dim(x5)

typeof(x5$bsnsPrk)

x5$bsnsPrk[1]
x5$bsnsPrk[1000]

#creating the function

extractBuspark<-function(q) 
{  sub(":.*","", q[which(str_extract(q, "True") == "True")])
}

x6<-x5%>% mutate (bsnsPrk=lapply(bsnsPrk, extractBuspark ) ) 

#how many examples by different values for 'Bus Park'

x6%>% group_by(bsnsPrk) %>% tally() %>% view()

x7 <- merge(x6,y)
x7%>% filter(str_detect (bsnsPrk,'lot'))%>% summarise(n(),AvgStar = mean(starsReview))
x7%>% filter(str_detect (bsnsPrk,'street'))%>% summarise(n(),AvgStar = mean(starsReview))



x7 = x7 %>% mutate(hiLo=ifelse(starsReview<=2,-1, ifelse(starsReview>=4, 1, 0 )))

str(x7)

x8 = subset(x7, select = -c(bsnsPrk) )

revAttDTM <- x8
dim(revAttDTM)
revAttDTM<-revAttDTM %>% replace(., is.na(.), 0)
revAttDTM$hiLo<-as.factor(revAttDTM$hiLo)
revAttDTM_split<- initial_split(revAttDTM, 0.5)
revAttDTM_trn<- training(revAttDTM_split)
revAttDTM_tst<- testing(revAttDTM_split)

rfModel_Att<-ranger(hiLo ~., data=revAttDTM_trn %>% select(-review_id), num.trees = 500, importance='permutation', probability = TRUE)

importance(rfModel_Att)
revAttDTM_predTrn<- predict(rfModel_Att, revAttDTM_trn %>% select(-review_id))$predictions
revAttDTM_predTst<- predict(rfModel_Att, revAttDTM_tst %>% select(-review_id))$predictions

table(actual=revAttDTM_trn$starsReview, preds=revAttDTM_predTrn[,2]>0.5) 
table(actual=revAttDTM_tst$starsReview, preds=revAttDTM_predTst[,2]>0.5) 
rocTrn2 <- roc(revAttDTM_trn$starsReview, revAttDTM_predTrn[,2])
rocTst2 <- roc(revAttDTM_tst$starsReview, revAttDTM_predTst[,2])
plot.roc(rocTrn2, col='blue', main = "Attribute")
plot.roc(rocTst2, col='red', add=TRUE)
legend("bottomright", legend=c("Training", "Test"),col=c("blue", "red"), lwd=2, cex=0.8, bty='n')



