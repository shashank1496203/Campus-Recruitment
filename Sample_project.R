library(plyr)
library(dplyr)
library(fastDummies)

library(ggplot2)
library(funModeling) 
library(Hmisc)
library(dummies)
library(corrplot)
library(gmodels)
library(class)
library(tidyr)



data<- read.csv("Placement_Data_Full_Class.csv")
View(data)
data$gender<- as.factor(data$gender)

data$ssc_b<-as.factor(data$ssc_b)
data$hsc_b<-as.factor(data$hsc_b)
data$hsc_s<-as.factor(data$hsc_s)
data$degree_t<-as.factor(data$degree_t)
data$workex<-as.factor(data$workex)
data$specialisation<-as.factor(data$specialisation)
data$status<-as.factor(data$status)
summary(data)
str(data)
data=data[,!names(data) %in% 'sl_no']
data=data[,!names(data) %in% 'salary']
data[data=='']<-NA
summary(data)
theme_set(theme_bw())
ggplot(data,aes(x=status,fill=status)) + geom_bar(aes(y=((..count..)/sum(..count..))*100))+ ylab("Percent")
num_data=subset(data,select = c(2,4,7,10,12))
num_data %>% gather() %>% ggplot(aes(value)) +  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 30) + geom_vline(aes(xintercept=mean(value)), color="blue", linetype="dashed", size=1)+
                                          ggtitle("Numerical Variables Histograms")
cat_data %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar(stat = 'count',width = 0.5) + 
  geom_text(stat = 'count',aes(label=..count..),vjust = 1, size = 3.5, color ="white")+
  ggtitle("Categorical Variables Distribution")
ggplot(data)+ geom_point(aes(x=gender, y=ssc_p,shape=status, color=status))
ggplot(data, aes(x=hsc_p, y=ssc_p,shape=status, color=status)) +
  geom_point(size=3)
categorical_c=c('gender','hsc_s','ssc_b','hsc_b','ssc_s','degree_t','workex','specialisation','status')
results_s <-fastDummies::dummy_cols(data,select_columns=categorical_c)
res_s<-results_s[, !(names(results_s) %in% categorical_c)]
corrplot(cor(res_s), type = 'upper', method = 'circle',  tl.col = 'black',  p.mat = res_s$P, sig.leve = 0.05)
categorical=c('gender','hsc_s','ssc_b','hsc_b','ssc_s','degree_t','workex','specialisation')
result<-fastDummies::dummy_cols(data,select_columns = categorical)
res<-result[, !names(result) %in% categorical]
View(res)
normalize <-function(x){
num <- x <- min(x)
denom <- max(x)-min(x)
return(num/denom)
}
place_norm <- as.data.frame(lapply(res[,c(1:5,7:22)],normalize))
set.seed(1234)
ind <- sample(2,nrow(place_norm),replace = TRUE, prob = c(0.666,0.334))
plac.training <- place_norm[ind==1,]
plac.test <- place_norm[ind==2,]
plac.trainlabels <- res[ind==1,6]
plac.testlabels <-res[ind==2,6]
knn_1 <- knn(plac.training, plac.test, plac.trainlabels, k=1, prob = TRUE)
knn_2 <-knn(plac.training,plac.test,plac.trainlabels,k=2,prob = TRUE)
round(sum(knn_1==plac.testLabels)/length(plac.testLabels)*100,2)