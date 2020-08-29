
library(dplyr)
library(ROCR)
library('ggplot2')
library(rpart)
library(rpart.plot)
library(caTools)
library("readxl")


# Read data from given excel sheet
df <- read_excel("AcmeTeleABT.xlsx",sheet = 'Training')

#keeping only abt columns
df <- subset(df, select = c(customerID,children,credit,creditCard,custcare,custcareTotal,directas,dropvce,income,marry,mou,mouTotal,outcalls,overage,peakOffPeak,recchrge,revenue,revenueTotal,revenueChange,roam,churn))


#exploratory data analysis
summary(df)

#revenueTotal & revenue Histogram
ggplot(df,aes(revenueTotal))+ geom_histogram(aes(fill=churn),color='black',bins=50,alpha=0.5) + theme_bw()
ggplot(df,aes(revenue))+ geom_histogram(aes(fill=churn),color='black',bins=50,alpha=0.5) + theme_bw()

#scatterplot against revenue & revenueTotal
ggplot(df,aes(revenue,revenueTotal)) +geom_point(aes(color=churn),size=2,alpha=0.7)

#data understanding
sum(df$revenue==0 | df$revenueTotal==0)


#Removing zero values (27 records) from dataset and adding revenueind column for better prediction
nrow(df)
df <- df %>% filter(df$revenue!=0 | df$revenueTotal!=0) %>% mutate(revenueind=revenueTotal/revenue)
nrow(df)


###################
# Use rpart to train and test a model
###################

# Generate a training and a test split

set.seed(101)

sample = sample.split(df$churn, SplitRatio = .80)
df.train = subset(df, sample == TRUE)
df.test = subset(df, sample == FALSE)

nrow(df.train)
nrow(df.test)

# Train a model 
tree <- rpart(churn ~ . ,method='class',data = df.train)

# Examine the model
printcp(tree)
summary(tree)
print(tree)
prp(tree)


# Prediction testing dataset
tree.preds <- predict(tree,df.test)
tree.probs <- predict(tree,df.test, type = "prob")

head(tree.preds)
tree.preds <- as.data.frame(tree.preds)

joiner <- function(x){
  if (x>=1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

tree.preds$Private <- sapply(tree.preds$'TRUE',joiner)
head(tree.preds)

#confusion matrix
table(tree.preds$Private,df.test$churn)

# Plotting an ROC curve 
pred <- prediction(tree.probs[, 2], df.test$churn)
perf <- performance(pred, "tpr", "fpr")
auc <- performance(pred, "auc")
plot(perf, lwd=2)


###################
# Model deployment on scoring datset
###################

df_scoring <- read_excel("AcmeTeleABT.xlsx",sheet = 'Scoring')
nrow(df_scoring)

summary(df_scoring)
sum(df_scoring$revenue==0 | df_scoring$revenueTotal==0)

#adding revenueind column for better prediction
df_scoring <- df_scoring %>% mutate(revenueind=revenueTotal/revenue)

# Prediction Scoring dataset
scoring.preds <- predict(tree,df_scoring)
scoring.probs <- predict(tree,df_scoring, type = "prob")

head(scoring.preds)
scoring.preds <- as.data.frame(scoring.preds)

joiner <- function(x){
  if (x>=1){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

scoring.preds$Private <- sapply(scoring.preds$'TRUE',joiner)
head(scoring.preds)

#confusion matrix of Scoring dataset
table(scoring.preds$Private,df_scoring$churn)

#clear global variables
#rm(list=ls()) 


