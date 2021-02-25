# environment
library(tidyverse)
library(dplyr)
require(foreign)
require(Hmisc)
require(reshape2)
require(caret)
require(boot)
require(pROC)
library(mlbench)
library(MLmetrics)
library(gbm)
library(xgboost)
library(oddsratio)
library(xlsx)
library(hmeasure)
library(pROC)
library(ROCR)

parameters <- read_csv("~/MEGA/Andalucía/CDSS Covid/covid_cdss/train/hm_data/hm_hospitales_covid_structured.csv")
# properly mapping variables
parameters$hospital_outcome<-as.factor(ifelse(parameters$hospital_outcome==TRUE,'Alive','Expired'))

# median imputation for continuous colums
# most frequent value for categorical columns
impute.median <- function(x) replace(x, is.na(x), median(x, na.rm = TRUE))
impute.mostfreq <- function(x) replace(x, is.na(x), as.data.frame(sort(table(x),decreasing=TRUE))[1,1] ) 

parameters[,grep('vitals',names(parameters))]<-as.data.frame(apply(parameters[,grep('vitals',names(parameters))],2, function(x) impute.median(x) ))
parameters[,grep('lab',names(parameters))]<-as.data.frame(apply(parameters[,grep('lab',names(parameters))],2, function(x) impute.median(x) ))
parameters[,grep('pmhx',names(parameters))]<-as.data.frame(apply(parameters[,grep('pmhx',names(parameters))],2, function(x) impute.mostfreq(x) ))

# Train and test datasets creation

## Splitting de-identified data into testing and training, balanced version.

#We want the data to be sampled randomly but always the same way and we want to be sure that train and test must be balanced.


# Creating id for partition 
parameters['id']<- seq.int(nrow(parameters))
## set the seed to make our partition reproducible
set.seed(123)
# createDataPartition: "the random sampling is done within the levels of y when y is a factor in an attempt to balance the class distributions within the splits."
## 75% of the sample size
train_idx <- createDataPartition(as.factor(parameters$hospital_outcome), times = 1, p = 0.75, list=F)
train <- parameters[train_idx, ]
test <- parameters[-train_idx, ]
#Checking outcome is actually balanced
round(prop.table(table(parameters$hospital_outcome)),2)


## Separating datasets into outcome and exposures


# train dataset
train_X<-train[, names(train)!= "hospital_outcome"]
train_Y<-train$hospital_outcome

# test dataset
test_X<-test[, names(test)!= "hospital_outcome"]
test_Y<-test$hospital_outcome


# Random Hyperparameter Tunning
# 
# The default method for optimizing tuning parameters in train is to use a grid search. This approach is usually effective but, in cases when there are many tuning parameters, it can be inefficient. An alternative is to use a combination of grid search and racing. Another is to use a random selection of tuning parameter combinations to cover the parameter space to a lesser extent.
# 
# Using [caret](https://topepo.github.io/caret/).

set.seed(123)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 1,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           verboseIter = T,
                           search = "random")

## Random selection of tuning parameter combinations

# Here we are first adressing several Machine Learning methods.
# There are more methods that can be addressed [Available Models in caret::train](https://rdrr.io/cran/caret/man/models.html)


# defining outcome, exposures

outcome_model <- 'hospital_outcome'
exposures_model <- names(train)[!names(train) %in% c('hospital_outcome','PATIENT_ID') ]


outcome_and_exposure <- as.formula(
  paste( paste(outcome_model ,'~'
               , paste(exposures_model, collapse = " + ") )
  )
)

# Machine learning methods
gbmFit <- train( outcome_and_exposure
                                    ,data = train,
                                    method = "gbm",
                                    trControl = fitControl,
                                    verbose = T,
                                    metric = "precision" ## Specify which metric to optimize
)
svmFit <- train( outcome_and_exposure
                                    ,data = train,
                                    method = "svmRadial",
                                    trControl = fitControl,
                                    preProc = c("center", "scale"),
                                    tuneLength = 8,
                                    metric = "precision" ## Specify which metric to optimize
)
rfFit <- train( outcome_and_exposure
                                   ,data = train,
                                   method = "rf",
                                   trControl = fitControl,
                                   verbose = T,
                                   metric = "precision" ## Specify which metric to optimize
)
xgbFit <- train( outcome_and_exposure
                                    ,data = train,
                                    method = "xgbTree",
                                    trControl = fitControl,
                                    verbose = T,
                                    metric = "precision" ## Specify which metric to optimize
)

lrFit <- train( outcome_and_exposure
                                   ,data = train,
                                   method = "LogitBoost",
                                   trControl = fitControl,
                                   verbose = T,
                                   metric = "precision" ## Specify which metric to optimize
)

## Best models comprarision


resamps <- resamples(list( #gbmFit = gbmFit
  svmFit = svmFit
  ,rfFit  = rfFit
  ,xgbFit = xgbFit
  ,lrFit = lrFit
))
summary_resamps<-summary(resamps)
summary_resamps<-as.data.frame(summary_resamps$statistics)
summary_resamps

# Selecting the model with the best performance

# we save the best performing model (based on its ROC) and its name
best_performing_model<-get(
  rownames(summary_resamps[which(summary_resamps$==max(summary_resamps$ROC.Median))]
  )
)
#manually select name
best_performing_model_name<-best_performing_model$method # extracts name as string from model

# Evaluating the predictor on our test dataset

## Creating prediction-probabilities dataset

prediction_probabilities<-predict(rfFit, newdata = test,type = "prob") # We create the probabilities dataset using our best performing model.
final_predictions<-cbind(test_Y,prediction_probabilities) # we bind our prediction with the actual data
final_predictions<-rename(final_predictions, obs = test_Y) # the function twoClassSummary reads the actual outcome as 'obs'
# NEEDS TO BE CHANGED FOR EVERY MODEL DEPENDING ON THE OUTCOME!!!
final_predictions['pred']<-ifelse(final_predictions$Pos > .6 # we have set the threshold in .5 this can be optimized until best performance is achieved
                                  ,'TRUE','FALSE'
)
# Setting proper data types
final_predictions$obs<-factor(final_predictions$obs,levels = c('TRUE','FALSE') )
final_predictions$pred<-factor(final_predictions$pred,levels = c('TRUE','FALSE') )

## Geting evaluation insights

obs.labels.01 <- relabel(final_predictions$obs)
pred.labels.01 <- relabel(final_predictions$pred)
insights_list<-HMeasure(obs.labels.01,pred.labels.01)

insights_metrics<-round(as.data.frame(insights_list$metrics),3)

# we have so many different metrics, let's select only some of them
insights_metrics<-insights_metrics%>%select(AUC, Sens,Spec, Precision,F)
#renaming metric
insights_metrics<-insights_metrics%>%rename(AUROC = AUC) 

# we traspose the data for its representation
insights_metrics<-as.data.frame(t(insights_metrics))
names(insights_metrics)<-'Percent'

insights_metrics$Percent<-insights_metrics$Percent*100

insights_metrics['Category']<-rownames(insights_metrics)
# how to order the bars
insights_metrics$Category <- factor(insights_metrics$Category
                                    , levels = insights_metrics$Category)

best_performing_model_name

len <- 5
df2 <- data.frame(Category = letters[1:len], Percent = rep(0, len), 
                  Category2 = rep("", len))
insights_metrics$Category2 <- 
  paste0(insights_metrics$Category,": ",insights_metrics$Percent,"%")

# append number to category name
insights_metrics <- rbind(insights_metrics, df2)

# set factor so it will plot in descending order 
insights_metrics$Category <-
  factor(insights_metrics$Category, 
         levels=rev(insights_metrics$Category))


ggplot(insights_metrics, aes(x = Category, y = Percent
                             ,fill = Category 
))+ 
  geom_bar(width = 0.9, stat="identity") + 
  coord_polar(theta = "y") +
  xlab("") + ylab("") +
  ylim(c(0,100)) +
  #ggtitle(paste(best_performing_model_name,"Performing Metrics")) +
  geom_text(data = insights_metrics, hjust = 1, size = 6,
            aes(x = Category, y = 0, label = Category2)) +
  theme_minimal() +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())+
  scale_fill_manual(
    values=c("#e57373", "#9fa8da","#81d4fa","#80cbc4","#ffab91","#fff176","#80cbc4","#81d4fa","#e57373")
    ,name='', labels=c("AUROC", "Sensitivity","Specificity", "F"))
