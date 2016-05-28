rm(list = ls())
library(dplyr)
library(ggplot2)

#Setting the Appropriate Working Directory
setwd('C:/Users/chirag/Downloads/Analytics Vidya Problems Datasets/Big Mart sales problem')

# reading the training and Test Datasets 
train <- read.csv('Train.csv', na.strings = "")
test <- read.csv('Test.csv',na.strings = "")

test$Item_Outlet_Sales <- NA
train$flag <- 'train'
test$flag <- 'test'



data.all <- rbind(train,test)

# find the number of missing values for each variable
sapply(data.all, function(x) sum(is.na(x)))

#find the classes of each column
sapply(data.all,class)

summary(data.all)

# finding the colums with numerical data
numerical_col <- names(train)[sapply(train,is.numeric)]

# Getting the number of unique values in each column or variable
data.all %>% sapply(function(x) length(unique(x)))

# Getting unique values for factor variables
data.all %>% select(-one_of(numerical_col),-Item_Identifier,-flag)%>%
  sapply(function(x) unique(x))

# Getting frequency of levels for each categorical variables
data.all %>% select(-one_of(numerical_col),-Item_Identifier,-flag)%>%
  sapply(function(x) table(x))

# cleaning up the factor variable Item_Fat_Content
data.all$Item_Fat_Content[data.all$Item_Fat_Content %in% c('low fat','LF')]= 'Low Fat'
data.all$Item_Fat_Content[data.all$Item_Fat_Content %in% c('reg')]= 'Regular'

# converting outlet extablishment year to the toal no of years it has been open
data.all$Outlet_Establishment_Year <- 2016 - data.all$Outlet_Establishment_Year 


# MISSING VALUE TREATMENT

# treating missing values for Item_Weight

index <- !complete.cases(data.all$Item_Weight)

Item_IDs <- unique(data.all[index,]$Item_Identifier)


# replacing each missing item weight value with the mean of the item weight across that item category

for(i in 1:length(Item_IDs)){
  
  if(data.all %>% filter(Item_Identifier==Item_IDs[i],is.na(Item_Weight)) %>% nrow() >0){
    
    data.all[data.all$Item_Identifier==Item_IDs[i] & is.na(data.all$Item_Weight),'Item_Weight']<- mean(data.all[data.all$Item_Identifier==Item_IDs[i],]$Item_Weight, na.rm = TRUE)
  }
  
}


# Treating 0 item_visbility as NA and applying missing value treatment on it
for(i in 1:length(Item_IDs)){
  
  if(data.all %>% filter(Item_Identifier==Item_IDs[i],Item_Visibility==0) %>% nrow() >0){
    
    data.all[data.all$Item_Identifier==Item_IDs[i] & data.all$Item_Visibility==0,'Item_Visibility']<- mean(data.all[data.all$Item_Identifier==Item_IDs[i],]$Item_Visibility, na.rm = TRUE)
  }
  
}
  
  
# missing value treatment for Outlet Size
data.all %>% sapply(function(x) length(unique(x)))

OutletTypes <- levels(data.all$Outlet_Type )



for(i in 1:length(OutletTypes)){
  
  if(data.all %>% filter(Outlet_Type==OutletTypes[i],is.na(Outlet_Size)) %>% nrow() >0){
    
    data.all[data.all$Outlet_Type==OutletTypes[i] & is.na(data.all$Outlet_Size),'Outlet_Size']<- names(desc(table(data.all[data.all$Outlet_Type==OutletTypes[1] & !is.na(data.all$Outlet_Size) ,]$Outlet_Size)))[1]
  }
  
}

#------------------------------------------------------------------------------------------

#dropping the unused factors
data.all$Item_Fat_Content <- droplevels(data.all$Item_Fat_Content)


train <- data.all %>% filter(flag=='train') %>% select(-flag)
test <- data.all %>% filter(flag=='test') %>% select(-flag)

# #BUILDING A LINEAR MODEL 
# model <- glm(Item_Outlet_Sales ~ Item_Fat_Content+ Item_Weight + Item_Visibility + Item_Type+ Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type + Outlet_Type, data = train)
# 
# # GETTING THE PREDICTED VALUES
# predictedvalues <- predict(model,newdata = test)
# 
# test <- cbind(test,predictedvalues)
# 
# submission <- read.csv('SampleSubmission.csv')
# 
# submission$Item_Outlet_Sales <- predictedvalues
# 
# write.csv(submission,'PredictedValues.csv')

# Trying Random Forest

library(rpart)
library(rpart.plot)
library(randomForest)

ForestModel <- randomForest(Item_Outlet_Sales ~ Item_Fat_Content+ Item_Weight + Item_Visibility + Item_Type+ Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type + Outlet_Type, data = train, ntree=200, nodesize=25)

predictedvalues <- predict(ForestModel,newdata = test)

test <- cbind(test,predictedvalues)

submission <- read.csv('SampleSubmission.csv')

submission$Item_Outlet_Sales <- predictedvalues

write.csv(submission,'PredictedValues.csv')


# cartModel <- rpart(Item_Outlet_Sales ~ Item_Fat_Content+ Item_Weight + Item_Visibility + Item_Type+ Item_MRP + Outlet_Identifier + Outlet_Establishment_Year + Outlet_Size + Outlet_Location_Type + Outlet_Type, data = train, minbucket=25)
# 
# predictedvalues <- predict(ForestModel,newdata = test)
# 
# test <- cbind(test,predictedvalues)
# 
# submission <- read.csv('SampleSubmission.csv')
# 
# submission$Item_Outlet_Sales <- predictedvalues
# 
# write.csv(submission,'PredictedValues.csv')






