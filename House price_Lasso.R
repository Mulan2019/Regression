library(tidyverse)
library(ggplot2)
library(data.table)
library (ggthemes)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
library(esquisse)
library(car)
library(estimatr)
library(caret)
library(glmnet)
library(car)
library(vcd)
library(MASS)
library(caret)
#Read csv file
housing_train<-read.csv(file.choose(), header=TRUE, sep=",")
housing_test<-read.csv(file.choose(), header=TRUE, sep=",")
#adding saleprice cloumn to test file
housing_test$SalePrice<-0
#combine train and test
housing<- rbind(housing_train,housing_test)
#delete volnmns with more than 2700 NAs
which(colnames(housing)=="Fence"|colnames(housing)=="PoolQC"|colnames(housing)=="MiscFeature"|colnames(housing)=="MiscVal"|colnames(housing)=="Alley")
housing<-housing[,-c(7,73,74,75,76)]
###feature engineering
#Remove outliers
housing_1<- housing[-c(1299, 524),]
#check NA values
housing_1 %>% 
  select_if(function(x)any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.)))) 
#replace NA values

housing_1$FireplaceQu<-as.character(housing_1$FireplaceQu)
housing_1$GarageCond<-as.character(housing_1$GarageCond)
housing_1$GarageQual<-as.character(housing_1$GarageQual)
housing_1$GarageFinish<-as.character(housing_1$GarageFinish)
housing_1$BsmtCond<-as.character(housing_1$BsmtCond)
housing_1$GarageType<-as.character(housing_1$GarageType)
housing_1$BsmtExposure<-as.character(housing_1$BsmtExposure)
housing_1$BsmtQual<-as.character(housing_1$BsmtQual)
housing_1$BsmtFinType2<-as.character(housing_1$BsmtFinType2)
housing_1$BsmtFinType1<-as.character(housing_1$BsmtFinType1)
housing_1$MasVnrType<-as.character(housing_1$MasVnrType)

housing_1$FireplaceQu[is.na(housing_1$FireplaceQu)]<-"None"
housing_1$GarageCond[is.na(housing_1$GarageCond)]<-"None"
housing_1$GarageQual[is.na(housing_1$GarageQual)]<-"None"
housing_1$GarageFinish[is.na(housing_1$GarageFinish)]<-"None"
housing_1$GarageType[is.na(housing_1$GarageType)]<-"None"
housing_1$BsmtCond[is.na(housing_1$BsmtCond)]<-"None"
housing_1$BsmtExposure[is.na(housing_1$BsmtExposure)]<-"None"
housing_1$BsmtFinType2[is.na(housing_1$BsmtFinType2)]<-"None"
housing_1$BsmtQual[is.na(housing_1$BsmtQual)]<-"None"
housing_1$BsmtFinType1[is.na(housing_1$BsmtFinType1)]<-"None"
housing_1$MasVnrType[is.na(housing_1$MasVnrType)]<-"None"

housing_1$FireplaceQu<-as.factor(housing_1$FireplaceQu)
housing_1$GarageCond<-as.factor(housing_1$GarageCond)
housing_1$GarageQual<-as.factor(housing_1$GarageQual)
housing_1$GarageFinish<-as.factor(housing_1$GarageFinish)
housing_1$BsmtCond<-as.factor(housing_1$BsmtCond)
housing_1$GarageType<-as.factor(housing_1$GarageType)
housing_1$BsmtExposure<-as.factor(housing_1$BsmtExposure)
housing_1$BsmtQual<-as.factor(housing_1$BsmtQual)
housing_1$BsmtFinType2<-as.factor(housing_1$BsmtFinType2)
housing_1$BsmtFinType1<-as.factor(housing_1$BsmtFinType1)
housing_1$MasVnrType<-as.factor(housing_1$MasVnrType)


housing_1$GarageArea[is.na(housing_1$GarageArea)] <- 0
housing_1$GarageCars[is.na(housing_1$GarageCars)] <- 0
housing_1$TotalBsmtSF[is.na(housing_1$TotalBsmtSF)] <- 0
housing_1$BsmtHalfBath[is.na(housing_1$BsmtHalfBath)] <- 0
housing_1$BsmtFullBath[is.na(housing_1$BsmtFullBath)] <- 0
housing_1$MasVnrArea[is.na(housing_1$MasVnrArea)] <- 0
housing_1$LotFrontage[is.na(housing_1$LotFrontage)] <- 0
housing_1$GarageYrBlt[is.na(housing_1$GarageYrBlt)] <- 0
housing_1$BsmtUnfSF[is.na(housing_1$BsmtUnfSF)] <- 0
housing_1$BsmtFinSF1[is.na(housing_1$BsmtFinSF1)] <- 0
housing_1$BsmtFinSF2[is.na(housing_1$BsmtFinSF2)] <- 0

housing_1$MSZoning[is.na(housing_1$MSZoning)] <- "RM"
housing_1$Functional[is.na(housing_1$Functional)] <- "Typ"
housing_1$Utilities[is.na(housing_1$Utilities)] <- "AllPub"
housing_1$Electrical[is.na(housing_1$Electrical)] <- "SBrkr"
housing_1$KitchenQual[is.na(housing_1$KitchenQual)] <- "TA"
housing_1$SaleType[is.na(housing_1$SaleType)] <- "WD"
housing_1$Exterior1st[is.na(housing_1$Exterior1st)] <- "VinylSd"
housing_1$Exterior2nd[is.na(housing_1$Exterior2nd)] <- "VinylSd"
#check NAs
sum(is.na(housing_1))
colnames(housing_1)[colSums(is.na(housing_1)) > 0]
#create porch
housing_1$porch<-housing_1$WoodDeckSF+housing_1$X3SsnPorch+housing_1$EnclosedPorch+housing_1$OpenPorchSF+housing_1$ScreenPorch
#add all bathrooms together 
housing_1$bath<-housing_1$BsmtHalfBath+housing_1$BsmtFullBath+housing_1$FullBath+housing_1$HalfBath

#select variable using stepwise
model<-lm(log(SalePrice)~log(LotArea)*log(OverallQual)+MSSubClass*MSZoning+
            GrLivArea+sqrt(GarageArea)+TotalBsmtSF+Neighborhood+YearBuilt+SaleCondition+
            ExterQual+CentralAir*Fireplaces*GarageCars+PoolArea+bath*BedroomAbvGr+sqrt(porch),housing_1_train)

step.model<-stepAIC(model,direction = "both",trace = FALSE)
summary(step.model)

#dummy
LandSlopeSev<-ifelse(housing_1$LandSlope=="Sev",1,0)
NeighborhoodVeenker<-ifelse(housing_1$Neighborhood=="Veenker",1,0)
Condition1Norm<-ifelse(housing_1$Condition1=="Norm",1,0)
BldgTypeTwnhs<-ifelse(housing_1$BldgType=="Twnhs ",1,0)
RoofMatlMembran<-ifelse(housing_1$RoofMatl=="Membran",1,0)
FunctionalMaj2<-ifelse(housing_1$Functional=="Maj2",1,0)
SaleConditionNormal<-ifelse(housing_1$SaleCondition=="Normal",1,0)

#split X into train and test
housing_1_train<-subset(housing_1, Id<=1164) 
housing_1_test<-subset(housing_1, (Id>=1165 & Id<=1460))
#LASSO model
y<-log(housing_1_train$SalePrice)
options(na.action='na.pass')

X<-model.matrix(Id~MSZoning+log(LotArea)+LandSlope+BsmtFinSF1*BsmtFinSF2+Neighborhood+
                  OverallQual+YearBuilt+YearRemodAdd+X2ndFlrSF+X1stFlrSF+GarageArea+
                  ScreenPorch+SaleCondition+RoofMatl+GrLivArea+MSSubClass*MSZoning+CentralAir*Fireplaces*GarageCars+
                  PoolArea+bath*BedroomAbvGr+sqrt(porch),housing_1)[,-1]

X<-cbind(housing_1$Id,X)
#check if there is NA
sum(is.na(X))
colnames(X)[colSums(is.na(X)) > 0]
#rownames(X)[rowSums(is.na(X)) > 0]
#structure(X[2189,]) 
# split X into train,validation and prediction 
X.train<-subset(X,X[,1]<=1164)
X.val<-subset(X, (X[,1]>=1165 & X[,1]<=1460))
X.prediction<-subset(X,X[,1]>=1461)

#LASSO (alpha=1)
lasso<-glmnet(x = X.train, y = y, alpha = 1)
plot(lasso, xvar = "lambda")
#selecting the best penalty lambda
crossval <-  cv.glmnet(x = X.train, y = y, alpha = 1) #create cross-validation data
plot(crossval)
penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
log(penalty.lasso) #see where it was on the graph
lasso.opt<-glmnet(x = X.train, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
#test
lasso.val <- exp(predict(lasso.opt, s = penalty.lasso, newx= X.val))
RMSE(lasso.test, housing_1_test$SalePrice,na.rm = TRUE)
#Prediction
lasso.prediction <- exp(predict(lasso.opt, s = penalty.lasso, newx= X.prediction))
sum(is.na(lasso.prediction))
#Export
write.csv(lasso.prediction,file="housing_submission_3.csv")
plot(lasso.test)



