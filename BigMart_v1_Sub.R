library(randomForest)
library(caret)
library(onehot)
library(ggplot2)
library(Metrics)
library(tree)
library(rpart)
library(h2o)

# Function to calculate Mode
Mode_Calc <- function(x){
  single = unique(x)
  single[which.max(tabulate(x))]
}

# Read text file
Train_Base = read.table("C:/Users/param/Desktop/NJIT/Sem 2/CS634 - DM/Project/Data/Train.csv",header = TRUE , sep = ",")
Test_Base  = read.table("C:/Users/param/Desktop/NJIT/Sem 2/CS634 - DM/Project/Data/Test.csv",header = TRUE , sep = ",")
Sub_Base = read.table("C:/Users/param/Desktop/NJIT/Sem 2/CS634 - DM/Project/Data/submit.csv",header = TRUE , sep = ",")

###################### Train #############################
## Replace Missing vaues in Item_Weight in mean
Train_Base[is.na(Train_Base)] <- 0
Train_Base$Item_Weight[Train_Base$Item_Weight == 0]<- mean(Train_Base$Item_Weight)

## REplace 0 in Item_Visibility by Mean
Train_Base$Item_Visibility[Train_Base$Item_Visibility == 0]<- mean(Train_Base$Item_Visibility)

## Replace year with corresponding number 
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 1985]<- 2013-1985
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 1987]<- 2013-1987
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 1997]<- 2013-1997
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 1998]<- 2013-1998
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 1999]<- 2013-1999
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 2002]<- 2013-2002
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 2004]<- 2013-2004
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 2007]<- 2013-2007
Train_Base$Outlet_Establishment_Year[Train_Base$Outlet_Establishment_Year == 2009]<- 2013-2009

## Replace 'LF' and 'low fat' with 'Low Fat' and 'reg' with 'Regular'
Train_Base$Item_Fat_Content[Train_Base$Item_Fat_Content == 'LF'] <- 'Low Fat'
Train_Base$Item_Fat_Content[Train_Base$Item_Fat_Content == 'low fat'] <- 'Low Fat'
Train_Base$Item_Fat_Content[Train_Base$Item_Fat_Content == 'reg'] <- 'Regular'
Train_Base$Item_Fat_Content = droplevels(Train_Base$Item_Fat_Content)

## Replace missing values in Outle_Size by mode
Train_Base$Outlet_Size[Train_Base$Outlet_Size == ""] <- Mode_Calc(Train_Base$Outlet_Size)
Train_Base$Outlet_Size = droplevels(Train_Base$Outlet_Size)

## Convert Outlet_Location_Type into numeric
Train_Base$Outlet_Size = as.numeric(Train_Base$Outlet_Size)

## Create Item Volume Sold.
Train_Base["Item_Volume"] = Train_Base$Item_Outlet_Sales/Train_Base$Item_MRP
Train_Base$Item_Volume = round(Train_Base$Item_Volume)
Train_Base$Item_Outlet_Sales = NULL


## Drop Outlet Size
Train_Base =  subset(Train_Base,select = -c(Item_Identifier))

########################### Test #################################

## Replace Missing vaues in Item_Weight in mean
Test_Base[is.na(Test_Base)] <- 0
Test_Base$Item_Weight[Test_Base$Item_Weight == 0]<- mean(Test_Base$Item_Weight)

## REplace 0 in Item_Visibility by Mean
Test_Base$Item_Visibility[Test_Base$Item_Visibility == 0]<- mean(Test_Base$Item_Visibility)

## Replace year with corresponding number 
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 1985]<- 2013-1985
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 1987]<- 2013-1987
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 1997]<- 2013-1997
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 1998]<- 2013-1998
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 1999]<- 2013-1999
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 2002]<- 2013-2002
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 2004]<- 2013-2004
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 2007]<- 2013-2007
Test_Base$Outlet_Establishment_Year[Test_Base$Outlet_Establishment_Year == 2009]<- 2013-2009

## Replace 'LF' and 'low fat' with 'Low Fat' and 'reg' with 'Regular'
Test_Base$Item_Fat_Content[Test_Base$Item_Fat_Content == 'LF'] <- 'Low Fat'
Test_Base$Item_Fat_Content[Test_Base$Item_Fat_Content == 'low fat'] <- 'Low Fat'
Test_Base$Item_Fat_Content[Test_Base$Item_Fat_Content == 'reg'] <- 'Regular'
Test_Base$Item_Fat_Content = droplevels(Test_Base$Item_Fat_Content)

## Replace missing values in Outle_Size by mode
Test_Base$Outlet_Size[Test_Base$Outlet_Size == ""] <- Mode_Calc(Test_Base$Outlet_Size)
Test_Base$Outlet_Size = droplevels(Test_Base$Outlet_Size)

## Convert Outlet_Location_Type into numeric
Test_Base$Outlet_Size = as.numeric(Test_Base$Outlet_Size)

## Create Item Volume Sold.
Test_Base["Item_Volume"] = Test_Base$Item_Outlet_Sales/Test_Base$Item_MRP
Test_Base$Item_Volume = round(Test_Base$Item_Volume)
Test_Base$Item_Outlet_Sales = NULL


## Drop Outlet Size
Test_Base =  subset(Test_Base,select = -c(Item_Identifier))


## Create One Hot Data Train
Train_Base_Onehot = onehot(Train_Base,max_levels = 16)
Train_Base_Predict = predict(Train_Base_Onehot,Train_Base)
Train_Base_Predict = as.data.frame(Train_Base_Predict)

## Create One Hot Data Test
Test_Base_Onehot = onehot(Test_Base,max_levels = 16)
Test_Base_Predict = predict(Test_Base_Onehot,Test_Base)
Test_Base_Predict = as.data.frame(Test_Base_Predict)


# Renaming Columns Train

colnames(Train_Base_Predict)[2] <- "LowFat"
colnames(Train_Base_Predict)[3] <- "Regular"

colnames(Train_Base_Predict)[5] <- "Baking_Goods"
colnames(Train_Base_Predict)[6] <- "Breads"
colnames(Train_Base_Predict)[7] <- "Breakfast"
colnames(Train_Base_Predict)[8] <- "Canned"
colnames(Train_Base_Predict)[9] <- "Dairy"
colnames(Train_Base_Predict)[10] <- "Frozen_Foods"
colnames(Train_Base_Predict)[11] <- "Fruit_and_Vegetables"
colnames(Train_Base_Predict)[12] <- "Hard_Drinks"
colnames(Train_Base_Predict)[13] <- "Health_and_Hygine"
colnames(Train_Base_Predict)[14] <- "Household"
colnames(Train_Base_Predict)[15] <- "Meat"
colnames(Train_Base_Predict)[16] <- "Others"
colnames(Train_Base_Predict)[17] <- "Seafood"
colnames(Train_Base_Predict)[18] <- "Snack_Foods"
colnames(Train_Base_Predict)[19] <- "Soft_Drinks"
colnames(Train_Base_Predict)[20] <- "Starch_Foods"

colnames(Train_Base_Predict)[22] <- "OUT10"
colnames(Train_Base_Predict)[23] <- "OUT13"
colnames(Train_Base_Predict)[24] <- "OUT17"
colnames(Train_Base_Predict)[25] <- "OUT18"
colnames(Train_Base_Predict)[26] <- "OUT19"
colnames(Train_Base_Predict)[27] <- "OUT27"
colnames(Train_Base_Predict)[28] <- "OUT35"
colnames(Train_Base_Predict)[29] <- "OUT45"
colnames(Train_Base_Predict)[30] <- "OUT46"
colnames(Train_Base_Predict)[31] <- "OUT49"

colnames(Train_Base_Predict)[34] <- "Tier1"
colnames(Train_Base_Predict)[35] <- "Tier2"
colnames(Train_Base_Predict)[36] <- "Tier3"

colnames(Train_Base_Predict)[37] <- "Grocery"
colnames(Train_Base_Predict)[38] <- "SuperMarket_1"
colnames(Train_Base_Predict)[39] <- "SuperMarket_2"
colnames(Train_Base_Predict)[40] <- "SuperMarket_3"

## Renaming columns Test

colnames(Test_Base_Predict)[2] <- "LowFat"
colnames(Test_Base_Predict)[3] <- "Regular"

colnames(Test_Base_Predict)[5] <- "Baking_Goods"
colnames(Test_Base_Predict)[6] <- "Breads"
colnames(Test_Base_Predict)[7] <- "Breakfast"
colnames(Test_Base_Predict)[8] <- "Canned"
colnames(Test_Base_Predict)[9] <- "Dairy"
colnames(Test_Base_Predict)[10] <- "Frozen_Foods"
colnames(Test_Base_Predict)[11] <- "Fruit_and_Vegetables"
colnames(Test_Base_Predict)[12] <- "Hard_Drinks"
colnames(Test_Base_Predict)[13] <- "Health_and_Hygine"
colnames(Test_Base_Predict)[14] <- "Household"
colnames(Test_Base_Predict)[15] <- "Meat"
colnames(Test_Base_Predict)[16] <- "Others"
colnames(Test_Base_Predict)[17] <- "Seafood"
colnames(Test_Base_Predict)[18] <- "Snack_Foods"
colnames(Test_Base_Predict)[19] <- "Soft_Drinks"
colnames(Test_Base_Predict)[20] <- "Starch_Foods"

colnames(Test_Base_Predict)[22] <- "OUT10"
colnames(Test_Base_Predict)[23] <- "OUT13"
colnames(Test_Base_Predict)[24] <- "OUT17"
colnames(Test_Base_Predict)[25] <- "OUT18"
colnames(Test_Base_Predict)[26] <- "OUT19"
colnames(Test_Base_Predict)[27] <- "OUT27"
colnames(Test_Base_Predict)[28] <- "OUT35"
colnames(Test_Base_Predict)[29] <- "OUT45"
colnames(Test_Base_Predict)[30] <- "OUT46"
colnames(Test_Base_Predict)[31] <- "OUT49"

colnames(Test_Base_Predict)[34] <- "Tier1"
colnames(Test_Base_Predict)[35] <- "Tier2"
colnames(Test_Base_Predict)[36] <- "Tier3"

colnames(Test_Base_Predict)[37] <- "Grocery"
colnames(Test_Base_Predict)[38] <- "SuperMarket_1"
colnames(Test_Base_Predict)[39] <- "SuperMarket_2"
colnames(Test_Base_Predict)[40] <- "SuperMarket_3"


## Decision Tree
ctrl = rpart.control(maxdepth = 4,minsplit = 20,minbucket = 7) 
dt1 = rpart(Item_Volume ~. , data = Train_Base_Predict,parms =  c(split = "gini"),control = ctrl)
PredictTree1 = predict(dt1,newdata = Test_Base_Predict[1:40])
Sub_Base_Tree = Sub_Base
Sub_Base_Tree$Item_Outlet_Sales = PredictTree1*Test_Base_Predict$Item_MRP
write.csv(Sub_Base_Tree, file = "Sub_v1_Tree.csv")

## Random Forest
rf1 =randomForest(Item_Volume ~ . ,data = Train_Base_Predict,ntree = 500,mtry = 30,maxnodes = 40)
PredictForest1 <- predict(rf1, newdata = Test_Base_Predict[1:40])
Sub_Base_RF = Sub_Base
Sub_Base_RF$Item_Outlet_Sales = PredictForest1*Test_Base_Predict$Item_MRP
write.csv(Sub_Base_RF, file = "Sub_v1_RF.csv")


## Linear Regression with multiple variables
lr1 =lm(Item_Volume ~ . ,data = Train_Base_Predict)
PredictLinear1 <- predict(lr1,newdata = Test_Base_Predict[1:40])
Sub_Base_LM = Sub_Base
Sub_Base_LM$Item_Outlet_Sales = PredictLinear1*Test_Base_Predict$Item_MRP
write.csv(Sub_Base_LM, file = "Sub_v1_LM.csv")


