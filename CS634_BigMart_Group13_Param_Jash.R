library(randomForest)
library(caret)
library(onehot)
library(ggplot2)
library(Metrics)
library(tree)
library(rpart)

## Multiplot
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Function to calculate Mode
Mode_Calc <- function(x){
  single = unique(x)
  single[which.max(tabulate(x))]
}

########################################## MODEL 1 #################################################

# Read text file
Train_Base = read.table("Train.csv",header = TRUE , sep = ",")

## Train

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

## Drop Outlet Size
Item_Identifier = Train_Base$Item_Identifier
Train_Base =  subset(Train_Base,select = -c(Item_Identifier))

## Create One Hot Data
Train_Base_Onehot = onehot(Train_Base,max_levels = 16)
Train_Base_Predict = predict(Train_Base_Onehot,Train_Base)
Train_Base_Predict = as.data.frame(Train_Base_Predict)

# Renaming Columns

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

# Split data into train and test
indexes = sample(1:nrow(Train_Base_Predict), size=0.7*nrow(Train_Base_Predict))
Train1 = as.data.frame(Train_Base_Predict[indexes,])
Test1 = as.data.frame(Train_Base_Predict[-indexes,])
Item_Identifier_Model1 = as.data.frame(Item_Identifier[-indexes])
Outlet_Identifier_1 = Train_Base$Outlet_Identifier[-indexes]
Outlet_Identifier_1 = as.data.frame(Outlet_Identifier)


###### Decision Tree #######

# Minsplit and RMSE intitialization
# Minsplit = 0
# RMSE = 0
# Decision Tree plots for different values of Minsplit and Minbucket
# for (i in 1:20) {
#   Minsplit[i] = i
#   ctrl = rpart.control(maxdepth = 4,minsplit = i,minbucket = round(i/3)) 
#   dt1 = rpart(Item_Outlet_Sales ~. , data = Train1,parms =  c(split = "gini"),control = ctrl)
#   PredictTree1 = predict(dt1,newdata = Test1[1:40])
#   RMSE[i] = rmse(Test1$Item_Outlet_Sales,PredictTree1)
# }

# Create a data frame dt_plot for plotting
# dt_plot <- NULL
# dt_plot$Minsplit <- Minsplit
# dt_plot$RMSE <- RMSE
# dt_plot = as.data.frame(dt_plot)

# plot the graph
#ggplot(dt_plot,aes(x = Minsplit, y= RMSE)) + ggtitle("Minsplit and Minbucket  vs RMSE") + labs(x="Minsplit",y="RMSE") +geom_line(colour = "red")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))

## Decision Tree for optimal parameter
ctrl = rpart.control(maxdepth = 4,minsplit = 20,minbucket = 7) 
dt1 = rpart(Item_Outlet_Sales ~. , data = Train1,parms =  c(split = "gini"),control = ctrl)
PredictTree1 = predict(dt1,newdata = Test1[1:40])
rmse(Test1$Item_Outlet_Sales,PredictTree1)
Dt_Model1  = data.frame(Item_Identifier_Model1,Outlet_Identifier_1,PredictTree1)
Dt_Model1 = as.data.frame(Dt_Model1)
write.csv(Dt_Model1, file = "Model_1_Decision_Tree.csv")


########### Random Forest ############
## Create rf_plot
# rf_plot <- NULL
# rf_plot$Id <- 1
# rf_plot$Ntree = c("100","300","500","700","1000")
# rf_plot$Ntree = as.numeric(rf_plot$Ntree)
# rf_plot$RMSE_Tree = 0
# rf_plot = as.data.frame(rf_plot)
# 
# ## create rf_plot1
# rf_plot1 <- NULL
# rf_plot1$MTry = c("5","10","15","20","25","30","35","40")
# rf_plot1$MTry = as.numeric(rf_plot1$MTry)
# rf_plot1$RMSE_Mtry = 0
# rf_plot1 = as.data.frame(rf_plot1)
# 
# ## Create rf_plot2
# rf_plot2 <- NULL
# rf_plot2$Maxnodes = c("10","20","30","40")
# rf_plot2$Maxnodes = as.numeric(rf_plot2$Maxnodes)
# rf_plot2$RMSE_MN = 0
# rf_plot2 = as.data.frame(rf_plot2)

# ## Random Forest for different value of Trees 
# for (i in 1:5) {
#   rf1 =randomForest(Item_Outlet_Sales ~ . ,data = Train1,ntree = rf_plot$Ntree[i])
#   PredictForest1 <- predict(rf1, newdata = Test1[1:40])
#   rf_plot$RMSE_Tree[i] = rmse(Test1$Item_Outlet_Sales,PredictForest1)
# }
# 
# ## Random Forest for different value of MTry
# for (i in 1:8) {
#   rf1 =randomForest(Item_Outlet_Sales ~ . ,data = Train1,ntree = 700,MTry = rf_plot1$MTry[i])
#   PredictForest1 <- predict(rf1, newdata = Test1[1:40])
#   rf_plot1$RMSE_Mtry[i] = rmse(Test1$Item_Outlet_Sales,PredictForest1)
# }
# 
# ## Random Forest for different value of Maxnodes
# for (i in 1:4) {
#   rf1 =randomForest(Item_Outlet_Sales ~ . ,data = Train1,ntree = 700,MTry = 20,Maxnodes = rf_plot2$Maxnodes[i])
#   PredictForest1 <- predict(rf1, newdata = Test1[1:40])
#   rf_plot2$RMSE_MN[i] = rmse(Test1$Item_Outlet_Sales,PredictForest1)
# }
# 
# # plot for NTrees vs RMSE for Random Forest
# rf1 <- ggplot(rf_plot,aes(x= Ntree,y=RMSE_Tree)) + ggtitle("No of Trees  vs RMSE") + labs(x="Trees",y="RMSE") +geom_line(colour = "red")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))
# 
# # plot for MTry vs RMSE for Random Forest
# rf2 <- ggplot(rf_plot1,aes(x = MTry, y= RMSE_Mtry)) + ggtitle("No of MTry  vs RMSE") + labs(x="MTry",y="RMSE") +geom_line(colour = "red")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))
# 
# # plot for Maxnnodes vs RMSE for Random Forest
# rf3 <- ggplot(rf_plot2,aes(x = Maxnodes, y= RMSE_MN)) + ggtitle("No of Maxnodes  vs RMSE") + labs(x="Maxnodes",y="RMSE") +geom_line(colour = "red")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))

rf1 =randomForest(Item_Outlet_Sales ~ . ,data = Train1,ntree = 700,MTry = 20, Maxnodes = 30)
PredictForest1 <- predict(rf1, newdata = Test1[1:40])
rmse(Test1$Item_Outlet_Sales,PredictForest1)
Rf_Model1  = data.frame(Item_Identifier_Model1,Outlet_Identifier_1,PredictForest1)
Rf_Model1 = as.data.frame(Rf_Model1)
write.csv(Rf_Model1, file = "Model_1_Random_Forest.csv")


########### Linear Regression with multiple variables  ############

lr1 =lm(Item_Outlet_Sales ~ . ,data = Train1)
PredictLinear1 <- predict(lr1,newdata = Test1[1:40])
rmse(Test1$Item_Outlet_Sales,PredictLinear1)
Lm_Model1  = data.frame(Item_Identifier_Model1,Outlet_Identifier_1,PredictLinear1)
Lm_Model1 = as.data.frame(Lm_Model1)
write.csv(Lm_Model1, file = "Model_1_Regression.csv")


#################################################### MODEL 2 ######################################################

# Read text file
Train_2 = read.table("Train.csv",header = TRUE , sep = ",")

## Train

## Replace Missing vaues in Item_Weight in mean
Train_2[is.na(Train_2)] <- 0
Train_2$Item_Weight[Train_2$Item_Weight == 0]<- mean(Train_2$Item_Weight)

## REplace 0 in Item_Visibility by Mean
Train_2$Item_Visibility[Train_2$Item_Visibility == 0]<- mean(Train_2$Item_Visibility)

## Replace year with corresponding number 
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 1985]<- 2013-1985
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 1987]<- 2013-1987
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 1997]<- 2013-1997
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 1998]<- 2013-1998
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 1999]<- 2013-1999
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 2002]<- 2013-2002
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 2004]<- 2013-2004
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 2007]<- 2013-2007
Train_2$Outlet_Establishment_Year[Train_2$Outlet_Establishment_Year == 2009]<- 2013-2009

## Replace 'LF' and 'low fat' with 'Low Fat' and 'reg' with 'Regular'
Train_2$Item_Fat_Content[Train_2$Item_Fat_Content == 'LF'] <- 'Low Fat'
Train_2$Item_Fat_Content[Train_2$Item_Fat_Content == 'low fat'] <- 'Low Fat'
Train_2$Item_Fat_Content[Train_2$Item_Fat_Content == 'reg'] <- 'Regular'
Train_2$Item_Fat_Content = droplevels(Train_2$Item_Fat_Content)

## Replace missing values in Outle_Size by mode
Train_2$Outlet_Size[Train_2$Outlet_Size == ""] <- Mode_Calc(Train_2$Outlet_Size)
Train_2$Outlet_Size = droplevels(Train_2$Outlet_Size)
Train_2$Outlet_Size = as.numeric(Train_2$Outlet_Size)

## Convert Outlet_Location_Type into numeric
Train_2$Outlet_Size = as.numeric(Train_2$Outlet_Size)

## Item Visibility normalization
Train_2$Item_Visibility = Train_2$Item_Visibility*100

## Create Item Volume Sold.
Train_2["Item_Volume"] = Train_2$Item_Outlet_Sales/Train_2$Item_MRP
Train_2$Item_Volume = round(Train_2$Item_Volume)
Train_2$Item_Outlet_Sales = NULL

## Drop Outlet Size
Item_Identifier = Train_2$Item_Identifier
Train_2 =  subset(Train_2,select = -c(Item_Identifier))

## Create One Hot Data
Train_2_Onehot = onehot(Train_2,max_levels = 16)
Train_2_Predict = predict(Train_2_Onehot,Train_2)
Train_2_Predict = as.data.frame(Train_2_Predict)

# Renaming Columns

colnames(Train_2_Predict)[2] <- "LowFat"
colnames(Train_2_Predict)[3] <- "Regular"

colnames(Train_2_Predict)[5] <- "Baking_Goods"
colnames(Train_2_Predict)[6] <- "Breads"
colnames(Train_2_Predict)[7] <- "Breakfast"
colnames(Train_2_Predict)[8] <- "Canned"
colnames(Train_2_Predict)[9] <- "Dairy"
colnames(Train_2_Predict)[10] <- "Frozen_Foods"
colnames(Train_2_Predict)[11] <- "Fruit_and_Vegetables"
colnames(Train_2_Predict)[12] <- "Hard_Drinks"
colnames(Train_2_Predict)[13] <- "Health_and_Hygine"
colnames(Train_2_Predict)[14] <- "Household"
colnames(Train_2_Predict)[15] <- "Meat"
colnames(Train_2_Predict)[16] <- "Others"
colnames(Train_2_Predict)[17] <- "Seafood"
colnames(Train_2_Predict)[18] <- "Snack_Foods"
colnames(Train_2_Predict)[19] <- "Soft_Drinks"
colnames(Train_2_Predict)[20] <- "Starch_Foods"

colnames(Train_2_Predict)[22] <- "OUT10"
colnames(Train_2_Predict)[23] <- "OUT13"
colnames(Train_2_Predict)[24] <- "OUT17"
colnames(Train_2_Predict)[25] <- "OUT18"
colnames(Train_2_Predict)[26] <- "OUT19"
colnames(Train_2_Predict)[27] <- "OUT27"
colnames(Train_2_Predict)[28] <- "OUT35"
colnames(Train_2_Predict)[29] <- "OUT45"
colnames(Train_2_Predict)[30] <- "OUT46"
colnames(Train_2_Predict)[31] <- "OUT49"

colnames(Train_2_Predict)[34] <- "Tier1"
colnames(Train_2_Predict)[35] <- "Tier2"
colnames(Train_2_Predict)[36] <- "Tier3"

colnames(Train_2_Predict)[37] <- "Grocery"
colnames(Train_2_Predict)[38] <- "SuperMarket_1"
colnames(Train_2_Predict)[39] <- "SuperMarket_2"
colnames(Train_2_Predict)[40] <- "SuperMarket_3"


# Split data into train and test
indexes = sample(1:nrow(Train_2_Predict), size=0.7*nrow(Train_2_Predict))
Train2 = as.data.frame(Train_2_Predict[indexes,])
Test2 = as.data.frame(Train_2_Predict[-indexes,])
Item_Identifier_Model2 = as.data.frame(Item_Identifier[-indexes])
Outlet_Identifier_2 = Train_Base$Outlet_Identifier[-indexes]
Outlet_Identifier_2 = as.data.frame(Outlet_Identifier)


## Decision Tree

# # Minsplit and RMSE intitialization
# Minsplit = 0
# RMSE = 0
# 
# # Decision Tree plots for different values of Minsplit and Minbucket
# for (i in 1:20) {
#   Minsplit[i] = i
#   ctrl = rpart.control(maxdepth = 4,minsplit = i,minbucket = round(i/3)) 
#   dt2 = rpart(Item_Volume ~. , data = Train2,parms =  c(split = "gini"),control = ctrl)
#   PredictTree2 = predict(dt2,newdata = Test2[1:40])
#   RMSE[i] =rmse(Test2$Item_Weight*Test2$Item_MRP,PredictTree2*Test2$Item_MRP)
# }
# 
# # Create a data frame dt_plot for plotting
# dt_plot <- NULL
# dt_plot$Minsplit <- Minsplit
# dt_plot$RMSE <- RMSE
# dt_plot = as.data.frame(dt_plot)
# 
# # plot the graph
# ggplot(dt_plot,aes(x = Minsplit, y= RMSE)) + ggtitle("Minsplit and Minbucket  vs RMSE") + labs(x="Minsplit",y="RMSE") +geom_line(colour = "red")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))

ctrl = rpart.control(maxdepth = 4,minsplit = 1,minbucket = 1) 
dt2 = rpart(Item_Volume ~. , data = Train2,parms =  c(split = "gini"),control = ctrl)
PredictTree2 = predict(dt2,newdata = Test2[1:40])
rmse(Test2$Item_Volume*Test2$Item_MRP,PredictTree2*Test2$Item_MRP)
Dt_Model2  = data.frame(Item_Identifier_Model2,Outlet_Identifier_2,PredictTree2*Test2$Item_MRP)
Dt_Model2 = as.data.frame(Dt_Model2)
write.csv(Dt_Model2, file = "Model_2_Decision_Tree.csv")


################## Random Forest  #################
# ## Create rf_plot_N
# rf_plot_N <- NULL
# rf_plot_N$Id <- 2
# rf_plot_N$Ntree = c("100","300","500","700","1000")
# rf_plot_N$Ntree = as.numeric(rf_plot_N$Ntree)
# rf_plot_N$RMSE_Tree = 0
# rf_plot_N = as.data.frame(rf_plot_N)
# 
# ## Create rf_plot1_N
# rf_plot1_N <- NULL
# rf_plot1_N$MTry = c("5","10","15","20","25","30","35","40")
# rf_plot1_N$MTry = as.numeric(rf_plot1_N$MTry)
# rf_plot1_N$RMSE_Mtry = 0
# rf_plot1_N = as.data.frame(rf_plot1_N)
# 
# ## Create rf_plot2_N
# rf_plot2_N <- NULL
# rf_plot2_N$Maxnodes = c("10","20","30","40")
# rf_plot2_N$Maxnodes = as.numeric(rf_plot2_N$Maxnodes)
# rf_plot2_N$RMSE_MN = 0
# rf_plot2_N = as.data.frame(rf_plot2_N)
# 
# ## Random Forest for different value of Trees 
# for (i in 1:5) {
#   rf2 =randomForest(Item_Volume ~ . ,data = Train2,ntree = rf_plot_N$Ntree[i])
#   PredictForest2 <- predict(rf2, newdata = Test2[1:40])
#   rf_plot_N$RMSE_Tree[i] =rmse(Test2$Item_Volume*Test2$Item_MRP,PredictForest2*Test2$Item_MRP)
# }
# 
# ## Random Forest for different value of MTry
# for (i in 1:8) {
#   rf2 =randomForest(Item_Volume ~ . ,data = Train2,ntree = 500,MTry = rf_plot1_N$MTry[i])
#   PredictForest2 <- predict(rf2, newdata = Test2[1:40])
#   rf_plot1_N$RMSE_Mtry[i] =rmse(Test2$Item_Volume*Test2$Item_MRP,PredictForest2*Test2$Item_MRP)
# }
# 
# ## Random Forest for different value of Maxnodes
# for (i in 1:4) {
#   rf2 =randomForest(Item_Volume ~ . ,data = Train2,ntree = 500,MTry = 35,Maxnodes = rf_plot2_N$Maxnodes[i])
#   PredictForest2 <- predict(rf2, newdata = Test2[1:40])
#   rf_plot2_N$RMSE_MN[i] =rmse(Test2$Item_Volume*Test2$Item_MRP,PredictForest2*Test2$Item_MRP)
# }
# 
# # plot for NTrees vs RMSE for Random Forest
# rf1_N <- ggplot(rf_plot_N,aes(x = Ntree, y= RMSE_Tree)) + ggtitle("No of Trees  vs RMSE") + labs(x="Trees",y="RMSE") +geom_line(colour = "blue")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))
# 
# # plot for MTry vs RMSE for Random Forest
# rf2_N <- ggplot(rf_plot1_N,aes(x = MTry, y= RMSE_Mtry)) + ggtitle("No of MTry  vs RMSE") + labs(x="MTry",y="RMSE") +geom_line(colour = "blue")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))
# 
# # plot for Maxnnodes vs RMSE for Random Forest
# rf3_N <- ggplot(rf_plot2_N,aes(x = Maxnodes, y= RMSE_MN)) + ggtitle("No of Maxnodes  vs RMSE") + labs(x="Maxnodes",y="RMSE") +geom_line(colour = "blue")+ geom_point() +theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))
#
# ## Multiplot 
# multiplot(rf1,rf1_N,cols = 2)
# multiplot(rf2,rf2_N,cols = 2)
# multiplot(rf3,rf3_N,cols = 2)

rf2 =randomForest(Item_Volume ~ . ,data = Train2,ntree = 500,MTry = 35,Maxnodes = 30)
PredictForest2 <- predict(rf2, newdata = Test2[1:40])
rmse(Test2$Item_Volume*Test2$Item_MRP,PredictForest2*Test2$Item_MRP)
Rf_Model2  = data.frame(Item_Identifier_Model2,Outlet_Identifier_2,PredictForest2*Test2$Item_MRP)
Rf_Model2 = as.data.frame(Rf_Model2)
write.csv(Rf_Model2, file = "Model_2_Random_Forest.csv")



############## Linear Regression with multiple variables  ##############
lr2 =lm(Item_Volume ~ . ,data = Train2)
PredictLinear2 <- predict(lr2,newdata = Test2[1:40])
rmse(Test2$Item_Volume*Test2$Item_MRP, PredictLinear2*Test2$Item_MRP)
Lm_Model2  = data.frame(Item_Identifier_Model2,Outlet_Identifier_2,PredictLinear2*Test2$Item_MRP)
Lm_Model2 = as.data.frame(Lm_Model2)
write.csv(Lm_Model2, file = "Model_2_Regression.csv")




################################################### RFE #######################################################

## RFE for Random Forest
RF = rfe(Train_Base_Predict[,1:40],Train_Base_Predict$Item_Outlet_Sales,size = seq(1,5),metric = "RMSE",rfeControl = rfeControl(functions = rfFuncs,method = "repeatedcv",number = 5))
plot(RF, type=c("g", "o"))

## RFE for Linear Regression
LM = rfe(data.frame(Train_Base_Predict[,1:40]),Train_Base_Predict$Item_Outlet_Sales,size = seq(1,40),metric = "RMSE",rfeControl = rfeControl(functions = lmFuncs,method = "repeatedcv",number = 5))
plot(LM, type=c("g","o"))

## RFE for Decision Tree
DT = rfe(data.frame(Train_Base_Predict[,1:40]),Train_Base_Predict$Item_Outlet_Sales,size = seq(1,40),metric = "RMSE",rfeControl = rfeControl(functions = treebagFuncs,method = "repeatedcv",number = 5))
plot(DT, type=c("g","o"))


############################################### Graphs ##########################################################

# Read text file
Train_Graph = read.table("Train.csv",header = TRUE , sep = ",")

## Train

## Replace Missing vaues in Item_Weight in mean
Train_Graph[is.na(Train_Graph)] <- 0
Train_Graph$Item_Weight[Train_Graph$Item_Weight == 0]<- mean(Train_Graph$Item_Weight)

## REplace 0 in Item_Visibility by Mean
Train_Graph$Item_Visibility[Train_Graph$Item_Visibility == 0]<- mean(Train_Graph$Item_Visibility)

## Replace year with corresponding number 
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 1985]<- 2013-1985
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 1987]<- 2013-1987
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 1997]<- 2013-1997
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 1998]<- 2013-1998
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 1999]<- 2013-1999
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 2002]<- 2013-2002
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 2004]<- 2013-2004
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 2007]<- 2013-2007
Train_Graph$Outlet_Establishment_Year[Train_Graph$Outlet_Establishment_Year == 2009]<- 2013-2009



## Replace 'LF' and 'low fat' with 'Low Fat' and 'reg' with 'Regular'
Train_Graph$Item_Fat_Content[Train_Graph$Item_Fat_Content == 'LF'] <- 'Low Fat'
Train_Graph$Item_Fat_Content[Train_Graph$Item_Fat_Content == 'low fat'] <- 'Low Fat'
Train_Graph$Item_Fat_Content[Train_Graph$Item_Fat_Content == 'reg'] <- 'Regular'
Train_Graph$Item_Fat_Content = droplevels(Train_Graph$Item_Fat_Content)

## Replace missing values in Outle_Size by mode
Train_Graph$Outlet_Size[Train_Graph$Outlet_Size == ""] <- Mode_Calcalc(Train_Graph$Outlet_Size)
Train_Graph$Outlet_Size = droplevels(Train_Graph$Outlet_Size)
Train_Graph$Outlet_Size = as.numeric(Train_Graph$Outlet_Size)
## replace 

## Drop Outlet Size
#Train_Graph =  subset(Train_Graph,select = -c(Item_Identifier))

## Graphs

## Outlet_Establishment_Year vs Item_Outlet_Sales
Graph_1 = aggregate(Train_Graph$Item_Outlet_Sales, by=list(Category=Train_Graph$Outlet_Establishment_Year), FUN=sum)
for (i in 1:9) {
  Graph_1$Cat_Num[i] = i
}
ggplot(Graph_1, aes(Cat_Num, y = x)) + ggtitle("Total Item Sales vs Outlet_Establishment_Year") + labs(x="Year",y="Total_Sales")  + geom_bar(stat="identity", width=.5, fill="red")+theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Total Item_Outlet_Sales vs Item_Type
Graph_2 = aggregate(Train_Graph$Item_Outlet_Sales, by=list(Category=Train_Graph$Item_Type), FUN=sum)
for (i in 1:16) {
  Graph_2$Cat_Num[i] = i
}
ggplot(Graph_2, aes(Category, y = x)) + ggtitle("Item Sales vs Item Category") + labs(x="Category",y="Total_Sales")  + geom_bar(stat="identity", width=.5, fill="red")+theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Item_Type vs Visibility
Graph_3 = aggregate(Train_Graph$Item_Visibility, by=list(Category=Train_Graph$Item_Type), FUN=sum)
for (i in 1:16) {
  Graph_3$Cat_Num[i] = i
}
ggplot(Graph_3, aes(Category, y = x)) + ggtitle("Item Visibility vs Item Category") + labs(x="Category",y="Total_Visibility") + geom_bar(stat="identity", width=.5, fill="blue")+theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Graph_Combi
Graph_Combi_1 <- NA
Graph_Combi_1$Id_Main = 1
Graph_Combi_1$Category <- Graph_2$Category
Graph_Combi_1$Item_Sales <- Graph_2$x
Graph_Combi_1$Id <- Graph_2$Cat_Num
Graph_Combi_1 = as.data.frame(Graph_Combi_1)

Graph_Combi_2 <- NA
Graph_Combi_2$Id_Main = 2
Graph_Combi_2$Category <- Graph_3$Category
Graph_Combi_2$Item_Visibility <- Graph_3$x
Graph_Combi_2$Id <- Graph_3$Cat_Num
Graph_Combi_2 = as.data.frame(Graph_Combi_2)

Graph_Combi <- data.frame(Id_Main = c(Graph_Combi_2$Id_Main,Graph_Combi_1$Id_Main),Value = c(as.integer(Graph_Combi_2$Item_Visibility),as.integer(Graph_Combi_1$Item_Sales/100000)),Category=c(Graph_Combi_2$Id,Graph_Combi_1$Id))
Graph_Combi["Cat_Name"] <- Graph_Combi_1$Category

p <-ggplot(Graph_Combi, aes(Cat_Name,Value))
p + ggtitle("(Item Visibility & Item Sales) based on Item Category")+ labs(x ="Category",y="Value") +geom_bar(stat = "identity", aes(fill=Id_Main),position = "dodge") +theme(axis.text.x = element_text(angle=90, vjust=0.6))

ggplot(Train_Graph,aes(x = Item_Type, y= Item_MRP)) + ggtitle("Item_Type vs Item_MRP") + labs(x="Item_Type",y="Item_MRP") +geom_bar(stat = "identity",width = .5,colour=Train_Graph$Outlet_Identifier)

## Outlet_Type vs Total_Item_Outlet_Sales
Graph_4 = aggregate(Train_Graph$Item_Outlet_Sales, by=list(Category=Train_Graph$Outlet_Type), FUN=sum)
for (i in 1:4) {
  Graph_4$Cat_Num[i] = i
}
ggplot(Graph_4, aes(Category, y = x)) + ggtitle("Total_Item_Sales vs Outlet_Type") + labs(x="Category",y="Total_Item_Sales") + geom_bar(stat="identity", width=.5, fill="blue")+theme(axis.text.x = element_text(angle=65, vjust=0.6))

## Item_MRP vs Total_Item_Sales
ggplot(Train_Graph,aes(x = Item_MRP, y = Item_Outlet_Sales)) + ggtitle("Item_Outlet_Sales vs Item_MRP") + labs(x="Item_MRP",y="Item_Outlet_Sales") + geom_point(stat="identity",position = "identity",aes(colour = Outlet_Type,shape = Outlet_Type)) +theme(axis.text.x = element_text(angle=65, vjust=0.6)) +scale_x_continuous(breaks = c(0,50,100,150,200,250,300))
ggplot(Train_Graph,aes(x = Item_MRP, y = Item_Outlet_Sales)) + ggtitle("Item_Outlet_Sales vs Item_MRP") + labs(x="Item_MRP",y="Item_Outlet_Sales") + geom_point(stat="identity",position = "identity",aes(colour = Item_Type)) +theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Item_Visiblity vs Item_Outlet_Sales
ggplot(Train_Graph,aes(x = Item_Visibility, y = Item_Outlet_Sales)) + ggtitle("Item_Outlet_Sales vs Item_Visibility") + labs(x="Item_Visibility",y="Item_Outlet_Sales") + geom_point(stat="identity",position = "identity",aes(colour = Outlet_Type)) +theme(axis.text.x = element_text(angle=65, vjust=0.6))
ggplot(Train_Graph,aes(x = Item_Visibility, y = Item_Outlet_Sales)) + ggtitle("Item_Outlet_Sales vs Item_Visibility") + labs(x="Item_Visibility",y="Item_Outlet_Sales") + geom_point(stat="identity",position = "identity",aes(colour = Item_Type)) +theme(axis.text.x = element_text(angle=65, vjust=0.6))

# Item_Weight (Missing  Values before and after)
ggplot(Train_Graph,aes(x = Item_Type, y= Item_Weight)) + ggtitle("Item_Type vs Item_Weight") + labs(x="Item_Type",y="Item_Weight") +geom_bar(stat = "identity",width = .5,fill="red")+theme(axis.text.x = element_text(angle=65, vjust=0.6),axis.text.y = element_text(angle=65, vjust=0.6))


