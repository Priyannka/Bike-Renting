rm(list=ls(all=T))

setwd("C:/Users/hp/OneDrive/Documents/edwisor/project")
getwd()

#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees')

#install.packages(x)
lapply(x, require, character.only = TRUE)
rm(x)


## Read the data
bike_df = read.csv("C:/Users/hp/Downloads/day.csv", header = T, na.strings = c(" ", "", "NA"))

##summary of data
summary(bike_df)

dim(bike_df)

sapply(bike_df, typeof)

###########################################Explore the data##########################################
str(bike_df)

#Exploratory data analysis

# for four variables dteday,season,yr,month,holiday,weekday,workingday,weatheersit are of integer type,we need to convert the variable to categorical/factor type
bike_df$season = as.factor(bike_df$season)
bike_df$yr = as.factor(bike_df$yr)
bike_df$mnth = as.factor(bike_df$mnth)
bike_df$holiday = as.factor(as.character(bike_df$holiday))
bike_df$weekday = as.factor(bike_df$weekday)
bike_df$workingday = as.factor(bike_df$workingday)
bike_df$weathersit = as.factor(bike_df$weathersit)
d1=unique(bike_df$dteday)
df=data.frame(d1)
bike_df$dteday=as.Date(df$d1,format="%Y-%m-%d")
df$d1=as.Date(df$d1,format="%Y-%m-%d")
bike_df$dteday=format(as.Date(df$d1,format="%Y-%m-%d"), "%d")
bike_df$dteday=as.factor(bike_df$dteday)



sapply(bike_df, typeof)
str(bike_df)


##################################Missing Values Analysis###############################################
missing_val = data.frame(apply(bike_df,2,function(x){sum(is.na(x))}))
missing_val$Columns = row.names(missing_val)
names(missing_val)[1] =  "Missing_percentage"
missing_val$Missing_percentage = (missing_val$Missing_percentage/nrow(bike_df)) * 100
missing_val = missing_val[order(-missing_val$Missing_percentage),]
row.names(missing_val) = NULL
missing_val = missing_val[,c(2,1)]
write.csv(missing_val, "Miising_perc.csv", row.names = F)
missing_val




############################################Outlier Analysis#############################################
# ## BoxPlots - Distribution and Outlier Check
numeric_index = sapply(bike_df,is.numeric) #selecting only numeric

numeric_data = bike_df[,numeric_index]

cnames = colnames(numeric_data)
# 
for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn3,gn2,ncol=2)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,ncol=2)

cnames

#from the above plot we can see outliers present in hum,casual,windspeed

# # #loop to remove from all variables
for(i in cnames){
  print(i)
  val = bike_df[,i][bike_df[,i] %in% boxplot.stats(bike_df[,i])$out]
  print(length(val))
  bike_df = bike_df[which(!bike_df[,i] %in% val),]
}

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "cnt"), data = subset(bike_df))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="cnt")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}
# ## Plotting plots together
gridExtra::grid.arrange(gn1,gn3,gn2,ncol=2)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)
gridExtra::grid.arrange(gn7,gn8,ncol=2)

##################################Feature Selection################################################
## Correlation Plot 
corrgram(bike_df[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


model <- lm(cnt ~ season + workingday + weathersit + yr + mnth + dteday, data = bike_df)
summary(model)
anova(model)


## Dimension Reduction
bike_df = subset(bike_df, 
                 select = -c(dteday,casual,registered,atemp,instant))

str(bike_df)


##################################Feature Scaling################################################
#Normality check
##not needed


rmExcept("bike_df")
rm(train)



##############Model Development###############################
############### MODELING #########################
#Divide the data into train and test

###########Decision Tree######################
#Load Libraries
library(rpart)
library(MASS)

#Load practice data


#Divide the data into train and test
#set.seed(123)
train_index = sample(1:nrow(bike_df), 0.8 * nrow(bike_df))
train = bike_df[train_index,]
test = bike_df[-train_index,]

# ##rpart for regression
fit = rpart(cnt ~ ., data = train, method = "anova")

#Predict for new test cases
predictions_DT = predict(fit, test[,-11])

#MAPE
#calculate MAPE
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

MAPE(test[,11], predictions_DT)

#Error Rate: 2,033858
#Accuracy: 89.67


##Linear Regression##############

#check multicollearity
library(usdm)
vif(bike_df[,-11])

##vifcor(bike_df[,-11], th = 0.9)

#run regression model
lm_model = lm(cnt ~., data = train)

#Summary of the model
summary(lm_model)
head(test)

#Predict
predictions_LR = predict(lm_model, test[,1:11])

#Calculate MAPE
MAPE(test[,11], predictions_LR)

#Error Rate: 1.307109
#acuracy: 91.



###Random Forest
RF_model = randomForest(cnt ~ ., train, importance = TRUE, ntree = 500)


#Presdict test data using random forest model
RF_Predictions = predict(RF_model, test[,-11])


predictions_RF=predict(RF_model,test[,-11])

#### Lets use RMSE to test accuracy of the Model


MAPE(test[,11],predictions_RF)

#Error Rate: 1.441646
#acuracy: 91.

#### Lets use RMSE to test accuracy of the Model
RMSE = function(y, yhat){
  sqrt(mean((y - yhat)^2))
}
RMSE(test[,11],predictions_RF)
##688.8319
RMSE(test[,11],predictions_DT)
##894.9066
RMSE(test[,11],predictions_LR)
###796.8933


######Extracting predicted values output from Rndom Forest Model#####################

results=data.frame(test,pred_cnt=predictions_RF)
write.csv(results,file = 'BikeRenting_output R.csv',row.names = FALSE,quote=FALSE)

##Visualisation#####################
#load libraries
library("ggplot2")
library("scales")
library("psych")
library("gplots")

head(results)

###########Bike Rental Analysis on Weather########################

ggplot(results, aes_string(x = results$weathersit , y=results$pred_cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("weathesit") + ylab('predicted_count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on weather") +  theme(text=element_text(size=15))



ggplot(bike_df, aes_string(x = bike_df$weathersit , y=bike_df$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("weathesit") + ylab('Actual Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on weather") +  theme(text=element_text(size=15))


###########Bike Rental Analysis on Season########################

ggplot(bike_df, aes_string(x = bike_df$season , y=bike_df$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("season") + ylab('Actual Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on season") +  theme(text=element_text(size=15))


ggplot(results, aes_string(x = results$season , y=results$pred_cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("weathesit") + ylab('predicted_count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on season") +  theme(text=element_text(size=15))

###########Bike Rental Analysis on holiday########################

ggplot(bike_df, aes_string(x = bike_df$holiday , y=bike_df$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("holiday") + ylab('Actual Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on holiday") +  theme(text=element_text(size=15))

###########Bike Rental Analysis on yr########################

ggplot(bike_df, aes_string(x = bike_df$yr , y=bike_df$cnt)) +
  geom_bar(stat="identity",fill =  "DarkSlateBlue") + theme_bw() +
  xlab("holiday") + ylab('Actual Count') + scale_y_continuous(breaks=pretty_breaks(n=10)) +
  ggtitle("Bike Rental Analysis on year") +  theme(text=element_text(size=15))







