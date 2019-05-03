###Clear the saved variables
rm(list=ls(all=T))

###Importing necessary Libraries

x = c("ggplot2", "geosphere", "usdm", "rpart", "MASS", "DMwR","randomForest")
lapply(x, require, character.only = TRUE)
rm(x)



#Setting the working directory
setwd("F:/DS/Project 2")
#Display current working directory
getwd()

#removing impurities after manual observations such as improper date and lat long format 
fare=read.csv("train_cab.csv",header = TRUE)[-c(1182,1328,1124),]
str(fare)


#Read the test data
fare_test=read.csv("test.csv",header = TRUE)
str(fare_test)




#Necessary Type conversions
fare$fare_amount=as.numeric(as.character(fare$fare_amount))
fare$pickup_datetime=as.POSIXct(fare$pickup_datetime,format="%Y-%m-%d %H:%M:%S")




#Necessary Type conversions
fare_test$pickup_datetime=as.POSIXct(fare_test$pickup_datetime,format="%Y-%m-%d %H:%M:%S")
fare_test$passenger_count=as.numeric(fare_test$passenger_count)

head(fare)
head(fare_test)

###Missing value Analysis
Missing_val=data.frame(apply(fare,2,function(x){sum(is.na(x))}))
colnames(Missing_val)="Percentage of Missing Values"
Missing_val[1]=((Missing_val[1]/(nrow(fare)))*100)
Missing_val$Variables=row.names(Missing_val)
row.names(Missing_val)=NULL

Missing_val=Missing_val[,c(2,1)]

#We decide to drop such incorrect observations
fare=na.omit(fare)


#Summarise the dataframe to get a brief idea
summary(fare)
summary(fare_test)


#Outlier analysis
#Lattitude----(-90 to 90)
#Longitude----(-180 to 180)


numeric_index = sapply(fare,is.numeric) #selecting only numeric
numeric_data = fare[,numeric_index]
cnames = colnames(numeric_data)


# Get the Boxplots of various variables
 for (i in 1:length(cnames))
 {
   assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(fare))+ 
            stat_boxplot(geom = "errorbar", width = 0.5) +
            geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                         outlier.size=1, notch=FALSE) +
            theme(legend.position="bottom")+
            ggtitle(paste("Box plot of ",cnames[i])))
 }





#Visualize the varibles for outliers
gridExtra::grid.arrange(gn1,gn2,gn3,ncol=3)
gridExtra::grid.arrange(gn4,gn5,gn6,ncol=3)


#Function to remove the outliers present in the variable
FindOutliers <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  result <- which(data > extreme.threshold.upper | data < extreme.threshold.lower)
}


#Remove outliers from pickup_latitude
temp <- FindOutliers(fare$pickup_latitude)
#length(temp)
fare=fare[-temp,]


#Remove outliers from pickup_longitude
temp <- FindOutliers(fare$pickup_longitude)
#length(temp)
fare=fare[-temp,]

#Remove outliers from dropoff_latitude
temp <- FindOutliers(fare$dropoff_latitude)
#length(temp)
fare=fare[-temp,]

#Remove outliers from dropoff_latitude
temp <- FindOutliers(fare$dropoff_longitude)
#length(temp)
fare=fare[-temp,]

#Find the distribution of target variable
d <- density(fare$fare_amount) # returns the density data 
plot(d) # plots the results


# Fare amount can't be 0 or less than 0 hence the minimum value
FindOutliersfareamount <- function(data) {
  lowerq = quantile(data)[2]
  upperq = quantile(data)[4]
  iqr = upperq - lowerq #Or use IQR(data)
  # we identify extreme outliers
  extreme.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.lower = lowerq - (iqr * 1.5)
  result <- which(data > extreme.threshold.upper | data <= 0)
}


#Remove outliers from fare_amount
temp <- FindOutliersfareamount(fare$fare_amount)
#length(temp)
fare=fare[-temp,]


#From the test set and boxplot, it is implied that the range of passenger count is within 1-6
FindOutlierspassengercount <- function(data) {
  result <- which(data > 6 | data < 1)
}

#Remove outliers from passenger_count
temp <- FindOutlierspassengercount(fare$passenger_count)
#length(temp)
fare=fare[-temp,]



#Feature Extraction
#Feature Extraction from existing available variables
for (i in 1:nrow(fare)) {
 fare$dist[i]=distHaversine(c(fare$pickup_longitude[i], fare$pickup_latitude[i]),
                            c(fare$dropoff_longitude[i], fare$dropoff_latitude[i]), r=6378137)
 fare$dist[i]=fare$dist[i]/1000
 #in Km
}


####Extract features for test data as well
for (i in 1:nrow(fare_test)) {
  fare_test$dist[i]=distHaversine(c(fare_test$pickup_longitude[i], fare_test$pickup_latitude[i]),
                             c(fare_test$dropoff_longitude[i], fare_test$dropoff_latitude[i]), r=6378137)
  fare_test$dist[i]=fare_test$dist[i]/1000
  #in Km
}

#Describing the variables
summary(fare)

###Distance can't be 0 so removing the incorrect observations
fare = fare[which((fare$pickup_latitude != fare$dropoff_latitude) & 
              (fare$pickup_longitude != fare$dropoff_longitude)),]

#Removed all observations with 0 distance
summary(fare)


#Plots 
x=fare$pickup_longitude
y=fare$pickup_latitude


a=fare$dropoff_longitude
b=fare$dropoff_latitude

#Scatter plot of Pickups 
plot(x, y, main = "Pickups",
     xlab = "pickup_longitude", ylab = "pickup_latitude",
     pch = 19, frame = FALSE,col.sub="blue")

#Scatter plot of Dropoffs
plot(a, b, main = "Dropoffs",
     xlab = "dropoff_longitude", ylab = "dropoff_latitude",
     pch = 19, frame = FALSE)



#Plotting the distribution of Distance covered
e <- density(fare$dist) # returns the density data 
plot(e,main="Distribution of Trip Distance ")




#Plot of Distance VS Fare
distance=fare$dist
amount=fare$fare_amount

plot(distance, amount, main = "Dropoffs",
     xlab = "Trip Distance", ylab = "Fare Amount",
     pch = 19, frame = FALSE)


####Feature Extraction from Datetime variable


fare$pickup_day=as.numeric(format(fare$pickup_datetime, format = "%d",  usetz = FALSE))
fare$pickup_hour=as.numeric(format(fare$pickup_datetime, format = "%H",  usetz = FALSE))
fare$pickup_month=as.numeric(format(fare$pickup_datetime, format = "%m",  usetz = FALSE))
fare$pickup_year=as.numeric(format(fare$pickup_datetime, format = "%Y",  usetz = FALSE))
fare$pickup_day_of_week=as.numeric(format(fare$pickup_datetime, format = "%u",  usetz = FALSE))



for (i in 1:nrow(fare)) {
  fare$late_night[i]= ifelse((fare$pickup_hour[i] <= 6 || fare$pickup_hour[i] >= 20), 1, 0)
  
}


###Applying same for test data
fare_test$pickup_day=as.numeric(format(fare_test$pickup_datetime, format = "%d",  usetz = FALSE))
fare_test$pickup_hour=as.numeric(format(fare_test$pickup_datetime, format = "%H",  usetz = FALSE))
fare_test$pickup_month=as.numeric(format(fare_test$pickup_datetime, format = "%m",  usetz = FALSE))
fare_test$pickup_year=as.numeric(format(fare_test$pickup_datetime, format = "%Y",  usetz = FALSE))
fare_test$pickup_day_of_week=as.numeric(format(fare_test$pickup_datetime, format = "%u",  usetz = FALSE))



for (i in 1:nrow(fare_test)) {
  fare_test$late_night[i]= ifelse((fare_test$pickup_hour[i] <= 6 || fare_test$pickup_hour[i] >= 20), 1, 0)
  
}

###Feature Distribution against Target variable


#pickup_day_of_week
tbl <- with(fare, table(pickup_day_of_week))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="pickup_day_of_week",
        ylab="Num_Trips",
        col="lightgreen")
ggplot(aes(x = pickup_day_of_week, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over week days")
 
 
#pickup_year
tbl <- with(fare, table(pickup_year))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="pickup_year",
        ylab="Num_Trips",
        col="lightblue")
ggplot(aes(x = pickup_year, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over Years")


#late_night
tbl <- with(fare, table(late_night))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="late_night",
        ylab="Num_Trips",
        col="lightblue")
ggplot(aes(x = late_night, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over week Night and day")


#pickup_month
tbl <- with(fare, table(pickup_month))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="pickup_month",
        ylab="Num_Trips",
        col="lightblue")
ggplot(aes(x = pickup_month, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount overMonths")


#pickup_hour
tbl <- with(fare, table(pickup_hour))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="pickup_hour",
        ylab="Num_Trips",
        col="pink")
ggplot(aes(x = pickup_hour, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over Hours")


#passenger_count
tbl <- with(fare, table(passenger_count))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="passenger_count",
        ylab="Num_Trips",
        col="pink")
ggplot(aes(x = passenger_count, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over Passenger Count")




##pickup_day
days=fare$pickup_day
Fareamount=fare$fare_amount

plot(days, Fareamount,
     xlab = "Day", ylab = "Fare",
     pch = 19, frame = FALSE)

tbl <- with(fare, table(pickup_day))
barplot(tbl, beside = TRUE, legend = FALSE,
        xlab="pickup_day",
        ylab="Num_Trips",
        col="pink")
ggplot(aes(x = pickup_day, y = fare_amount), data = fare) + 
  stat_summary(fun.y = "mean", geom = "bar")+ggtitle("Avg Fare Amount over Days")


str(fare)

#Drop unnecessary variables and divide
#Training set into train and validation set

fare=fare[,-2]
fare_test=fare_test[,-1]

#25% Validation set
train_index = sample(1:nrow(fare), 0.75 * nrow(fare))
train = fare[train_index,]
test = fare[-train_index,]


#Error Metric
MAPE = function(y, yhat){
  mean(abs((y - yhat)/y))
}

####Model Development


#Linear Regression
#check multicollearity

vif(fare[,-1])
vifcor(fare[,-1], th = 0.9)
#No Multicollinearity present


#run regression model
model_LR = lm(fare_amount ~., data = train)

#Summary of the model
summary(model_LR)

#Predict
predictions_LR = predict(model_LR, test[,2:13])

#Calculate Accuracy
Accuracy1=100-MAPE(test[,1], predictions_LR)*100

#Linear Regression Evaluation Metric
regr.eval(test[,1], predictions_LR,stats=c("mape","rmse"))

summary(model_LR)$r.squared 
print(Accuracy1)





#Decision tree


# fit model
model_DT = rpart(fare_amount ~ ., data = train, method = "anova")
# summarize the Decision Tree
summary(model_DT)
# predict
predictions_DT <- predict(model_DT, test[,2:13])


#Calculate Accuracy
Accuracy2=100-MAPE(test[,1], predictions_DT)*100


#Decision Tree Evaluation Metric
regr.eval(test[,1], predictions_DT,stats=c("mape","rmse"))

print(Accuracy2)


#Random Forest

model_RF<- randomForest(fare_amount ~ ., data = train, importance = TRUE)

summary(model_RF)

#predict
predictions_RF <- predict(model_RF, test[,2:13])
# summarize accuracy

#Calculate Accuracy
Accuracy3=100-MAPE(test[,1], predictions_RF)*100

#Decision Tree Evaluation Metric
regr.eval(test[,1], predictions_RF,stats=c("mape","rmse"))

print(Accuracy3)

###Find the fare_amount of test data
#Predicting test set with Random Forest


predictions_RF <- predict(model_RF, fare_test[,1:12])

fare_test$fare_amount=predictions_RF
write.csv(fare_test, "Cabfare_PredictionR.csv", row.names = F)








