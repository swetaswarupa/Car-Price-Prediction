if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(gglot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(randomForest)
library(ggplot2)
library(corrplot)
library(dplyr)

#Read the data file
car<- read.csv("https://raw.githubusercontent.com/swetaswarupa/Car-Price-Prediction/main/Car%20details%20v3.csv", header = TRUE)
str(car)

#Describing Variables
#name - Name of the cars
#year - Year of the car when it was bought
#selling_price - Price at which the car is being sold
#km_driven - Number of Kilometers the car is driven
#fuel - Fuel type of car (petrol / diesel / CNG / LPG / electric)
#seller_type - Tells if a Seller is Individual or a Dealer
#transmission - Gear transmission of the car (Automatic/Manual)
#Owner - Number of previous owners of the car.

#There are 8128 rows and 13 variables. Our target variable is the selling_price, which signifies the price of the car. We will use other variable except the Car Name.

#Removing Torque variable

car <- car %>% select(-c(torque))

#Exploring the dataset

head(car, n=15)

#Extracting brand name from car name

car$name <- sapply(strsplit(car$name, " "), `[`, 1)

ggplot(data = car, aes(x=name, fill = name)) +
  geom_bar() + labs(x='Car Brand') + labs(title = "Bar Graph of Car Brand") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

car$name <- str_replace(car$name, 'Maruti', '0')
car$name <- str_replace(car$name, 'Skoda', '1')
car$name <- str_replace(car$name, 'Honda', '2')
car$name <- str_replace(car$name, 'Hyundai', '3')
car$name <- str_replace(car$name, 'Toyota', '4')
car$name <- str_replace(car$name, 'Ford', '5')
car$name <- str_replace(car$name, 'Renault', '6')
car$name <- str_replace(car$name, 'Mahindra', '7')
car$name <- str_replace(car$name, 'Tata', '8')
car$name <- str_replace(car$name, 'Chevrolet', '9')
car$name <- str_replace(car$name, 'Fiat', '10')
car$name <- str_replace(car$name, 'Datsun', '11')
car$name <- str_replace(car$name, 'Jeep', '12')
car$name <- str_replace(car$name, 'Mercedes-Benz', '13')
car$name <- str_replace(car$name, 'Mitsubishi', '14')
car$name <- str_replace(car$name, 'Audi', '15')
car$name <- str_replace(car$name, 'Volkswagen', '16')
car$name <- str_replace(car$name, 'BMW', '17')
car$name <- str_replace(car$name, 'Nissan', '18')
car$name <- str_replace(car$name, 'Lexus', '19')
car$name <- str_replace(car$name, 'Jaguar', '20')
car$name <- str_replace(car$name, 'Land', '21')
car$name <- str_replace(car$name, 'MG', '22')
car$name <- str_replace(car$name, 'Volvo', '23')
car$name <- str_replace(car$name, 'Daewoo', '24')
car$name <- str_replace(car$name, 'Kia', '25')
car$name <- str_replace(car$name, 'Force', '26')
car$name <- str_replace(car$name, 'Ambassador', '27')
car$name <- str_replace(car$name, 'Ashok', '28')
car$name <- str_replace(car$name, 'Isuzu', '29')
car$name <- str_replace(car$name, 'Opel', '30')
car$name <- str_replace(car$name, 'Peugeot', '31')

car$name <- as.numeric(car$name)
table(car$name)

#There are some blank values for columns mileage, engine, max_power, torque
#Substituting blank with NA

car$mileage[car$mileage == ""] <- NA
car$engine[car$engine == ""] <- NA
car$max_power[car$max_power == ""] <- NA

# Checking for missing values
sapply(car, function(x) sum(is.na(x)))

#Data Transformation

#Removing unit from mileage, converting it to numeric value and replacing the missing values
car$mileage <- str_replace(car$mileage, 'kmpl', '')
car$mileage <- str_replace(car$mileage, 'km/kg', '')
car$mileage <- as.numeric(car$mileage)
car$mileage[is.na(car$mileage)]<-mean(car$mileage,na.rm=TRUE)

#Removing unit from engine, converting it to numeric value and replacing the missing values

car$engine <- str_replace(car$engine, 'CC', '')
car$engine <- as.numeric(car$engine)
car$engine[is.na(car$engine)]<-mean(car$engine,na.rm=TRUE)

#Removing unit from max_power, converting it to numeric value and replacing the missing values

car$max_power <- str_replace(car$max_power, 'bhp', '')
car$max_power <- as.numeric(car$max_power)
car$max_power[is.na(car$max_power)]<-mean(car$max_power,na.rm=TRUE)

#Converting seats to numeric value and replacing the missing values
car$seats <- as.numeric(car$seats)
car$seats[is.na(car$seats)]<-median(car$seats,na.rm=TRUE)

# Checking for missing values once again
sapply(car, function(x) sum(is.na(x)))


#Categorical Values
ggplot(data = car, aes(x=reorder(fuel, fuel, function(x)-length(x)), fill = fuel)) +
  geom_bar() + labs(x='Fuel') + labs(title = "Bar Graph of Fuel") 

ggplot(data = car, aes(x=reorder(seller_type, seller_type, function(x)-length(x)), fill = seller_type)) +
  geom_bar() + labs(x='Seller Type') + labs(title = "Bar Graph of Seller Type")

ggplot(data = car, aes(x=reorder(owner, owner, function(x)-length(x)), fill = owner)) +
  geom_bar() + labs(x='Owner') + labs(title = "Bar Graph of Owner") 

ggplot(data = car, aes(x=reorder(seats, seats, function(x)-length(x)), fill = seats)) +
  geom_bar() + labs(x='Seats') + labs(title = "Bar Graph of Seats") 

#Converting transmission column into binary 0 if Manual and 1 if Automatic
car$transmission <- str_replace(car$transmission, 'Manual', "0")
car$transmission <- str_replace(car$transmission, 'Automatic', "1")
car$transmission <- as.numeric(car$transmission)
table(car$transmission)

#Converting owner into Ordinal Encoder
car$owner <- str_replace(car$owner, 'First Owner', "0")
car$owner <- str_replace(car$owner, 'Second Owner', "1")
car$owner <- str_replace(car$owner, 'Third Owner', "2")
car$owner <- str_replace(car$owner, 'Fourth & Above Owner', "3")
car$owner <- str_replace(car$owner, 'Test Drive Car', "4")
car$owner <- as.numeric(car$owner)
table(car$owner)

#Converting seller_type into Ordinal Encoder
car$seller_type <- str_replace(car$seller_type, "Trustmark Dealer", "0")
car$seller_type <- str_replace(car$seller_type, "Dealer", "1")
car$seller_type <- str_replace(car$seller_type, "Individual", "2")
car$seller_type <- as.numeric(car$seller_type)
table(car$seller_type)

#Converting fuel into Ordinal Encoder
car$fuel <- str_replace(car$fuel, 'Diesel', "0")
car$fuel <- str_replace(car$fuel, 'Petrol', "1")
car$fuel <- str_replace(car$fuel, 'CNG', "2")
car$fuel <- str_replace(car$fuel, 'LPG', "3")
car$fuel <- as.numeric(car$fuel)
table(car$fuel)

#Histogram of Selling Price
ggplot(car, aes(x=selling_price)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="blue")+
  labs(x='Selling Price ') + labs(title = "Histogram Graph of Selling Price") +
  scale_x_continuous(trans='log10')

#Histogram of Km Driven
ggplot(car, aes(x=km_driven)) + 
  geom_histogram(color="black", fill="blue", bins = 200)+
  labs(x='Km Driven ') + labs(title = "Histogram Graph of Km Driven") +
  scale_x_continuous(trans='log10')


#Checking correlation between variables

library(corrplot)
corrplot(cor(car), type="full", 
         method ="color", title = "car correlatoin plot", 
         mar=c(0,0,1,0), tl.cex= 0.8, outline= T, tl.col="indianred4")
round(cor(car),2)


# Splitting the Data into training and test data sets
library(caret)
set.seed(5)
trainIndex <- createDataPartition(car$selling_price, p = .7,
                                  list = FALSE,
                                  times = 1)
Train <- car[ trainIndex,]
Test <- car[-trainIndex,]

#Model 1 Linear Regression

set.seed(123)
m1_lr <- lm(selling_price ~ name+year+km_driven+seller_type+mileage+transmission+max_power, data = Train)
summary(m1_lr)
#plot(m1_lr)

pred_lr <- predict(m1_lr, newdata = Test)
error_lr <- Test$selling_price - pred_lr
RMSE_lr <- sqrt(mean(error_lr^2))
RMSE_lr

#plot predicted vs. actual values
plot(Test$selling_price,pred_lr, main="Scatterplot", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")



#Model 2 Random Forest
library(randomForest)

m2_rf <- randomForest(selling_price~.,data = Train)
m2_rf
plot(m2_rf)


varImpPlot(m2_rf, main ='Feature Importance')

pred_rf <- predict(m2_rf, Test)

error_rf <- Test$selling_price - pred_rf
RMSE_rf <- sqrt(mean(error_rf^2))
RMSE_rf

plot(Test$selling_price,pred_rf, main="Scatterplot", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")

#Model 3 Gradient Boosting 
library(gbm)
set.seed(123)
m3_gbm <- gbm(
  formula = selling_price ~ .,
  distribution = "gaussian",
  data = Train,
  n.trees = 6000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

m3_gbm

summary(
  m3_gbm, 
  cBars = 10,
  method = relative.influence, las = 2
)

# plot loss function as a result of n trees added to the ensemble
gbm.perf(m3_gbm, method = "cv")

pred_gbm <- predict(m3_gbm, Test)

error_gbm <- Test$selling_price - pred_gbm
RMSE_gbm <- sqrt(mean(error_gbm^2))
RMSE_gbm

plot(Test$selling_price,pred_gbm, main="Scatterplot", col = c("red","blue"), xlab = "Actual Selling Price", ylab = "Predicted Selling Price")