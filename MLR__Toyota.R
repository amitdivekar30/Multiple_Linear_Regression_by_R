# Consider only the below columns and prepare a prediction model for predicting Price.
# Corolla<-Corolla[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
# Model -- model of the car
# Price  -- Offer Price in EUROs	
# Age_08_04 -- Age in months as in August 2004	
# Mfg_Month -- Manufacturing month (1-12)	
# Mfg_Year	-- Manufacturing Year
# KM -- Accumulated Kilometers on odometer
# Fuel_Type	 -- Fuel Type (Petrol, Diesel, CNG)
# HP -- Horse Power
# Met_Color	 -- Metallic Color?  (Yes=1, No=0)
# Color -- Color (Blue, Red, Grey, Silver, Black, etc.)
# Automatic	-- Automatic ( (Yes=1, No=0)
#                          cc -- Cylinder Volume in cubic centimeters
#                          Doors -- Number of doors
#                          Cylinders	-- Number of cylinders
#                          Gears -- Number of gear positions
#                          Quarterly_Tax -- Quarterly road tax in EUROs
#                          Weight -- Weight in Kilograms
#                          Mfr_Guarantee -- Within Manufacturer's Guarantee period  (Yes=1, No=0)
# BOVAG_Guarantee -- BOVAG (Dutch dealer network) Guarantee  (Yes=1, No=0)
# Guarantee_Period -- 	Guarantee period in months
# ABS -- Anti-Lock Brake System (Yes=1, No=0)
# Airbag_1 -- Driver_Airbag  (Yes=1, No=0)
# Airbag_2 -- Passenger Airbag  (Yes=1, No=0)
# Airco -- Airconditioning  (Yes=1, No=0)
# Automatic_airco -- Automatic Airconditioning  (Yes=1, No=0)
# Boardcomputer -- Boardcomputer  (Yes=1, No=0)
# CD_Player -- CD Player  (Yes=1, No=0)
# Central_Lock -- Central Lock  (Yes=1, No=0)
# Powered_Windows -- Powered Windows  (Yes=1, No=0)
# Power_Steering -- Power Steering  (Yes=1, No=0)
# Radio -- Radio  (Yes=1, No=0)
# Mistlamps	-- Mistlamps  (Yes=1, No=0)
# Sport_Model -- Sport Model  (Yes=1, No=0)
# Backseat_Divider -- Backseat Divider  (Yes=1, No=0)
# Metallic_Rim --Metallic Rim  (Yes=1, No=0)
# Radio_cassette -- Radio Cassette  (Yes=1, No=0)
# Tow_Bar -- Tow Bar  (Yes=1, No=0)


dataset1<-read.csv(file.choose())
View(dataset1) #38 features
summary(dataset1)
str(dataset1)
dataset<-dataset1[,-c(1,2,5,6)]
dataset<-dataset[,c(1:3,5,9,10,12:14)]
View(dataset)
# display first 10 rows of dataset
head(dataset, n=10)
# display the dimensions of the dataset
dim(dataset)
summary(dataset)
str(dataset)
attach(dataset)


# list types for each attribute
sapply(dataset, class)

# standard deviations and mean for each class
y<-sapply(dataset[,c(1:9)], mean)
sapply(dataset[,c(1:9)], sd)
xn<-colnames(dataset[,c(1:9)])
x<-c(1:9)
y<-sapply(dataset[,c(1:9)], mean)
barplot(y, main = "Average Value For Each Feature",
        xlab = "Feature Name",
        ylab = "Average Value")

barplot(sapply(dataset[,c(1:9)], sd), main = "standard Deviation For Each Feature",
        xlab = "Feature Name",
        ylab = "Standard Deviation Value")

#Exploratory Data Analysis

#graphical representation
boxplot(Age_08_04,horizontal = T)
boxplot(KM, horizontal= T)
boxplot(HP,horizontal = T)
boxplot(Price, horizontal= T)
hist(Age_08_04)

hist(KM)
hist(HP)
hist(Price)

qqnorm(Price)
qqline(Price)


plot(Age_08_04,Price) # Plot relation ships between each X with Y
plot(KM,Price)
## Or make a combined plot
pairs(dataset)   # Scatter plot for all pairs of variables
# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(dataset[,c(1:9)])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")

###                                   Partial Correlation matrix
#install.packages("corpcor")
library(corpcor)
cor2pcor(cor(dataset))

# Fitting Multiple Linear Regression to the Training set
regressor <- lm(formula = Price ~ .,
                data = dataset)
summary(regressor) #Adjusted R-squared:  0.863 


# Diagnostic Plots
#install.packages(car)
library(car)
plot(regressor)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance
qqPlot(regressor, id.n=5) # QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(regressor)
influenceIndexPlot(regressor, id.n=3) # Index Plots of the influence measures
influencePlot(regressor, id.n=3) # A user friendly representation of the above

## Regression after deleting the influential observations
regressor1<-lm(Price~., data=dataset[-c(81,961,222), ])
summary(regressor1) #Adjusted R-squared:  0.8845 
influencePlot(regressor1, id.n=3)

### Variance Inflation Factors
vif(regressor)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(regressor, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(regressor)
stepAIC(regressor1) # backward
# all variables are important

#eliminating insignificant variables
regressor2<- lm(formula = Price ~ Age_08_04 + KM + HP + Gears + Quarterly_Tax + 
     Weight, data = dataset)
summary(regressor2) #Adjusted R-squared:  0.863


#RMSE

RMSE_regressor<-sqrt(mean(regressor$residuals^2))
RMSE_regressor

RMSE_regressor1<-sqrt(mean(regressor1$residuals^2))
RMSE_regressor1

RMSE_regressor2<-sqrt(mean(regressor2$residuals^2))
RMSE_regressor2

RMSE<-as.data.frame(cbind(RMSE_regressor,RMSE_regressor1, RMSE_regressor2))
table(RMSE_regressor,RMSE_regressor1, RMSE_regressor2)


#note model with removal of influentail data has highest r squared has least RMSE


model.final <- lm(Price~., data=dataset[-c(81,961,222), ])
summary(model.final)

avPlots(model.final, id.n=2, id.cex=0.8, col="red")
vif(model.final)

