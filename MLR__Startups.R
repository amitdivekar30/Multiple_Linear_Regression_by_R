# Prepare a prediction model for profit of 50_startups data.
# Do transformations for getting better predictions of profit and
# make a table containing R^2 value for each prepared model.

# R&D Spend -- Research and devolop spend in the past few years
# Administration -- spend on administration in the past few years
# Marketing Spend -- spend on Marketing in the past few years
# State -- states from which data is collected
# Profit  -- profit of each state in the past few years

dataset<-read.csv(file.choose()) #50_startups.csv
View(dataset)
# display first 10 rows of dataset
head(dataset, n=10)
# display the dimensions of the dataset
dim(dataset)
summary(dataset)
attach(dataset)

# list types for each attribute
sapply(dataset, class)

# standard deviations and mean for each class
y<-sapply(dataset[,c(1:3,5)], mean)
sapply(dataset[,c(1:3,5)], sd)
xn<-colnames(dataset[,c(1:3,5)])
x<-c(1:3,5)
y<-sapply(dataset[,c(1:3,5)], mean)
barplot(y, main = "Average Value For Each Feature",
        xlab = "Feature Name",
        ylab = "Average Value")

barplot(sapply(dataset[,c(1:3,5)], sd), main = "standard Deviation For Each Feature",
        xlab = "Feature Name",
        ylab = "Standard Deviation Value")


#Exploratory Data Analysis
summary(dataset)

#graphical representation
boxplot(R.D.Spend,horizontal = T)
boxplot(Administration, horizontal= T)
boxplot(Marketing.Spend,horizontal = T)
boxplot(Profit, horizontal= T)
hist(R.D.Spend)
hist(Administration)
hist(Marketing.Spend)
hist(Profit)

pairs(dataset[-4])   # Scatter plot for all pairs of variables except state feature

# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(dataset[,c(1:3,5)])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")

#Encoding Categorical Data
dataset$State<-factor(dataset$State,
                      levels = c('New York','California','Florida'),
                      labels = c(1, 2, 3))

# Fitting Multiple Linear Regression to the Training set
regressor <- lm(formula = Profit ~ .,
               data = dataset)
summary(regressor)


# Deletion Diagnostics for identifying influential variable
library(car)
influence.measures(regressor)
influenceIndexPlot(regressor, id.n=3) # Index Plots of the influence measures
influencePlot(regressor, id.n=3) # A user friendly representation of the above

### Variance Inflation Factors
vif(regressor)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(regressor, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(regressor)


## Regression after deleting the 50th observation
regressor1<-lm(Profit~., data= dataset[-50, ])
summary(regressor1)
influencePlot(regressor1) # A user friendly representation of the above
# Variance Inflation Factors
vif(regressor1)  # VIF is > 10 => collinearity

# #### Added Variable Plots ######
avPlots(regressor1, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(regressor1) # backward

plot(regressor1)
 
# Regression after ignoring State feature
regressor2<-lm(Profit~R.D.Spend+Administration+Marketing.Spend, 
               data=dataset[-50,])

summary(regressor2)
vif(regressor2)
avPlots(regressor2, id.n=2, id.cex=0.8, col="red")
stepAIC(regressor2)

#RMSE

RMSE_regressor<-sqrt(mean(regressor$residuals^2))
RMSE_regressor

RMSE_regressor1<-sqrt(mean(regressor1$residuals^2))
RMSE_regressor1

RMSE_regressor2<-sqrt(mean(regressor2$residuals^2))
RMSE_regressor2

RMSE<-as.data.frame(cbind(RMSE_regressor,RMSE_regressor1, RMSE_regressor2))
table(RMSE_regressor,RMSE_regressor1, RMSE_regressor2)


# Regression after ignoring State and Administration feature
final_model<-lm(Profit~R.D.Spend+Marketing.Spend, 
               data=daatset[-50,])

summary(final_model)
vif(final_model)
avPlots(final_model, id.n=2, id.cex=0.8, col="red")
stepAIC(final_model)

