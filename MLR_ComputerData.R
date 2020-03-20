# Predict Price of the computer
# A dataframe containing :
#   price : price in US dollars of 486 PCs
# speed : clock speed in MHz
# hd : size of hard drive in MB
# ram : size of Ram in in MB
# screen : size of screen in inches
# cd : is a CD-ROM present ?
#   multi : is a multimedia kit (speakers, sound card) included ?
#   premium : is the manufacturer was a "premium" firm (IBM, COMPAQ) ?
#   ads : number of 486 price listings for each month
# trend : time trend indicating month starting from January of 1993 to November of 1995.

Computer_data <- read.csv (file.choose())
View(Computer_data)
Computer_data<-Computer_data[-1] # removing first column
# display first 10 rows of Computer_data
head(Computer_data, n=10)
# display the dimensions of the Computer_data
dim(Computer_data)
summary(Computer_data)
attach(Computer_data)
str(Computer_data)
# list types for each attribute
sapply(Computer_data, class)

# standard deviations and mean for each class
y<-sapply(Computer_data[,c(1:5,9,10)], mean)
sapply(Computer_data[,c(1:5,9,10)], sd)
xn<-colnames(Computer_data[,c(1:5,9,10)])
x<-c(1:5,9,10)
y<-sapply(Computer_data[,c(1:5,9,10)], mean)
barplot(y, main = "Average Value For Each Feature",
        xlab = "Feature Name",
        ylab = "Average Value")

barplot(sapply(Computer_data[,c(1:5,9,10)], sd), main = "standard Deviation For Each Feature",
        xlab = "Feature Name",
        ylab = "Standard Deviation Value")



qqnorm(price)
qqline(price)

plot(speed,price) # Plot relation ships between each X with Y
plot(hd,price)
## Or make a combined plot
pairs(Computer_data)   # Scatter plot for all pairs of variables

# calculate a correlation matrix for numeric variables
library(corrplot)
correlations <- cor(Computer_data[,c(1:5,9,10)])
# display the correlation matrix
print(correlations)
corrplot(correlations, method = "circle")


#Encoding Categorical Variable
Computer_data$cd<-factor(Computer_data$cd,levels = c("no","yes"),labels = c(0,1))
Computer_data$multi<-factor(Computer_data$multi,levels = c("no","yes"),labels = c(0,1))
Computer_data$premium<-factor(Computer_data$premium,levels = c("no","yes"),labels = c(0,1))
# The prediction model
regressor <- lm(price~., data=Computer_data) # lm(Y ~ X)
summary(regressor)


#######                    Scatter plot matrix with Correlations inserted in graph
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r = (cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.4/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex)
}
pairs(Computer_data, upper.panel=panel.cor,main="Scatter Plot Matrix with Correlation Coefficients")

###                                   Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(Computer_data[,-c(6:8)])

cor2pcor(cor(Computer_data[,-c(6:8)]))

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
regressor1<-lm(price~., data=Computer_data[-c(1441,1701,3784,4478), ])
summary(regressor1)
influencePlot(regressor1, id.n=3)

### Variance Inflation Factors
vif(regressor)  # VIF is > 10 => collinearity

#### Added Variable Plots ######
avPlots(regressor, id.n=2, id.cex=0.8, col="red")

library("MASS")
stepAIC(regressor) # backward
# all variables are important

plot(regressor)

model.final <- lm(price~., data=Computer_data)
summary(model.final)


model.final1 <- lm(price~., data=Computer_data[-c(1441,1701,3784,4478),])
summary(model.final1)

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.