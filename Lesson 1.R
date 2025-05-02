install.packages("remotes")
library(remotes)
remotes::install_github("cran/fOptions")
remotes::install_github("cran/fAsianOptions")
remotes::install_github("cran/probs")

#install.packages("distrEx")


library(prob)


##Sample Spaces
tosscoin(1)
tosscoin(3)


rolldie(1)
rolldie(3)



#Sampling on urns
#E.g Let your urn simply contain three balls, labelled 1,2, and 3 repectively.
#We are goin to take sample size of 2 from the urn.

urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE) #Selection with replacement
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE) #without replacement


urnsamples(c("red","yellow","green"),size = 2, replace = TRUE, ordered = TRUE) #Selection with replacement using color
urnsamples(c("red","yellow","green"),size = 2, replace = FALSE, ordered = TRUE) #without replacement using 


##Events
S <- tosscoin(2, makespace = TRUE)
S[1:3,]
S[c(2, 4), ]


A <- cards()
subset(A, suit == "Heart")
subset(A, rank %in% 7:9)

S=rolldie(3)
subset(rolldie(3), X1 + X2 + X3 > 16)


#Set Union, Intersection, and Difference
S=cards()
A=subset(S, Suit == "Hearts")
B=subset(S, rank %in% 7:9)

union(A, B)
intersect(A, B)
setdiff(A, B)
setdiff(B, A)

##Calculating probabilities
#Probabilities are calculated in the prob package with the 'Prob' function.

S <- cards(makespace = TRUE)
A <- subset(S, suit == "Heart")
B <-subset(S, rank %in% 7:9)

Prob(A)
Prob(union(A,B))
Prob(intersect(A,B))
Prob(setdiff(A,B))
Prob(setdiff(B,A))

library(probs)
S <- rolldie(2, makespace = TRUE)
A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)

Prob(A, given = B)
Prob(B, given = A)


install.packages("remotes")
remotes::install_github("cran/fOptions") 
remotes::install_github("cran/fAsianOptions") 
remotes::install_github("cran/prob")

#install.packages("distrEx")

library(prob)

##Sample Spaces
tosscoin(1)
tosscoin(3)

rolldie(1)
rolldie(3)

cards()

#Sampling from Urns
#E.g. Let our urn simply contain three balls, labeled 1, 2, and 3, respectively. 
#We are going to take a sample of size 2 from the urn.

urnsamples(1:3, size = 2, replace = TRUE, ordered = TRUE) #Selection with replacement
urnsamples(1:3, size = 2, replace = FALSE, ordered = TRUE) #Without replacement


##Events
S <- tosscoin(2, makespace = TRUE)
S[1:3, ]
S[c(2, 4), ]

S <- cards()
subset(S, suit == "Heart")
subset(S, rank %in% 7:9)

S = rolldie(3)
subset(rolldie(3), X1 + X2 + X3 > 16)


##Set Union, Intersection, and Difference
S = cards()
A = subset(S, suit == "Heart")
B = subset(S, rank %in% 7:9)

union(A, B)
intersect(A, B)
setdiff(A, B)
setdiff(B, A)

##Calculating probabilities
#Probabilities are calculated in the prob package with the 'Prob' function.

S <- cards(makespace = TRUE)
A <- subset(S, suit == "Heart")
B <- subset(S, rank %in% 7:9)

Prob(A)
Prob(B)
Prob(union(A, B))
Prob(intersect(A, B))
Prob(setdiff(A, B))
Prob(setdiff(B, A))


##Conditional probability
#E.g. Toss a six-sided die twice. Let A = The outcomes match and 
#B = The sum of outcomes is at least 8. Find P(A/B) and P(B/A)

library(prob)
S <- rolldie(2, makespace = TRUE)
A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)

Prob(A, given = B)
Prob(B, given = A)

### Descriptive Statistics
#Opening inbuilt dataset in R
data("UKDriverDeaths")
head(UKDriverDeaths)
tail(UKDriverDeaths)
str(UKDriverDeaths)

#Measures of central tendency
mean(UKDriverDeaths)
mean(UKDriverDeaths, trim=.05) #5% trimmed mean
median(UKDriverDeaths)
mean(range(UKDriverDeaths)) #Midrange
#install.packages("DescTools")
library(DescTools)
Mode(UKDriverDeaths)

#Measures of variation
range(UKDriverDeaths)
var(UKDriverDeaths)
sd(UKDriverDeaths)
sd(UKDriverDeaths)/mean(UKDriverDeaths)*100 #Coefficient of variation

# Measures of shape
library(e1071)
skewness(UKDriverDeaths)
kurtosis(UKDriverDeaths)

#A single command for descriptive statistics
library("psych")
describe(UKDriverDeaths)

## Exploratory data analysis
# Stem-and-leaf-plot
library(aplpack)
stem.leaf(UKDriverDeaths, depth = FALSE)
#Boxplots
boxplot(UKDriverDeaths)

##Hypotheses testing
library(TeachingDemos)
#Large sample mean test
z.test(43260, n=30, mu = 42000, sd = 5230, conf.level = 0.95,alternative = "greater")
z.test(27, n=30, mu = 29.4, sd = 2, conf.level = 0.99,alternative = "less")
z.test(25226, n=35, mu = 24672, sd = 3251, conf.level = 0.99,alternative = "two.sided")

#Small sample mean test
t.test(c(22000,23000,24000,21000,22000,22000,23000,24000,21000,22000), n=10, 
       mu = 24000, sd = 400, conf.level = 0.95,alternative = "two.sided")

## Correlation and Regression Analysis
# The speed in the 'cars' dataset represents how fast the car was going (x) 
# in miles per hour and dist (Y) measures how far it took the car to stop, in feet.
data(cars)
head(cars)

#Scatter plot
plot(dist ~ speed, data = cars)

#Correlation
cor(cars$dist, cars$speed)
cor.test(cars$dist, cars$speed) #Test to generalized from sample to population
cor(cars) #Corelation matrix, mainly used for multiple variables

#Simple Linear Regression
cars.lm <- lm(dist ~ speed, data = cars)
coef(cars.lm)
summary(cars.lm)
#fitted(cars.lm)
predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))
confint(cars.lm) #Are the results only applicable to the sample? Or they can be generalized?
plot(cars.lm) #Testing model assumptions

#Multiple Linear Regression
#Measurements were made of the girth, height, and volume of timber in 31 
#felled black cherry trees.
data(trees)
head(trees)
tail(trees)
library(lattice)
splom(trees)
cor(trees)
library(psych)
corr.test(trees)
fit <- lm(Volume ~ Girth + Height, data = trees)
summary(fit)
confint(fit)
#Prediction
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69,74, 87))
predict(fit, newdata = new)



#MID-SEMESTER EXAMINATION

# Install packages(tidyverse,car,ggplot2)
install.packages("tidyverse")
install.packages("car")        # for VIF/multicollinearity
install.packages("ggplot2")    # for plots

# Load the libraries installed
library(tidyverse)
library(car)
library(ggplot2)

#Manual input of data
df <- data.frame(
  Project = 1:20,
  Number_of_Features_Used = c(25,15,32,10,20,28,12,35,11,22,27,13,30,9,18,29,21,34,8,19),
  Lines_of_Code = c(1500,1000,1800,800,1300,1600,950,1900,870,1450,1550,920,1750,780,1200,1650,1400,1850,700,1250),
  Training_Data_Size_MB = c(120,80,150,70,100,140,75,160,68,110,130,72,145,65,95,138,105,155,60,90),
  Model_Training_Time_min = c(45,30,60,25,40,55,28,65,26,42,50,27,58,23,36,54,39,63,20,34),
  Final_Model_Accuracy = c(87,78,91,72,82,89,74,93,70,85,88,76,90,68,80,87,84,92,65,79)
)
head(df)    # view first 6 rows
View(df)    # open spreadsheet view

#Fitting of the linear Regression Model
model <- lm(Final_Model_Accuracy ~ Number_of_Features_Used + Lines_of_Code + Training_Data_Size_MB + Model_Training_Time_min, data = df)

# View the model summary
summary(model)

# Create a residual plot (fitted values vs residuals)
plot(model$fitted.values, model$residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot")

# Add a horizontal line at 0 (mean of residuals)
abline(h = 0, col = "red", lty = 2)

#Checking Multicollinearity
vif(model)

#Confidence Intervals and Prediction
# Get 95% confidence intervals
confint(model)

# Predict effect of a 10MB increase in training data size
coef(model)["Training_Data_Size_MB"] * 10
