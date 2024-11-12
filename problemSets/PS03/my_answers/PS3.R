# Question 1
# Part 1 - Load the Data 
> library(readr)
> incumbents_subset <- read_csv("~/Desktop/GitHub/StatsI_Fall2024/datasets/incumbents_subset.csv")
# View the Data
sum(incumbents_subset) 

# QUESTION 1

# Part 1: Run a regression where the outcome variable is vote share and the explanatory variable
# is difflog.
# Regression analysis:
# Fit the Model 
model <- lm(voteshare ~ difflog, data = incumbents_subset)
# View the summary of the model
summary(model)

install.packages("stargazer")
library(stargazer)
stargazer(model, type = "text", title = "Regression Results", digits = 3)
stargazer(model)

# Load ggplot2 package 
install.packages("ggplot2")
library(ggplot2)

#Part 2. Make a scatterplot of the two variables and add the regression
pdf("ScatterPlot1")
print(
  ggplot(incumbents_subset, aes(x = difflog, y = voteshare)) +
    geom_point() +                             
    geom_smooth(method = "lm", se = FALSE) +   
    labs(x = "Difference in Log Values (difflog)",
         y = "Vote Share",
         title = "Scatterplot of Vote Share vs. Difference in Log Values") +
    theme_minimal()                           
)
dev.off()

model <- lm(voteshare ~ difflog, data = incumbents_subset)

#Save the residuals in separate object
residuals_object <- resid(model)
print(residuals_object)
summary(residuals_object)
#I thought it might be useful for later questions to also place them in a plot. 
plot(residuals_object, main = "Residual Plot", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")

intercept <- coef(model)[1]
slope <- coef(model)[2]

cat("Prediction equation: voteshare =", round(intercept, 3), "+", round(slope, 3), "* difflog")

#Question 2

#1. 1. Run a regression where the outcome variable is presvote and the explanatory variable
#is difflog
model2 <- lm(presvote ~ difflog, data = incumbents_subset)
summary(model2)
stargazer(model2)
#2. 2. Make a scatterplot of the two variables and add the regression
pdf("ScatterPlot2.pdf")
print(
  ggplot(incumbents_subset, aes(x = difflog, y = presvote)) +
    geom_point() +                             
    geom_smooth(method = "lm", se = FALSE) +   
    labs(x = "Difference in Log Values (difflog)",
         y = "Presidential Vote",
         title = "Scatterplot of Presidential Vote vs. Difference in Log Values") +
    theme_minimal()                           
)
dev.off()
#Save the residuals in separate object
residuals_object2 <- resid(model2)
print(residuals_object)
summary(residuals_object)
# As previously, I thought it might be useful to view them in a plot
plot(residuals_object2, main = "Residual Plot 2", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red")


# Question 3

#1. Run a regression where the outcome variable is voteshare and the explanatory variable
#is presvote
model3 <- lm(voteshare ~ presvote, data = incumbents_subset)
summary(model3)
stargazer(model3)
#2. Make a scatterplot of the two variables and add the regression 
pdf("ScatterPlot3.pdf")
print(
  ggplot(incumbents_subset, aes(x = presvote, y = voteshare)) +
    geom_point() +                             
    geom_smooth(method = "lm", se = FALSE) +   
    labs(x = "Presidential Vote",
         y = "Vote Share",
         title = "Scatterplot of Presidential Vote vs. Vote Share") +
    theme_minimal()                           
)
dev.off()

#3. Write the prediction equation 

# Question 4
# Run a regression where the outcome variable is the residuals from Question 1 
#and the explanatory variable is the residuals from Question 2.

model4 <- lm(residuals_object ~ residuals_object2, data = incumbents_subset)
View(model4)
summary(model4)
stargazer(model4)

##2. Make a scatterplot of the two variables and add the regression line
pdf("ScatterPlot4.pdf")
print(
  ggplot(incumbents_subset, aes(x = residuals_object, y = residuals_object2)) +
    geom_point() +                             
    geom_smooth(method = "lm", se = FALSE) +   
    labs(x = "Residuals Q1",
         y = "Residuals Q2",
         title = "Scatterplot of Residuals") +
    theme_minimal()                           
)
dev.off()


# Question 5 
# Run the regression model
model5 <- lm(voteshare ~ difflog + presvote, data = incumbents_subset)
summary(model5)
stargazer(model5)


