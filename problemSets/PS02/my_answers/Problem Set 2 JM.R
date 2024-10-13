# Load data - I put the data into an Excel document and loaded into my Global Environment. 

# I look at the data

View(Table_Corruption_Experiment_)
summary(Table_Corruption_Experiment_)

# Question 1 (a): Chi-Square Test Statistic

# Step 1: Set up the Null
# As Above 

# Step 2: Construct Contingency Table 

observed <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE)

# Assign row and column names for clarity
rownames(observed) <- c("Upper Class", "Lower Class")
colnames(observed) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")

print("Observed Frequencies:")
observed

# Step 3: Calculate the Expected Frequencies 

Expect <- chi_square_test$expected

# Step 4: Compute the Chi-square Statistic

chi_square_test$statistic

# Step 5: Determine the Degrees of Freedom
# As Above

# Step 6: Conclusion: Compare Chi Square with Critical Value

chi_square_test$p.value

# View the standardized residuals
print("Standardized Residuals:")
print(chi_square_test$stdres) 
stand <- (observed - Expect) / sqrt(Expect)
print(stand)


#Question 2: Economics 

summary(Women)
data <- read.csv("Women.csv")

# Bivariate Regression  

# Save model as object
model <- lm(water ~ reserved, data = data)
install.packages("stargazer")
summary(model)
library(stargazer)
stargazer(model,type = "text") 
stargazer(model,type = "latex") 

# Check p-value
summary(model) 
 