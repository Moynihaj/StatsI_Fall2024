install.packages("car")
library(car)
data(Prestige)
# Question 1 
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)

# Part 2 

model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)

install.packages("stargazer")
library(stargazer)
stargazer(model, type = "latex", title = "Linear Model Output", label = "tab:linear_model",
          covariate.labels = c("Income", "Professional", "Income:Professional"),
          dep.var.labels = "Prestige",
          style = "default")
# Part (f) 

#Extract coefficients from the model
coef_income <- coef(model)["income"]
coef_interaction <- coef(model)["income:professional"]
#Calculate the marginal effect of income when professional = 1
marginal_effect_professional <- coef_income + coef_interaction
View(marginal_effect_professional)
# Calculate the change in prestige for a $1,000 increase in income
change_in_prestige <- marginal_effect_professional * 1000
View(change_in_prestige)



# Given coefficients based on the answer for (c)
coef_professional <- 37.7813        
coef_interaction <- -0.0023         
# Calculate the marginal effect of changing from non-professional to professional with an income of = 6000
change_in_prestige_professional <- coef_professional + (coef_interaction * 6000)
# Display the result
change_in_prestige_professional

