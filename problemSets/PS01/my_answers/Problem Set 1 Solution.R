#Problem Set 1 Jennie Moynihan
 
# Question 1: Find a 90% confidence interval for the average student IQ in the school.

# Load the data  
Student_IQ <- c(105 , 69 , 86 , 100 , 82 , 111 , 104 , 110 , 87 , 108 , 87 , 90 , 94 , 113 , 112 , 98 ,
        80 , 97 , 95 , 111 , 114 , 89 , 95 , 126 , 98)
sum(Student_IQ)

# I get the sample mean 
mean(Student_IQ) 

# I calculate the standard deviation to measure the variability in the sample
sd(Student_IQ)
#The SD is approximately 13.09

# Then I calculate the standard error of the sample mean

# I next to calculte the z-score for the 90% confidence interval

qnorm(1 - (1 - 0.90) / 2)

# Now I need to use these values to calculate the lower bound and the upper bound of the confidence interval. 

# Lower bound

lower_90 = (mean(Student_IQ)-(1.645)*(sd(Student_IQ)/sqrt(length((Student_IQ)))

# Answer = 94.13
                                
# Upper Bound

upper_90 = (mean(Student_IQ)+(1.645)*(sd(Student_IQ)/sqrt(length((Student_IQ)))
 
# Answer = 102.74

lower_90
mean(Student_IQ)
upper_90

#Based on the results of this analysis, we can say that we are 90% confident that the true population mean IQ lies between 94.132 and 102.7476

# Question 2: Next, the school counselor was curious whether the average student IQ in her school
#is higher than the average IQ score (100) among all the schools in the country.

# Using the same sample, conduct the appropriate hypothesis test with  = 0:05.

# To conduct the hypothesis test, I am going to follow five steps: 

# Step 1: Check assumptions re data 

# Student IQ is a continuous and numerical variable

# The sample size (n) is less than 30, we cannot involke CLT. 

# We assume a randomized sample 

#Step 2

# State hypotheses

# Set up the null (H0): The null hypothesis states that the average student IQ in the school is equal to or less than the county average (100).

# Alternative hypothesis (H1): The average student IQ is greater than the county average. 

# Step 3: Calculate the test statistic

# Given that there is a strong directional expectation here, we require a one-tail test.  

t.test (Student_IQ, mu=100, alternative = "greater")

# The Results of the One Sample t-test

#data:  Student_IQ
#t = -0.59574, df = 24, p-value = 0.7215
# hypothesis: true mean is greater than 100
#95 percent confidence interval:
#93.95993      Inf
#sample estimates:
#mean of x 
#98.44 
#data:  Student_IQ

#Step 5: Interpret the results 

# Based on these results, I think we can conclude that the p-value is 0.7215, which is greater than the common significance level of 0.05. This means we fail to reject the null hypothesis.
# There is no evidence to suggest that the true mean IQ of the population is less than 100, based on this sample.We therefore cannot reject the null meaning that the sample mean is unlikelty to be higher than that of the true population. 

# PART 2

# Load the data

Expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# I familiarise myself with the data 

View(Expenditure)
head(Expenditure)
summary(Expenditure)

# I make the plots. 

pdf("Y_X1PDF.pdf")
plot(y=Expenditure$Y,x=Expenditure$X1,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Per capita personal income in state",
      main="Money Communities Spend on Addressing Homelessness")
dev.off()

pdf("Y_X2PDF.pdf")
plot(y=Expenditure$Y,x=Expenditure$X2,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Number of residents per 100,000 that are Financially Insecure",
     main="Money Communities Spend on Addressing Homelessness")
dev.off()

pdf("Y_X3PDF.pdf")
plot(y=Expenditure$Y,x=Expenditure$X3,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Number of people per thousand residing in urban areas in state",
     main="Money Communities Spend on Addressing Homelessness")
dev.off()

# The wasn't happy with my few attempts at the following plot as it didn't clearly tell us anything (and it was hard to see the average). So I amended the command to make it a box plot.  

pdf("Y_RegionPDF.pdf")
plot(y=Expenditure$Y,x=Expenditure$Region,
     ylab="Per capita expenditure on shelters/housing assistance",
     xlab="Region",
     main="Money Communities Spend on Addressing Homelessness")
axis(1, at = c(1, 2, 3, 4), labels = c("Northeast", "North Central", "South", "West"))
dev.off()

pdf("Y_X1RegionPDF.pdf")
plot(y=Expenditure$Y,x=Expenditure$X1,
     col=Expenditure$Region,
     pch=16,
     ylab="Expenditure on shelters/housing assistance in state (PC)",
     xlab="Personal income in state (PC)",
     main="Money Communities Spend on Addressing Homelessness by State")
dev.off()


pdf("X1_X2PDF.pdf")
plot(y=Expenditure$X1,x=Expenditure$X2,
     col=Expenditure$Region,
     pch=16,
     ylab="Per capita personal income in state",
     xlab="Number of residents per 100,000 that are financially insecure",
     main="Financial Insecurity and Personal Income")
legend("topright",                              
       legend = c("Northeast", "North Central", "South", "West"), 
       col = 1:4,                                
       pch = 16)
dev.off()

# For these plots it made sense to include a key. 

pdf("X1_X3PDF.pdf")
plot(y=Expenditure$X1,x=Expenditure$X3,
     col=Expenditure$Region,
     pch=16,
     ylab="Per capita personal income in state",
     xlab="Number of people per thousand residing in urban areas in state",
     main="Urban Population and Personal Income")
legend("topright",                              
       legend = c("Northeast", "North Central", "South", "West"), 
       col = 1:4,                                
       pch = 16)
dev.off()

pdf("X2_X3PDF.pdf")
plot(y=Expenditure$X2,x=Expenditure$X3,
     col=Expenditure$Region,
     pch=16,
     ylab="Number of residents per 100,000 that are financially insecure",
     xlab="Number of people per thousand residing in urban areas",
     main="Urban Population and Personal Income")
legend("topright",                              
       legend = c("Northeast", "North Central", "South", "West"), 
       col = 1:4,                                
       pch = 16)
dev.off()

# For the final graph, I defined the shapes and colours for each region. 

shapes <- c(16, 17, 18, 19)  

colors <- c("green", "black", "red", "blue")

# I then created the final scatterplot using these different shapes and colors with a key in the left. 

pdf("Final_PlotPDF.pdf")
plot(y = Expenditure$Y, x = Expenditure$X1,
     col = colors[Expenditure$Region],     
     pch = shapes[Expenditure$Region],     
     ylab = "Expenditure on shelters/housing assistance in state (PC)",
     xlab = "Personal income in state (PC)",
     main = "The Amount of Money Communities Spend on Addressing Homelessness by State")

legend("topleft", 
       legend = c("Northeast", "North Central", "South", "West"),
       col = colors,      
       pch = shapes)      

# I also added the state names as labels next to each observation for clarify
text(x = Expenditure$X1, y = Expenditure$Y, 
     labels = Expenditure$STATE,     
     pos = 4,                        
     cex = 0.7) 
dev.off()


