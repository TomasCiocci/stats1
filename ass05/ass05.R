setwd("/Users/misha/git/stats1/ass05")
library(psych)
library(ggplot2)

salary <- read.table("Stats1.13.HW.04.txt", header = T)
describe(salary)
model1 <- lm(salary$salary ~ salary$years)
summary(model1)

# Print 95% confidence interval for the regression coefficient
confint(model1)

model2 <- lm(salary$salary ~ salary$courses)
summary(model2)

# Print 95% confidence interval for the regression coefficient
confint(model2)

model3 <- lm(salary$salary ~ salary$years + salary$courses)
summary(model3)

# Print 95% confidence interval for the regression coefficient
confint(model3)

#
# Scaled
#
model1 <- lm(scale(salary$salary) ~ scale(salary$years))
summary(model1)

# Print 95% confidence interval for the regression coefficient
confint(model1)

model2 <- lm(scale(salary$salary) ~ scale(salary$courses))
summary(model2)

# Print 95% confidence interval for the regression coefficient
confint(model2)

model3 <- lm(scale(salary$salary) ~ scale(salary$years) + scale(salary$courses))
summary(model3)

# Print 95% confidence interval for the regression coefficient
confint(model3)

#
# Sampled
#
salary.15 <- salary[sample(nrow(salary), 15), ]
cor.test(salary$salary, salary$years)
cor.test(salary.15$salary, salary.15$years)

subsalary = salary[51:70,]
submodel <- lm(subsalary$salary ~ subsalary$years + subsalary$courses)
summary(submodel)

summary(lm(subsalary$salary ~ subsalary$years))
summary(lm(subsalary$salary ~ subsalary$courses))
summary(lm(subsalary$salary ~ subsalary$years + subsalary$courses))

submodel_multi <- lm(subsalary$salary ~ subsalary$years + subsalary$courses)
subsalary$predicted <- fitted(submodel_multi)
cor.test(subsalary$salary, subsalary$predicted)

subsalary$residuals <- resid(submodel_multi)
cor.test(subsalary$salary, subsalary$residuals)