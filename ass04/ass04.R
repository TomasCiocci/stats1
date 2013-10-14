library(psych)

setwd("/Users/misha/git/stats1/ass04")
salary <- read.table("Stats1.13.HW.04.txt")
edit(salary)

cor(salary$salary, salary$years)

model <- lm(salary$salary ~ salary$years)
summary(model)

model2 <- lm(salary$salary ~ salary$courses)
summary(model2)

model3 <- lm(salary$salary ~ salary$years + salary$courses)
summary(model3)

model.z <- lm(scale(salary$salary) ~ scale(salary$years))
summary(model.z)

model2.z <- lm(scale(salary$salary) ~ scale(salary$courses))
summary(model2.z)

residuals <- resid(model3)