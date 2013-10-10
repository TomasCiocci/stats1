setwd("/Users/misha/r")

# Load packages
library(psych)
library(sm)

# Read data into a dataframe called impact
data <- read.table("Stats1.13.HW.02.txt", header = T)
pre <- subset(data, data[, 3]=="pre")
post <- subset(data, data[, 3]=="post")

cat("num rows:", nrow(data))
cat("mean SR:", mean(data[,4]))
cat("variance SR:", var(data[,4]))

cat("mean SR (pretest):", mean(pre[,4]))
cat("stddev SR (posttest):", sd(post[,4]))
cat("median SR (posttest):", median(post[,4]))

wm_pre <- subset(pre, pre[, 2] == "WM")
pe_pre <- subset(pre, pre[, 2] == "PE")
ds_pre <- subset(pre, pre[, 2] == "DS")

wm_post <- subset(post, post[, 2] == "WM")
pe_post <- subset(post, post[, 2] == "PE")
ds_post <- subset(post, post[, 2] == "DS")

cat("posttest SR means: WM: ", mean(wm_post[,4]), " PE: ", mean(pe_post[,4]), " DS: ", mean(ds_post[,4]))

par(mfrow = c(2,3)) # To view 6 histograms on one page 
hist(wm_pre[, 4], xlab = "WM pre", main = "") 
hist(pe_pre[, 4], xlab = "PE pre", main = "") 
hist(ds_pre[, 4], xlab = "DS pre", main = "") 

hist(wm_post[, 4], xlab = "WM post", main = "") 
hist(pe_post[, 4], xlab = "PE post", main = "") 
hist(ds_post[, 4], xlab = "DS post", main = "") 
