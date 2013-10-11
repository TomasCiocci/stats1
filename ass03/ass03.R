# Load packages
library(psych)
library(sm)
library(rgl)
library(gclus)

setwd("/Users/misha/git/stats1/ass03")
data <- read.table("Stats1.13.HW.03.txt", header = T)

cat("q1) ", cor(data$S1.pre, data$S2.pre))
cat("q2) ", cor(data$V1.pre, data$V2.pre))

aer <- subset(data, data$cond == "aer")
des <- subset(data, data$cond == "des")

data$spatial.pre <- (data$S1.pre + data$S2.pre)/2
data$spatial.post <- (data$S1.post + data$S2.post)/2
cor(data$spatial.pre, (data$spatial.post - data$spatial.pre))

data$verbal.pre <- (data$V1.pre + data$V2.pre)/2
data$verbal.post <- (data$V1.post + data$V2.post)/2
cor(data$verbal.pre, (data$verbal.post - data$verbal.pre))

# for convenience...
aer <- subset(data, data$cond == "aer")
des <- subset(data, data$cond == "des")

mean(des$spatial.post - des$spatial.pre)
mean(aer$spatial.post - aer$spatial.pre)

pre_matrix <- data[, c("S1.pre", "S2.pre", "V1.pre", "V2.pre")]
pre_matrix.r <- abs(cor(pre_matrix))
pre_matrix.color <- dmat.color(pre_matrix.r)
pre_matrix.order <- order.single(pre_matrix.r)
cpairs(pre_matrix, pre_matrix.order, panel.colors = pre_matrix.color, gap = .5)

post_matrix <- data[, c("S1.post", "S2.post", "V1.post", "V2.post")]
post_matrix.r <- abs(cor(post_matrix))
post_matrix.color <- dmat.color(post_matrix.r)
post_matrix.order <- order.single(post_matrix.r)
cpairs(post_matrix, post_matrix.order, panel.colors = post_matrix.color, gap = .5)

