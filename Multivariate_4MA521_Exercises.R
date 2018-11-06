##### 0. File information and prep #####
# Author: Fredrik A. A. Eriksson
# Date: 29 October, 2018


setwd("/Users/Fredrik/Dropbox/LnU 2 - Multivariate Analysis - 4MA521 - HT18")
library(ggplot2)
# install.packages("xtable")
library(xtable)

##### 1. Chapter 1 #####

## Exercise 1.6
# The data in Table 1.5 are 42 measurements on air-pollution variables recorded at 12:00
# noon in the Los Angeles area on different days. (See also the air-pollution data on the
#                                                  web at www.prenhall.com/statistics.)
# (a) Plot the marginal dot diagrams for all the variables.
# (b) Construct the x, Sn, and R arrays, and interpret the entries in R.

table1.5 <- read.table("JohnsonWichern Data sets/T1-5.DAT", header=FALSE)
airPollution <- table1.5

# A) 
dotPlot1V1 <- ggplot(airPollution, aes(x = V1)) + 
  geom_dotplot(dotsize=0.5) +
  xlab("x_1 (Wind)") +
  coord_cartesian(ylim = c(0, 10))  +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("outputs/1_6_dotPlot1.pdf")

dotPlot1V7 <- ggplot(airPollution, aes(x = V7)) + 
  geom_dotplot(dotsize=0.5) +
  xlab("x_7 (HC)") +
  coord_cartesian(ylim = c(0, 10))  +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("outputs/1_6_dotPlot7.pdf")

# B)
means1 <- as.table(colMeans(airPollution))
cov1 <- cov(airPollution, y = NULL, use = "everything", method = "pearson")
cor1 <- cor(airPollution, y = NULL, use = "everything", method = "pearson")

print(xtable(means1, type = "latex"), file = "outputs/means1_6.tex")
print(xtable(cov1, type = "latex"), file = "outputs/cov1_6.tex")
print(xtable(cor1, type = "latex"), file = "outputs/cor1_6.tex")

print(means1)
print(cov1)
print(cor1)

## Exercise 1.12
# Define the distance from the point P = (x1 , x2) to the origin 0 = (0, 0) as
# d(O, P) = max(/xJ/, /x2 /)
# (a) Compute the distance from P == ( -3,4) to the origin.
# (b) Plot the locus of points whose squared distance from the origin is L
# (c) Generalize the foregoing distance expression to points in p dimensions.


pdf(file = "outputs/1_12_b.pdf", height=6, width=6)

fig1 <- ggplot() +
  xlab("x") +
  ylab("y") +
  geom_hline(mapping = NULL, data = NULL, yintercept = 1,
             na.rm = FALSE, show.legend = NA) +
  geom_hline(mapping = NULL, data = NULL, yintercept = -1,
             na.rm = FALSE, show.legend = NA) +
  geom_vline(mapping = NULL, data = NULL, xintercept = 1,
           na.rm = FALSE, show.legend = NA) +
  geom_vline(mapping = NULL, data = NULL, xintercept = -1,
             na.rm = FALSE, show.legend = NA)
# ggsave("outputs/1_12_b.pdf")
print(fig1)

dev.off()


## Exercise 1.15
# Some of the 98 measurements described in Section 1.2 are listed in Table 1.7 
# (See also the radiotherapy data on the web at www.prenhall.com/statistics.) 
# The data consist of average ratings over the course of treatment for patients 
# undergoing radiotherapy. Variables measured include 
# x1 (number of symptoms, such as sore throat or nausea); 
# x2 (amount of activity, on a 1-5 scale); 
# x3 (amount of sleep, on a 1-5 scale); 
# x4 (amount of food consumed, on a 1-3 scale); 
# x5 (appetite, on a 1-5 scale); and 
# x6 (skin reaction, on a 0-3 scale).
# (a) Construct the two-dimensional scatter plot for variables x2 and x3 and the marginal
# dot diagrams (or histograms). Do there appear to be any errors in the x3 data?
# (b) Compute the x, Sn, and R arrays. Interpret the pairwise correlations.

# A):
dotPlot2V2 <- ggplot(radiology, aes(x = V2)) + 
  geom_dotplot(dotsize=0.5) +
  xlab("x_2 (Activity)") +
  coord_cartesian(ylim = c(0, 10))  +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("outputs/1_15_dotPlot2.pdf")

dotPlot2V3 <- ggplot(radiology, aes(x = V3)) + 
  geom_dotplot(dotsize=0.5) +
  xlab("x_3 (Sleep)") +
  coord_cartesian(ylim = c(0, 10))  +
  theme_minimal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
ggsave("outputs/1_15_dotPlot3.pdf")

scatterplot1 <- ggplot(radiology, aes(x = V2, y = V3)) + 
  geom_point()
ggsave("outputs/1_15_scatterplot.pdf")


# B):
table1.7 <- read.table("JohnsonWichern Data sets/T1-7.DAT", header=FALSE)
radiology <- table1.7

means2 <- as.table(colMeans(radiology))
cov2 <- cov(radiology, y = NULL, use = "everything", method = "pearson")
cor2 <- cor(radiology, y = NULL, use = "everything", method = "pearson")

print(xtable(means2, type = "latex"), file = "outputs/means1_15.tex")
print(xtable(cov2, type = "latex"), file = "outputs/cov1_15.tex")
print(xtable(cor2, type = "latex"), file = "outputs/cor1_15.tex")

##### 2. Chapter 2 #####
