# Import dataset
airquality = read.table("PM25.csv", header = TRUE, sep=",")
airquality
respdis = read.table("chron-resp.csv", header = TRUE, sep=",") # respiratory diseases
respdis
smoking=read.table("smoking.csv", header = TRUE, sep=",")
smoking
# Merging the air quality data and the respiratory diseases rates in one table
# using na.omit() to exclude NA values
data1 = na.omit(merge(airquality, respdis, by = "Countries"))
data
# Merging all of the data together
# We had to divide the merging in 2 steps, because the merge function couldn't handle combining
# all of the 3 columns at once
data = na.omit(merge(data1, smoking, by = "Countries"))
data
# Setting the variables for easier use
pm25 = data$pm25
crd = data$crd
smoking = data$Smoking
# Excluding outliers to see if there was a significant difference in the relation 
# between air quality vs. crd with outliers and without
subset_indices = which(data$pm25 < 80)
pm25v2 = data$pm25[subset_indices]
crdv2 = data$crd[subset_indices]
cor(pm25v2, crdv2) #0.5272442
# Checking differences between correlations with outliers (air quality)
cor(pm25, crd) #0.5335651
# It doesn't seem like there is a significant change in the correlation, therefore we decided to keep our outliers
### DESCRIPTIVE OVERVIEW ###
summary(pm25)
summary(crd)
summary(smoking)
par(mfrow=c(2,3))
# The Histograms of our 3 variables
hist(crd, col = "lavender", xlab = "Death rate from chronic \n respiratory diseases", main="Distribution of deaths \n from chronic \n respiratory diseases", probability = TRUE, cex.main=1)
abline(v=mean(crd), col="maroon", lwd=2)
hist(pm25, col = "lavender", xlab = "Average PM2.5 \n concentration (μg/m³)", main="Distribution \n of PM2.5 \n concentration", probability = TRUE, cex.main=1)
abline(v=mean(pm25), col="maroon", lwd=2)
hist(smoking, col = "lavender", xlab = "Prevalence of tobacco use \n in 2019 (% of adults)", main="Distribution \n of the smoking \n rates", probability = TRUE, cex.main=1)
abline(v=mean(smoking), col="maroon", lwd=2)
# The qqnorms of our 3 variables
qqnorm(crd, col="maroon", main = "QQ-plot of deaths \n from chronic \n respiratory diseases", cex.main=1)
qqline(crd)
qqnorm(pm25, col="maroon", main = "QQ-plot of PM2.5 \n concentration", cex.main=1)
qqline(pm25)
qqnorm(smoking, col="maroon", main = "QQ-plot of the \n smoking rates", cex.main=1)
qqline(smoking)
# Histogram of the crd distribution
par(mfrow=c(1,1))
hist(crd, col = "lavender", xlab = "Death rate from chronic respiratory diseases \n (per 100K people)", main="Distribution of deaths \n from chronic respiratory diseases \n in countries worldwide in 2019", probability = TRUE, ylim = c(0, 0.030))
lines(density(crd), col="darkblue", lwd=2)
# Histogram of the pm25 distribution
hist(pm25, col = "lavender", xlab = "Average PM2.5 concentration (μg/m³)", main="Distribution of PM2.5 concentration \n in countries worldwide in 2019", probability = TRUE,  ylim = c(0, 0.040))
lines(density(pm25), col="darkblue", lwd=2)
# Histogram of the smoking distribution
hist(smoking, col = "lavender", xlab = "Prevalence of tobacco use in 2019 (% of adults)", main="Distribution of the smoking rates \n in countries worldwide in 2019", probability = TRUE)
lines(density(smoking), col="darkblue", lwd=2)
# Scatterplot of pm25 vs. crd
par(mar = c(5, 6, 4, 2))
plot(pm25, crd, xlab="Average PM2.5 concentration (μg/m³) in 2019", ylab="Chronic respiratory \n diseases death rate (2019)", main="Scatter plot of \n air quality vs. respiratory diseases")
abline(v=mean(pm25), col="maroon")
abline(h=mean(crd), col="maroon")
# Scatterplot of smoking vs. crd
plot(smoking, crd, xlab="Prevalence of tobacco use in 2019 (% of adults)", ylab="Chronic respiratory \n diseases death rate (2019)", main="Scatter plot of \n smoking rate vs. respiratory diseases")
abline(v=mean(smoking), col="maroon")
abline(h=mean(crd), col="maroon")
#testing CLT
moyennes = function(donnees, taille) {
  moyennes = NULL
  for (i in 1:1200) {
    moyennes[i] = mean(sample(donnees, taille, replace=TRUE))
  }
  return(moyennes)
}
mean5=moyennes(crd, 5) 
mean20=moyennes(crd,20)
mean100=moyennes(crd,100)

par(mfrow = c(2, 2))
hist(crd, probability = T, col="lavender") # crd
box()
abline(v = mean(crd), col = "maroon", lwd=2)

hist(mean5, probability = T, col="lavender") # mean5
box()
abline(v = mean(crd), col = "maroon", lwd=2)

hist(mean20, probability = T, col="lavender") #mean20
box()
abline(v = mean(crd), col = "maroon", lwd=2)

hist(mean100, probability = T, col="lavender") #mean100
box()
abline(v = mean(crd), col = "maroon", lwd=2) 
### AIR QUALITY ANALYSIS ###
# We divided the sample in two sub-samples
# n_bad represents the number of countries with bad air quality (pm 2.5 <= 19.1)
# while n_good is the number of countries in our sample with good air quality
# median(pm25) = 19.1
# H0: E(crd | pm25 > 19.1) - E(crd | pm25 <= 19.1) = 0
# H1: E(crd | pm25 > 19.1) - E(crd | pm25 <= 19.1) > 0
# In H1, we suppose that the expected value of of chronic respiratory diseases 
# in countries with bad air quality is bigger than in the countries with good air qualities
estim_crd_bar_good = mean(crd[pm25 <= 19.1])
estim_crd_bar_good
estim_crd_bar_bad = mean(crd[pm25 > 19.1])
estim_crd_bar_bad
estim_crd_bar_bad - estim_crd_bar_good
# The variability of the estimators of the expectations of each of the sub-populations
s_bad = var(crd[pm25 > 19.1])
n_bad = length(crd[pm25 > 19.1])
s_bad/n_bad
s_good = var(crd[pm25 <= 19.1])
n_good = length(crd[pm25 <= 19.1])
s_good/n_good
var_estimator = (s_bad/n_bad) + (s_good/n_good)
var_estimator
se_estimator = sqrt(var_estimator)
se_estimator
# Calculate the z-score
z = ((estim_crd_bar_bad - estim_crd_bar_good) - 0)/se_estimator
z
# Calculate the p-value
p = 1-pnorm(z)
p
# Here, p = 3.364425e-05, therefore p<0.05, therefore we can reject H0
par(mfrow=c(1,1))
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 5, xlab = "Estimator", ylab = "Probability Density", main = "Normal estimator of air quality and crd under H0")
abline(v = z, col = "maroon", lwd=2, lt=2)
abline(v = qnorm(0.95), col = "aquamarine3", lwd=2)  # Critical z
legend("topleft", legend=c("95-percentile","z-score"), col=c("aquamarine3", "maroon"), lty=c(1, 1), lwd=2)
# Correlation test 
install.packages("car")
library(car)
dataEllipse(pm25,crd, xlab="Average PM2.5 concentration (μg/m³) in 2019", ylab="Chronic respiratory \n diseases death rate (2019)", col=c("black", "maroon"))
cor.test(pm25, crd)
### SMOKING ###
# We divided the sample in two sub-samples
# n_sm corresponds to the number of countries in our sample with a small rate of smokers in the country (less than 23.4%)
# n_big stands for the number of countries where the share of smokers is bigger than 23.4

# median(smoking) = 23.4
# H0: E(crd | smoking > 23.4) - E(crd | smoking <= 23.4) = 0
# H1: E(crd | smoking > 23.4) - E(crd | smoking <= 23.4) > 0
# In H1, we suppose that the expected value of of chronic respiratory diseases 
# In countries with a lot of smokers is bigger than in countries with lesser rate of smoking adults
estim_crd_bar_small = mean(crd[smoking <= 23.4])
estim_crd_bar_small
estim_crd_bar_big = mean(crd[smoking > 23.4])
estim_crd_bar_big
estim_crd_bar_big - estim_crd_bar_small
# The variability of the estimators of the expectations of each of the sub-populations
s_small = var(crd[smoking > 23.4])
n_sm = length(crd[smoking > 23.4])
s_small/n_sm 
s_big = var(crd[smoking <= 23.4])
n_big = length(crd[smoking <= 23.4])
s_big/n_big
var_estimator2 = (s_small/n1_sm) + (s_big/n2_sm)
var_estimator2
se_estimator2 = sqrt(var_estimator2)
se_estimator2
# Calculate the z-score
z2 = ((estim_crd_bar_big - estim_crd_bar_small) - 0)/se_estimator2
z2
# Calculate the p-value
p2 = 1-pnorm(z2)
p2
# p2 = 0.172899, with p2 > 0.05, we cannot reject H0
par(mfrow=c(1,1))
curve(dnorm(x, mean = 0, sd = 1), from = -4, to = 5, xlab = "Estimator", ylab = "Probability Density", main = "Normal estimator of smoking and crd under H0")
abline(v = z2, col = "maroon", lwd=2, lt=2)
abline(v = qnorm(0.95), col = "aquamarine3", lwd=2)  # Critical z
legend("topleft", legend=c("95-percentile","z-score"), col=c("aquamarine3", "maroon"), lty=c(1, 1), lwd=2)

#correlation test
dataEllipse(smoking,crd, xlab="Prevalence of tobacco use in 2019 (% of adults)", ylab="Chronic respiratory \n diseases death rate (2019)", col=c("black", "maroon"))
cor.test(smoking, crd)
