#part 1 meet the data
install.packages("tidyverse")
library(tidyverse)
install.packages("readxl")
library(readxl)
data <- read_excel("/Users/victoralvarez/Desktop/MA541/Course Project/Course project Data/MA 541 Course Project Data.xlsx")
# Sample mean and sample standard deviation for each column
data_summary <- data %>% summarise(
  mean_CloseETF = mean(Close_ETF, na.rm = TRUE),
  sd_CloseETF = sd(Close_ETF, na.rm = TRUE),
  mean_Oil = mean(oil, na.rm = TRUE),
  sd_Oil = sd(oil, na.rm = TRUE),
  mean_Gold = mean(gold, na.rm = TRUE),
  sd_Gold = sd(gold, na.rm = TRUE),
  mean_JPM = mean(JPM, na.rm = TRUE),
  sd_JPM = sd(JPM, na.rm = TRUE),
)
print(data_summary)
#sample correlations
cor_matrix <- cor(data, use = "complete.obs")
print(cor_matrix)
#Part 2 Describe your data
par(mfrow=c(2,2))  # Set up the plotting area to have a 2x2 grid

hist(data$Close_ETF, main="ETF", col="skyblue", border="black")
hist(data$oil, main="OIL", col="skyblue", border="black")
hist(data$gold, main="GOLD", col="skyblue", border="black")
hist(data$JPM, main="JPM", col="skyblue", border="black")

#time series plot
par(mfrow=c(2,2))

plot(data$Close_ETF, type="l", main="ETF Time Series", ylab="ETF")
plot(data$oil, type="l", main="OIL Time Series", ylab="OIL")
plot(data$gold, type="l", main="GOLD Time Series", ylab="GOLD")
plot(data$JPM, type="l", main="JPM Time Series", ylab="JPM")

#Time Series plot for all four columns
# Convert the data to a long format
long_data <- data %>%
  gather(variable, value)

# Plotting
ggplot(long_data, aes(x = as.numeric(rownames(long_data)), y = value, color = variable)) + 
  geom_line() +
  labs(title = "Time Series Plot for All Columns",
       x = "Index", 
       y = "Value") +
  theme_minimal() +
  scale_color_manual(values = c("red", "blue", "green", "purple"))
#Scatter Plot describing the relationship berween close_ETF and the other columns
par(mfrow=c(2,2))

plot(data$Close_ETF, data$oil, main="ETF vs OIL", xlab="ETF", ylab="OIL")
plot(data$Close_ETF, data$gold, main="ETF vs GOLD", xlab="ETF", ylab="GOLD")
plot(data$Close_ETF, data$JPM, main="ETF vs JPM", xlab="ETF", ylab="JPM")

#Part 3 What distribution does your data follow
# Shapiro-Wilk Test
shapiro.test(data$Close_ETF)
shapiro.test(data$oil)
shapiro.test(data$gold)
shapiro.test(data$JPM)

#Part 4 Break your data into small groups and let them discuss the importance of the Central Limit Theorem
#1)	Calculate the mean 〖μ〗_x  and the standard deviation σ_x  of the population.
mu_x <- mean(data$Close_ETF)
sigma_x <- sd(data$Close_ETF)
print(mu_x)
print(sigma_x)
#2)	Break the population into 50 groups sequentially and each group includes 20 values. 
grouped_means_20 <- sapply(split(data$Close_ETF, ceiling(seq_along(data$Close_ETF)/20)), mean)
print(grouped_means_20)
#3) Histogram of sample means.
hist(grouped_means_20, main="Histogram of Sample Means (n=20)", xlab="Sample Mean")
#4)	Calculate the mean (〖μ〗_¯x )  and the standard deviation (σ_¯x) of the data including these sample means.
#Make a comparison between μ_x and μ_¯x , between ( σ_x)/√n and σ_¯x .
#Here, n is the number of sample means calculated from Item 3) above.
mu_bar_x_20 <- mean(grouped_means_20)
sigma_bar_x_20 <- sd(grouped_means_20)
print(mu_bar_x_20)
print(sigma_bar_x_20)
print(paste("Mean of ETF column (μ_x) =", round(mu_x, 4)))
print(paste("Mean of sample means (μ_{bar{x}}) for n=20 =", round(mu_bar_x_20, 4)))
expected_sigma_bar_x_20 <- sigma_x / sqrt(20)
print(expected_sigma_bar_x_20)

print(paste("Standard deviation of ETF column (σ_x) =", round(sigma_x, 4)))
print(paste("Expected standard deviation of sample means for n=20 (σ_x/√n) =", round(expected_sigma_bar_x_20, 4)))
print(paste("Calculated standard deviation of sample means for n=20 (σ_{bar{x}}) =", round(sigma_bar_x_20, 4)))

#6) Break the population into 10 groups sequentially and each group includes 100 values.
groups_100 <- split(data$Close_ETF, ceiling(seq_along(data$Close_ETF)/100))
grouped_means_100 <- sapply(groups_100, mean)
print(grouped_means_100)
#7)	Repeat Items 3) ~ 5).
hist(grouped_means_100, main="Histogram of Sample Means (n=100)", xlab="Sample Mean")
mu_bar_x_100 <- mean(grouped_means_100)
sigma_bar_x_100 <- sd(grouped_means_100)
print(paste("Mean of ETF column (μ_x) =", round(mu_x, 4)))
print(paste("Mean of sample means (μ_{bar{x}}) for n=100 =", round(mu_bar_x_100, 4)))
expected_sigma_bar_x_100 <- sigma_x / sqrt(100)

print(paste("Standard deviation of ETF column (σ_x) =", round(sigma_x, 4)))
print(paste("Expected standard deviation of sample means for n=100 (σ_x/√n) =", round(expected_sigma_bar_x_100, 4)))
print(paste("Calculated standard deviation of sample means for n=100 (σ_{bar{x}}) =", round(sigma_bar_x_100, 4)))

#8)	Generate 50 simple random samples or groups (with replacement) from the population. 
#The size of each sample is 20, i.e., each group includes 20 values.
set.seed(123) # Ensuring reproducibility
random_samples_20 <- replicate(50, sample(data$Close_ETF, size = 20, replace = TRUE))
sample_means_20 <- apply(random_samples_20, 2, mean)

#9)	Repeat Items 3) ~ 5).
hist(sample_means_20, main="Histogram of Sample Means from Random Samples (n=20)", xlab="Sample Mean")
mu_bar_x_rand_20 <- mean(sample_means_20)
sigma_bar_x_rand_20 <- sd(sample_means_20)
print(paste("Mean of ETF column (μ_x) =", round(mu_x, 4)))
print(paste("Mean of sample means from random samples (μ_{bar{x}}) for n=20 =", round(mu_bar_x_rand_20, 4)))
expected_sigma_bar_x_rand_20 <- sigma_x / sqrt(20)

print(paste("Standard deviation of ETF column (σ_x) =", round(sigma_x, 4)))
print(paste("Expected standard deviation of sample means from random samples for n=20 (σ_x/√n) =", round(expected_sigma_bar_x_rand_20, 4)))
print(paste("Calculated standard deviation of sample means from random samples for n=20 (σ_{bar{x}}) =", round(sigma_bar_x_rand_20, 4)))

#10)	Generate 10 simple random samples or groups (with replacement) from the population. The size of each sample is 100, i.e., each group includes 100 values.
set.seed(123)  # Ensuring reproducibility
random_samples_100 <- replicate(10, sample(data$Close_ETF, size = 100, replace = TRUE))
#11)	Repeat Items 3) ~ 5).
sample_means_100_rand <- apply(random_samples_100, 2, mean)
hist(sample_means_100_rand, main="Histogram of Sample Means from Random Samples (n=100)", xlab="Sample Mean")
mu_bar_x_rand_100 <- mean(sample_means_100_rand)
sigma_bar_x_rand_100 <- sd(sample_means_100_rand)
print(paste("Mean of ETF column (μ_x) =", round(mu_x, 4)))
print(paste("Average of sample means from random samples (μ_{bar{x}}) for n=100 =", round(mu_bar_x_rand_100, 4)))
expected_sigma_bar_x_rand_100 <- sigma_x / sqrt(100)

print(paste("Standard deviation of ETF column (σ_x) =", round(sigma_x, 4)))
print(paste("Expected standard deviation of sample means from random samples for n=100 (σ_x/√n) =", round(expected_sigma_bar_x_rand_100, 4)))
print(paste("Standard deviation of sample means from random samples for n=100 (σ_{bar{x}}) =", round(sigma_bar_x_rand_100, 4)))

#Part 5: Construct a confidence interval with your data
#For one of the 10 random samples (n=100):
sample_100 <- random_samples_100[, 1] # Taking the first sample for demonstration
mean_100 <- mean(sample_100)
std_100 <- sd(sample_100)
print(mean_100)
print(std_100)

# 95% CI using t-distribution (because population standard deviation might be unknown)
ci_lower_100 <- mean_100 - qt(0.975, df=99) * (std_100/sqrt(100))
ci_upper_100 <- mean_100 + qt(0.975, df=99) * (std_100/sqrt(100))
print(ci_lower_100)
print(ci_upper_100)
#For one of the 50 random samples (n=20):
sample_20 <- random_samples_20[, 1] # Taking the first sample for demonstration
mean_20 <- mean(sample_20)
std_20 <- sd(sample_20)
print(mean_20)
print(std_20)
# 95% CI using t-distribution
ci_lower_20 <- mean_20 - qt(0.975, df=19) * (std_20/sqrt(20))
ci_upper_20 <- mean_20 + qt(0.975, df=19) * (std_20/sqrt(20))
print(ci_lower_20)
print(ci_upper_20)
#Comparing with the population mean 
# Check if mu_x lies in the 95% CI for the n=100 sample
within_ci_100 <- ci_lower_100 <= mu_x && ci_upper_100 >= mu_x
print(within_ci_100)
# Check if mu_x lies in the 95% CI for the n=20 sample
within_ci_20 <- ci_lower_20 <= mu_x && ci_upper_20 >= mu_x
print(within_ci_20)
#Part 6: Form a hypothesis and test it with your data
t_test_100 <- t.test(sample_100, mu = 100)
p_value_100 <- t_test_100$p.value
t_test_20 <- t.test(sample_20, mu = 100)
p_value_20 <- t_test_20$p.value
observed_chi <- (length(sample_20) - 1) * (sd(sample_20)^2) / 15^2
chi_test <- pchisq(observed_chi, df = length(sample_20) - 1, lower.tail = FALSE) + pchisq(observed_chi, df = length(sample_20) - 1)
chi_test_one_sided <- pchisq(observed_chi, df = length(sample_20) - 1)
print(t_test_100)
print(p_value_100)
print(t_test_20)
print(p_value_20)
print(observed_chi)
print(chi_test)
print(chi_test_one_sided)
#Part 7: Compare your data with a different data set
t_test_means <- t.test(data$gold, data$oil, alternative = "two.sided", var.equal = FALSE)
t_test_means$p.value
diff_gold_oil <- data$gold - data$oil
t_test_diff <- t.test(diff_gold_oil, mu = 0, alternative = "two.sided")
t_test_diff$p.value
var_gold <- var(data$gold)
var_oil <- var(data$oil)
f_statistic <- var_gold / var_oil
print(t_test_means)
print(diff_gold_oil)
print(var_gold)
print(var_oil)
print(f_statistic)
# Using the F-distribution to get the p-value
p_value_f <- 2 * min(pf(f_statistic, length(data$gold)-1, length(data$oil)-1), 1 - pf(f_statistic, length(data$gold)-1, length(data$oil)-1))

#Part 8: Fitting the line to the data
#Scatter plot of ETF vs. Gold
ggplot(data, aes(x=gold, y=Close_ETF)) +
  geom_point() +
  ggtitle("Scatter plot of ETF vs. Gold") +
  xlab("Gold") +
  ylab("ETF")
#Calculate the coefficient of correlation between ETF and Gold
correlation <- cor(data$gold, data$Close_ETF)
correlation
#Fit a regression line
fit <- lm(Close_ETF~ gold, data=data)
summary(fit)
#Two-tailed t-test for 
fit_summary <- summary(fit)
p_value <- fit_summary$coefficients[2,4]
p_value
#Calculate confidence and prediction intervals
newdata <- data.frame(gold = 0.005127)
predict(fit, newdata, interval="confidence", level=0.99)
predict(fit, newdata, interval="prediction", level=0.99)

#Part 9: Does your model predict?
fit_mlr <- lm(Close_ETF ~ gold + oil, data=data)
adj_r2 <- summary(fit_mlr)$adj.r.squared
adj_r2
#Part 10
residuals <- residuals(fit_mlr)
mean(residuals)

plot(fitted(fit_mlr), residuals, 
     ylab="Residuals", xlab="Fitted Values", 
     main="Residuals vs Fitted Values")
abline(h=0, col="red")
qqnorm(residuals)
qqline(residuals)
plot(residuals, type='line', 
     ylab="Residuals", xlab="Order of Data", 
     main="Residuals in Order of Data")




