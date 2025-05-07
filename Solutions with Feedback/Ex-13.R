#Q.1(b)
# Parameters
n <- 20
p <- 0.5
alpha <- 0.05

# Plot distribution
x <- 0:n
prob <- dbinom(x, size = n, prob = p)
critical_value <- qbinom(c(alpha / 2, 1 - alpha / 2), size = n, prob = p)

barplot(prob, names.arg = x, col = ifelse(x %in% critical_value[1]:critical_value[2], "black", "red"),
        main = "Binomial Distribution with Critical Region", xlab = "Number of Heads", ylab = "Probability")

#Q.1(c)

observed_heads <-

sample_proportion <- observed_heads / n
Z <- (sample_proportion - p) / sqrt(p * (1 - p) / n)


p_value_clt <- 2 * pnorm(-abs(Z))

x_seq <- seq(-3, 3, length.out = 1000)
pdf_normal <- dnorm(x_seq, mean = 0, sd = 1)

plot(x_seq, pdf_normal, type = "l", col = "blue", lwd = 2, 
     xlab = "Z", ylab = "Density", 
     main = "Approximate Normal Distribution with Critical Regions")
abline(v = c(-1.96, 1.96), col = "black", lty = 2)

# Output results
list(Z = Z, p_value_clt = p_value_clt)


#Q.1(d)
observed_x <- 4

#for exact test
p_value_exact <- 2 * min(pbinom(observed_x, size = n, prob = p), 1 - pbinom(observed_x - 1, size = n, prob = p))

#for CLT approximation
sample_proportion <- observed_heads / n
Z <- (sample_proportion - p) / sqrt(p * (1 - p) / n)


p_value_clt <- 2 * pnorm(-abs(Z))
list(p_value_exact = p_value_exact, p_value_clt = p_value_clt)

#Q.2(b)
# Parameters
mu_0 <- 600
sample_mean <- 605
sigma <- 15
n <- 40
alpha <- 0.05

# Test statistic
z <- (sample_mean - mu_0) / (sigma / sqrt(n))
critical_z <- qnorm(1 - alpha)
p_value <- 1 - pnorm(z)

cat("T Value is:", z, "\n")

# Checkingifweshouldrejectthenullhypothesis
alpha <- 0.05
reject_null <- p_value < alpha

cat("Reject H0 at 0.05:", reject_null, "\n")

#Q.2(c)
# Parameters
mu_0 <- 600 
delta <- 5  
sigma <- 15   
alpha <- 0.01   
z_alpha <- qnorm(1 - alpha)

n_required <- (z_alpha * sigma / delta)^2
n_required <- ceiling(n_required) 

cat("Required sample is:", n_required, "\n")

#Q.5
alpha <- 2 * (1 - pnorm(2))
cat("Probability of Committing type I error is:",alpha, "\n")

beta <- pnorm(1) - pnorm(-3)
cat("Probability of Committing type II error is:",beta, "\n")


#Q.6
library(UsingR)
library(datasets)
data(normtemp)

summary(normtemp$temperature)


# Create a histogram 
hist(normtemp$temperature, 
     main = "Distribution of Body Temperatures",
     xlab = "Temperature (°F)",
     probability = TRUE)  

lines(density(normtemp$temperature), col = "red", lwd = 2)


qqnorm(normtemp$temperature)
qqline(normtemp$temperature)

t.test(normtemp$temperature, mu = 98.6)

print("We reject the Null Hypothesis as P-value is less than 0.05")
print("Alternative hypothesis H1 is: true mean is not equal to 98.6")




