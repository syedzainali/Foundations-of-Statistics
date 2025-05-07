#Q1(b)
set.seed(42)  # For reproducibility
# Number of samples
n <- 10000
# Generate U ~ Unif(0, 1)
U <- runif(n)
# Transform U using the inverse CDF of Exp(1): F_X^-1(u) = -ln(1 - u)
Z <- -log(1 - U)
# Compare the empirical distribution of Z to Exp(1)
# Plot histogram of Z
hist(Z, breaks = 50, probability = TRUE, col = "lightblue", 
     main = "Distribution of Z (via Inverse CDF)", xlab = "Z")
# Overlay theoretical density of Exp(1): f(x) = exp(-x)
curve(dexp(x, rate = 1), col = "red", lwd = 2, add = TRUE)
# Verify mean and variance match Exp(1)
cat("Empirical mean of Z:", mean(Z), "\n")
cat("Empirical variance of Z:", var(Z), "\n")
cat("Theoretical mean and variance of Exp(1): 1 and 1\n")

#Q.1(c)
set.seed(42)  # For reproducibility
# Parameters
n <- 10^5  # Number of simulated values
#Define the density function f(x)
f <- function(x) {
  2 / (x + 1)^3
}
#Define the inverse CDF (derived analytically)
F_inv <- function(u) {
  (2 / (1 - u)) - 1
}

#Simulate values using the inverse transform sampling method
U <- runif(n)  # Generate n samples from Unif(0, 1)
X <- F_inv(U)  # Transform using the inverse CDF

#Filter values to plot (to handle heavy tail, only x < 10)
X_filtered <- X[X < 10]

#Create a histogram
hist(X_filtered, breaks = 100, probability = TRUE, 
     main = "Histogram of Simulated Values with Density Function", 
     xlab = "x", col = "lightblue", xlim = c(0, 10))
#Overlay the density function
curve(f(x), from = 0, to = 10, col = "red", lwd = 2, add = TRUE)
#Add legend
legend("topright", legend = c("Simulated Histogram", "Density Function"), 
       fill = c("lightblue", NA), border = c("black", NA), lty = c(NA, 1), 
       col = c("lightblue", "red"))

#Q3(a)
# Number of samples
n <- 10^6
# Simulate U and V from Uniform(0, 1)
set.seed(42)
U <- runif(n)
V <- runif(n)

# Compute max(U, V) and min(U, V)
max_vals <- pmax(U, V)
min_vals <- pmin(U, V)

# Plot histogram and compare with theoretical density for max(U, V)
hist(max_vals, breaks = 50, probability = TRUE, col = "lightblue", main = "Max(U, V)", xlab = "Value")
curve(2 * x, from = 0, to = 1, col = "red", lwd = 2, add = TRUE)

# Plot histogram and compare with theoretical density for min(U, V)
hist(min_vals, breaks = 50, probability = TRUE, col = "lightgreen", main = "Min(U, V)", xlab = "Value")
curve(2 * (1 - x), from = 0, to = 1, col = "blue", lwd = 2, add = TRUE)

#Q.4(b)
# Probability of Z >= 3
p_z_ge_3 <- pnorm(3, lower.tail = FALSE)
p_abs_z_ge_3 <- 2 * p_z_ge_3
p_z_ge_3
p_abs_z_ge_3

#Q4(d)
# Load UsingR package
install.packages("UsingR")
library(UsingR)
# Load the exec.pay dataset
data("exec.pay")

#Calculate and print summary statistics
statistics <- c(min = min(exec.pay),
                Q1 = quantile(exec.pay, 0.25),
                median = median(exec.pay),
                mean = mean(exec.pay),
                Q3 = quantile(exec.pay, 0.75),
                max = max(exec.pay))
print(statistics)

#Set up plotting area for two side-by-side plots
par(mfrow = c(1, 2))  # Arrange plots side by side

# Compute outliers using IQR method (manual calculation)
Q1 <- quantile(exec.pay, 0.25)
Q3 <- quantile(exec.pay, 0.75)
IQR_value <- IQR(exec.pay)

# Upper and lower outlier thresholds
upper_threshold <- Q3 + 1.5 * IQR_value
lower_threshold <- Q1 - 1.5 * IQR_value

#outlier thresholds
cat("Upper outlier threshold:", upper_threshold, "\n")
cat("Lower outlier threshold:", lower_threshold, "\n")

# Identify outliers using the IQR method
outliers_above <- exec.pay[exec.pay > upper_threshold]
outliers_below <- exec.pay[exec.pay < lower_threshold]

# Print the outliers
cat("Upper outliers:\n")
print(outliers_above)
cat("Lower outliers:\n")
print(outliers_below)

#Boxplot without outliers
boxplot(exec.pay, outline = FALSE, main = "Boxplot without Outliers", 
        col = "lightblue", ylab = "Compensation in $1000")

#Highlight outliers on the first boxplot (including outliers)
boxplot(exec.pay, main = "Boxplot with Outliers",
        col = "lightgreen", ylab = "Compensation in $1000")
#points(rep(1, length(outliers_above)), outliers_above, col = "red", pch = 19)  # Upper outliers in red

#Q.4(e)
#Z-scores
z_scores <- (exec.pay - mean(exec.pay)) / sd(exec.pay)

# Proportion of data beyond 3 standard deviations
prop_outliers <- mean(abs(z_scores) > 3)
prop_outliers

#Q.4(g)
# Load the exec.pay dataset
data(exec.pay)
# Define n, mean, and standard deviation manually
n <- length(exec.pay)
mean_exec <- mean(exec.pay)
# Use denominator 'n' for consistency
std_dev_exec <- sqrt(sum((exec.pay - mean_exec)^2) / n)
# Compute z-scores
z_scores <- (exec.pay - mean_exec) / std_dev_exec
# Calculate skewness using z-scores
sample_skewness <- mean(z_scores^3)
# Calculate skewness using the original formula
numerator <- sum((exec.pay - mean_exec)^3)
denominator <- (sum((exec.pay - mean_exec)^2)^(3/2)) / sqrt(n)  # Adjust denominator for sqrt(n)
original_skewness <- numerator / denominator
# Print both values
cat("Sample Skewness (z-scores):", sample_skewness, "\n")
cat("Original Skewness (formula):", original_skewness, "\n")

#Q.4(h)
# Simulate 10^7 random samples from a standard normal distribution
set.seed(42)  # Set seed for reproducibility
sample_size <- 10^7
x <- rnorm(sample_size)
# Calculate the fourth moment
fourth_moment <- mean(x^4)
# Display the result
cat("The fourth moment (mean of x^4) for a standard normal distribution is:", fourth_moment, "\n")

#Q.4(i)
# Compute sample kurtosis
sample_kurtosis <- mean(z_scores^4) - 3
# Verify original kurtosis matches
original_kurtosis <- length(exec.pay) * sum((exec.pay - mean(exec.pay))^4) / (sum((exec.pay - mean(exec.pay))^2)^2) - 3

sample_kurtosis
original_kurtosis

#Q.5(d)
set.seed(42)
# Simulate 100 sums of Uniform(-0.5, 0.5)
n_sums <- 10^6
n <- 100
x <- matrix(runif(n * n_sums, -0.5, 0.5), nrow = n_sums, ncol = n)
s <- rowSums(x)

# Calculate probability |S| > 10
p_approx <- mean(abs(s) > 10)
cat("Approximate P(|S| > 10):", p_approx, "\n")

##

# Load the required package
install.packages("UsingR")  # Run if package is not already installed
library(UsingR)

