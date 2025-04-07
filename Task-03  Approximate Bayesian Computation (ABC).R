
if (!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)
# theta3 to numeric
theta3_fixed <- as.numeric(theta3)
theta3_fixed
theta3_val_x1_4 <- theta3_fixed[3]  # coefficient for x1^4
theta3_val_x2   <- theta3_fixed[4]  # coefficient for x2
theta3_bias     <- theta3_fixed[5]  # bias term
# Prior ranges around point estimates
theta1_prior <- runif(5000, min = 6.55, max = 10.55)   # θ₁ for x1
theta2_prior <- runif(5000, min = 4.25, max = 8.25)    # θ₂ for x1^2
#ABC Rejection Sampling
set.seed(123)
accepted_theta1 <- c()
accepted_theta2 <- c()
threshold <- 0.1 * sum((y - mean(y))^2)
# ABC Rejection Loop
for (i in 1:5000) {
  # Candidate theta values
  theta1 <- theta1_prior[i]
  theta2 <- theta2_prior[i]
  # Simulate y using candidate values
  y_sim <- theta1 * x1 + theta2 * x1^2 + theta3_val_x1_4 * x1^4 +
    theta3_val_x2 * x2 + theta3_bias
  # Calculate distance (squared error)
  distance <- sum((y - y_sim)^2)
  # Accept if within threshold
  if (distance < threshold) {
    accepted_theta1 <- c(accepted_theta1, theta1)
    accepted_theta2 <- c(accepted_theta2, theta2)
  }
}
# number of accepted samples
cat("Number of accepted samples:", length(accepted_theta1), "\n")
#Create Posterior DF
posterior_df <- data.frame(
  theta1 = accepted_theta1,
  theta2 = accepted_theta2
)
#Joint Posterior Plot
ggplot(posterior_df, aes(x = theta1, y = theta2)) +
  geom_point(alpha = 0.4, color = "darkblue") +
  labs(title = "Joint Posterior Distribution (θ₁ vs θ₂)",
       x = expression(theta[1] ~ "(x1 coefficient)"),
       y = expression(theta[2] ~ "(x1² coefficient)")) +
  theme_minimal()
# Marginal Posterior for θ₁
ggplot(posterior_df, aes(x = theta1)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Marginal Posterior of θ₁ (x1)",
       x = expression(theta[1]), y = "Frequency") +
  theme_minimal()
# Marginal Posterior for θ₂
ggplot(posterior_df, aes(x = theta2)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  geom_vline(xintercept = 6.25, color = "blue", linetype = "dashed", linewidth = 1.2) +
  labs(title = "Marginal Posterior of θ₂ (x1²)",
       x = expression(theta[2]), y = "Frequency") +
  theme_minimal()

