setwd("C:/Users/kittu/OneDrive/Desktop/Coventry_Assignments/Statstics/8f10f805f930dc1e548a083330b18258data/data")

# Load data
load("X.RData")      
load("y.RData")      
load("time.RData")   
X_data <- X
Y_data <- y
time_data <- time
neuro <- data.frame(
  time = time_data[[1]],
  x1 = X_data[[1]],
  x2 = as.factor(X_data[[2]]),
  y = Y_data[[1]]
)
x1 <- neuro$x1
x2 <- as.numeric(as.character(neuro$x2))
y  <- neuro$y
n  <- length(y)
if (!require(MASS)) install.packages("MASS")
if (!require(ggplot2)) install.packages("ggplot2")
library(MASS)
library(ggplot2)
#2.1
# Generalized least squares
least_squares <- function(X, y) {
  MASS::ginv(t(X) %*% X) %*% t(X) %*% y
}
# Model 1
X1 <- cbind(x1^3, x1^5, x2, rep(1, n))
theta1 <- least_squares(X1, y)
# Model 2
X2 <- cbind(x1, x2, rep(1, n))
theta2 <- least_squares(X2, y)
# Model 3
X3 <- cbind(x1, x1^2, x1^4, x2, rep(1, n))
theta3 <- least_squares(X3, y)
# Model 4
X4 <- cbind(x1, x1^2, x1^3, x1^5, x2, rep(1, n))
theta4 <- least_squares(X4, y)
# Model 5
X5 <- cbind(x1, x1^3, x1^4, x2, rep(1, n))
theta5 <- least_squares(X5, y)
# Print
cat("Model 1 :\n"); print(theta1)
cat("Model 2 :\n"); print(theta2)
cat("Model 3 :\n"); print(theta3)
cat("Model 4 :\n"); print(theta4)
cat("Model 5 :\n"); print(theta5)
#2.1 RSS Values
compute_rss <- function(X, y, theta) {
  y_pred <- X %*% theta
  sum((y - y_pred)^2)
}
rss1 <- compute_rss(X1, y, theta1)
rss2 <- compute_rss(X2, y, theta2)
rss3 <- compute_rss(X3, y, theta3)
rss4 <- compute_rss(X4, y, theta4)
rss5 <- compute_rss(X5, y, theta5)
cat("RSS Model 1:", rss1, "\n")
cat("RSS Model 2:", rss2, "\n")
cat("RSS Model 3:", rss3, "\n")
cat("RSS Model 4:", rss4, "\n")
cat("RSS Model 5:", rss5, "\n")
#2.3 log-likelihood
compute_loglik <- function(rss, n) {
  sigma2 <- rss / (n - 1)
  - (n / 2) * log(2 * pi) - (n / 2) * log(sigma2) - (1 / (2 * sigma2)) * rss
}
loglik1 <- compute_loglik(rss1, n)
loglik2 <- compute_loglik(rss2, n)
loglik3 <- compute_loglik(rss3, n)
loglik4 <- compute_loglik(rss4, n)
loglik5 <- compute_loglik(rss5, n)
cat("Log-Likelihood Model 1:", loglik1, "\n")
cat("Log-Likelihood Model 2:", loglik2, "\n")
cat("Log-Likelihood Model 3:", loglik3, "\n")
cat("Log-Likelihood Model 4:", loglik4, "\n")
cat("Log-Likelihood Model 5:", loglik5, "\n")
#2.4 AIC and BIC 
# Number of parameters (k)
k1 <- 4; k2 <- 3; k3 <- 5; k4 <- 6; k5 <- 5
aic1 <- 2*k1 - 2*loglik1
aic2 <- 2*k2 - 2*loglik2
aic3 <- 2*k3 - 2*loglik3
aic4 <- 2*k4 - 2*loglik4
aic5 <- 2*k5 - 2*loglik5
bic1 <- k1*log(n) - 2*loglik1
bic2 <- k2*log(n) - 2*loglik2
bic3 <- k3*log(n) - 2*loglik3
bic4 <- k4*log(n) - 2*loglik4
bic5 <- k5*log(n) - 2*loglik5
cat("Model 1 AIC:", aic1, " BIC:", bic1, "\n")
cat("Model 2 AIC:", aic2, " BIC:", bic2, "\n")
cat("Model 3 AIC:", aic3, " BIC:", bic3, "\n")
cat("Model 4 AIC:", aic4, " BIC:", bic4, "\n")
cat("Model 5 AIC:", aic5, " BIC:", bic5, "\n")
# 2.5 Residual Plots
y_pred3 <- X3 %*% theta3
residuals3 <- y - y_pred3
ggplot(data.frame(residuals3), aes(x = residuals3)) +
  geom_histogram(bins = 30, fill = "orchid", color = "black") +
  labs(title = "Histogram of Residuals (Model 3)", x = "Residual", y = "Frequency")
qqnorm(residuals3, main = "Q-Q Plot of Residuals (Model 3)", col = "blue")
qqline(residuals3, col = "red", lwd = 2)
#2.7 Train Split 
set.seed(123)
n_total <- nrow(neuro)
train_index <- sample(1:n_total, size = 0.7 * n_total)
train_neuro <- neuro[train_index, ]
test_neuro  <- neuro[-train_index, ]
# Extract variables
x1_train <- train_neuro$x1
x2_train <- as.numeric(as.character(train_neuro$x2))
y_train  <- train_neuro$y
x1_test <- test_neuro$x1
x2_test <- as.numeric(as.character(test_neuro$x2))
y_test  <- test_neuro$y
# Model 3 design matrices
X_train <- cbind(x1_train, x1_train^2, x1_train^4, x2_train, rep(1, length(x1_train)))
X_test <- cbind(x1_test, x1_test^2, x1_test^4, x2_test, rep(1, length(x1_test)))
# Fit on train
theta_model3 <- MASS::ginv(t(X_train) %*% X_train) %*% t(X_train) %*% y_train
# Predict
y_pred_test <- X_test %*% theta_model3
residuals_train <- y_train - X_train %*% theta_model3
sigma2_hat <- sum(residuals_train^2) / (nrow(X_train) - ncol(X_train))
X_test_se <- sqrt(diag(X_test %*% MASS::ginv(t(X_train) %*% X_train) %*% t(X_test)))
ci_upper <- y_pred_test + 1.96 * sqrt(sigma2_hat) * X_test_se
ci_lower <- y_pred_test - 1.96 * sqrt(sigma2_hat) * X_test_se
df_pred <- data.frame(
  index = 1:length(y_test),
  actual = y_test,
  predicted = y_pred_test,
  lower = ci_lower,
  upper = ci_upper
)
ggplot(df_pred, aes(x = index)) +
  geom_point(aes(y = actual), color = "black", size = 1.5, alpha = 0.6) +
  geom_line(aes(y = predicted), color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "lightblue", alpha = 0.3) +
  labs(title = "Model 3: Prediction with 95% Confidence Interval",
       x = "Test Sample Index", y = "Brain Signal (y)") +
  theme_minimal()
#Final
rss_test <- sum((y_test - y_pred_test)^2)
tss_test <- sum((y_test - mean(y_test))^2)
r2 <- 1 - (rss_test / tss_test)
mse <- mean((y_test - y_pred_test)^2)
mae <- mean(abs(y_test - y_pred_test))

cat("R-squared (R²):", r2, "\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

