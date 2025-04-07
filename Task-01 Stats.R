setwd("C:/Users/kittu/OneDrive/Desktop/Coventry_Assignments/Statstics/8f10f805f930dc1e548a083330b18258data/data")

load("X.RData")      # loads object named X
load("y.RData")      # loads object named y
load("time.RData")   # loads object named time

X_data <- X
Y_data <- y
time_data <- time

neuro <- data.frame(
  time = time_data[[1]],
  x1 = X_data[[1]],
  x2 = as.factor(X_data[[2]]),
  y = Y_data[[1]]
)

neuro$x2 <- factor(neuro$x2, levels = c(0, 1), labels = c("Neutral", "Emotional"))

head(neuro)
View(neuro)

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
library(ggplot2)
library(dplyr)

# Time Series: Audio Signal
ggplot(neuro, aes(x = time, y = x1)) +
  geom_line(color = "blue") +
  labs(title = "Time Series of Audio Signal (x1)", x = "Time (s)", y = "Audio Signal")

# Time Series: Brain Signal
ggplot(neuro, aes(x = time, y = y)) +
  geom_line(color = "darkred") +
  labs(title = "Time Series of Brain Signal (y)", x = "Time (s)", y = "Brain Signal")

# Histogram: Audio Signal
ggplot(neuro, aes(x = x1)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Audio Signal (x1)", x = "x1", y = "Frequency")

# Histogram: Brain Signal
ggplot(neuro, aes(x = y)) +
  geom_histogram(bins = 30, fill = "salmon", color = "black") +
  labs(title = "Distribution of Brain Signal (y)", x = "y", y = "Frequency")

# Correlation: x1 vs y
cor(neuro$x1, neuro$y)

# Scatter Plot: x1 vs y
ggplot(neuro, aes(x = x1, y = y)) +
  geom_point(alpha = 0.6, color = "mediumpurple") +
  labs(title = "Audio Signal vs Brain Response", x = "Audio Input (x1)", y = "Brain Output (y)") +
  theme_bw()

# Boxplot: Brain Signal by Audio Type
ggplot(neuro, aes(x = x2, y = y, fill = x2)) +
  geom_boxplot() +
  scale_fill_manual(values = c("lightgray", "tomato")) +
  labs(title = "Boxplot of Brain Signal by Audio Type", x = "Audio Type", y = "Brain Signal") +
  theme_minimal()

df_neutral <- neuro %>% filter(x2 == "Neutral")
df_emotional <- neuro %>% filter(x2 == "Emotional")

# Histogram: Brain Response for Neutral Audio
ggplot(df_neutral, aes(x = y)) +
  geom_histogram(bins = 25, fill = "lightskyblue", color = "gray30") +
  labs(title = "Distribution of Brain Response - Neutral Audio",
       x = "Brain Signal (y)", y = "Count")

# Histogram: Brain Response for Emotional Audio
ggplot(df_emotional, aes(x = y)) +
  geom_histogram(bins = 20, fill = "orangered", color = "gray30") +
  labs(title = "Brain Response Distribution - Emotional Audio",
       x = "Brain Signal (y)", y = "Count")

# Final Boxplot with Relabeled Axis
ggplot(neuro, aes(x = x2, y = y, fill = x2)) +
  geom_boxplot() +
  scale_fill_manual(values = c("skyblue2", "coral")) +
  labs(title = "Brain Signal Comparison by Audio Emotion",
       x = "Audio Emotion", y = "Brain Signal Value") +
  theme_light()

neuro %>%
  group_by(x2) %>%
  summarise(
    Mean_y = mean(y),
    SD_y = sd(y),
    Mean_x1 = mean(x1),
    SD_x1 = sd(x1)
  )



