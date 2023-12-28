#Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(corrplot)

# Data loading
mydata <- read.csv("~/Downloads/Health Sciences Data File New (project).csv") %>% 
  select(-where(~all(is.na(.))))

# Count non-empty values per column
non_empty_counts <- colSums(!is.na(mydata))

# Filter columns with more than 6000 non-empty values
selected_columns <- names(non_empty_counts[non_empty_counts > 6000])

# Save new data
mydata <- mydata %>% select(all_of(selected_columns))

# Compute the correlation matrix and plot
correlation_matrix <- cor(mydata %>% select_if(is.numeric))
corrplot(correlation_matrix, method = "color", type = "upper", order = "hclust")

# Variable selection rationale
selected_variables <- c("Ht", "RF.2", "FF", "RGM", "LGM", "VC", "Stages", "PL.2", "Wt", "HR.2", "HR.rest")
selected_variables_rationale <- correlation_matrix["FFTotal", selected_variables]
print("Correlation of selected variables with FFTotal:")
print(selected_variables_rationale)

# Variable selection rationale
selected_variables <- c("Ht", "RF.2", "FF", "RGM", "LGM", "VC", "Stages", "PL.2", "Wt", "HR.2", "HR.rest")
selected_variables_rationale <- correlation_matrix["FFTotal", selected_variables]
print("Correlation of selected variables with FFTotal:")
print(selected_variables_rationale)

# Individual scatter plots with regression lines
ggplot(data = mydata, aes(x = Ht, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Height", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = RF.2, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "RF.2", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = FF, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "FF", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = RGM, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "RGM", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = LGM, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "LGM", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = VC, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "VC", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = Stages, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Stages", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = PL.2, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "PL.2", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = Wt, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Weight", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = HR.2, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "HR.2", y = "Total Fitness Factor")

ggplot(data = mydata, aes(x = HR.rest, y = FFTotal)) +
  geom_point() + geom_smooth(method = "lm", se = FALSE) + labs(x = "Resting Heart Rate", y = "Total Fitness Factor")

# Removal of outliers for each variable
remove_outliers_and_plot <- function(data, variable) {
  # Calculate IQR
  variable_iqr <- IQR(data[[variable]])
  
  # Calculate lower and upper bounds
  lower_bound <- quantile(data[[variable]])[2] - 1.5 * variable_iqr
  upper_bound <- quantile(data[[variable]])[4] + 1.5 * variable_iqr
  
  # Filter the data
  filtered_data <- data %>% filter(.data[[variable]] > lower_bound & .data[[variable]] < upper_bound)
  
  # Save the filtered data to a new variable
  assign(paste0("filtered_", variable, "_data"), filtered_data, envir  = .GlobalEnv)
  
  # Plot the filtered data
  ggplot(data = filtered_data, aes(x = .data[[variable]], y = FFTotal)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    labs(x = variable, y = "Total Fitness Factor")
  
  # Return the filtered data
  return(filtered_data)
}

# Create a list to store filtered datasets
filtered_datasets <- list()

# Loop through selected variables and filter data
for (variable in selected_variables) {
  filtered_data <- remove_outliers_and_plot(mydata, variable)
  if (nrow(filtered_data) > 0) {
    filtered_datasets[[variable]] <- filtered_data
  }
}

final_filtered_data <- Reduce(inner_join, filtered_datasets)

# Split data into train and test subsets
train_size <- nrow(mydata) / 2
train_data <- mydata[1:train_size, ]
test_data <- mydata[(train_size + 1):nrow(mydata), ]

# Variable Transformations
# BMI
train_data <- train_data %>%
  mutate(BMI = Wt / Ht^2)

# Power at Different Stages
train_data <- train_data %>%
  mutate(PL1 = PL.1, PL2 = PL.2, PowerChange = PL2 - PL1)

# Heart Rate Change
train_data <- train_data %>%
  mutate(HR1 = HR.1, HR2 = HR.2, HeartRateChange = HR2 - HR1)

# Linear regression models with transformed variables
model_1 <- lm(FFTotal ~ FF + RF.2 + FF + BMI + PowerChange + HeartRateChange, data = train_data)
model_2 <- lm(FFTotal ~ RGM + LGM + FF + BMI + PowerChange + HeartRateChange, data = train_data)
model_3 <- lm(FFTotal ~ RF.2 + LGM + FF + BMI + PowerChange + HeartRateChange, data = train_data)
model_4 <- lm(FFTotal ~ RGM + FF + FF + BMI + PowerChange + HeartRateChange, data = train_data)
model_5 <- lm(FFTotal ~ RF.2 + FF + BMI + PowerChange + HeartRateChange, data = train_data)

# Find best R^2 model
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
summary(model_5)

install.packages("Metrics")
library(Metrics)
test_data <- test_data %>% mutate(BMI = Wt / Ht^2, PL1 = PL.1, PL2 = PL.2, PowerChange = PL2 - PL1, HR1 = HR.1, HR2 = HR.2, HeartRateChange = HR2 - HR1)
predicted_values <- predict(model_3, newdata = test_data, interval = "confidence", level= 0.99)
rmse_value <- rmse(test_data$FFTotal, predicted_values)
cat("RMSE for the Best Model on Test Data:", rmse_value, "\n")