# --- Load essential libraries ---
library(tidyverse)   # Includes dplyr, ggplot2, readr, etc.
library(jsonlite)    # For working with JSON (e.g., kaggle.json)
library(httr)        # Optional for advanced API access
library(readr)       # For reading CSV
library(ggplot2)     # For plotting

Sys.setenv(KAGGLE_USERNAME = fromJSON(".kaggle/kaggle.json")$username)
Sys.setenv(KAGGLE_KEY = fromJSON(".kaggle/kaggle.json")$key)

# --- Load the dataset from the extracted CSV file ---
data <- read_csv("student_habits_performance.csv")

# --- Initial Data State ---
# Check structure and basic statistics
str(data)
summary(data)
colSums(is.na(data))  # Check missing values

# --- Data Cleaning & Preprocessing ---

# 1. Convert relevant categorical variables to factor
data <- data %>%
  mutate(
    gender = as.factor(gender),
    part_time_job = as.factor(part_time_job),
    parental_education_level = as.factor(parental_education_level),
    internet_quality = as.factor(internet_quality),
    extracurricular_participation = as.factor(extracurricular_participation)
  )

# 2. Remove duplicates
data <- data[!duplicated(data), ]
nrow(data)  # Confirm no duplicates remain

# 3. Categorize exam_score into performance_level with 6 categories
data <- data %>%
  mutate(performance_level = case_when(
    exam_score >= 90 ~ "Excellent",
    exam_score >= 80 & exam_score < 90 ~ "High",
    exam_score >= 70 & exam_score < 80 ~ "Satisfactory",
    exam_score >= 60 & exam_score < 70 ~ "Medium",
    exam_score >= 50 & exam_score < 60 ~ "Fair",
    exam_score < 50 ~ "Low"
  ))
data$performance_level <- as.factor(data$performance_level)

# 4. Create derived variable 'screen_time' (social media + Netflix)
data <- data %>%
  mutate(screen_time = social_media_hours + netflix_hours)

# 5. Outlier removal for screen_time > 12 hours
ggplot(data, aes(y = screen_time)) +
  geom_boxplot(fill = "deepskyblue3", outlier.color = "red") +
  labs(title = "Boxplot of Total Screen Time", y = "Hours") +
  theme_minimal()

data <- data[data$screen_time < 12, ]

# Final checks
str(data)
summary(data)
head(data)

# Save cleaned dataset
write_csv(data, "cleaned_student_habits_performance_data.csv")
