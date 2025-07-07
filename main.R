library("tidyverse")

# -----------------------------------------------------------------------------
# Loading data and setup
# -----------------------------------------------------------------------------
data <- read.csv("heart.csv")
head(data)
str(data)
colnames(data)

# Meaning of Features
  # Age: age of the patient [years]
  # Sex: sex of the patient [M: Male, F: Female]
  # ChestPainType: chest pain type [TA: Typical Angina, ATA: Atypical Angina, 
  #   NAP: Non-Anginal Pain, ASY: Asymptomatic]
  # RestingBP: resting blood pressure [mm Hg]
  # Cholesterol: serum cholesterol [mm/dl]
  # FastingBS: fasting blood sugar [1: if FastingBS > 120 mg/dl, 0: otherwise]
  # RestingECG: resting electrocardiogram results [Normal: Normal, 
  #   ST: having ST-T wave abnormality (T wave inversions and/or ST elevation 
  #   or depression of > 0.05 mV), LVH: showing probable or definite left ventricular hypertrophy 
  #   by Estes' criteria]
  # MaxHR: maximum heart rate achieved [Numeric value between 60 and 202]
  # ExerciseAngina: exercise-induced angina [Y: Yes, N: No]
  # Oldpeak: oldpeak = ST [Numeric value measured in depression]
  # ST_Slope: the slope of the peak exercise ST segment [Up: upsloping, 
  #   Flat: flat, Down: downsloping]
  # HeartDisease: output class [1: heart disease, 0: Normal]


# -----------------------------------------------------------------------------
# Data Cleaning
# -----------------------------------------------------------------------------

# Check for NA values for each feature
colSums(is.na(data))
# Data is clean from NA

# Check for duplicates
nrow(data) == nrow(distinct(data))
# Data has no duplicates

# -----------------------------------------------------------------------------
# Data Analysis
# -----------------------------------------------------------------------------
# Correlation for numerical features
numerical_features <- c("Age", "RestingBP", "Cholesterol", "FastingBS", "MaxHR", "Oldpeak")
cor(data[, numerical_features])
# Features are fairly correlated with each other. There is no strong correlation in the dataset
str(data)
# Pairplot of numerical features
pairs(data[, numerical_features])
# Plots do not tell us important information about relationship, we can see only correlationship between
# Age and MaxHR; Age and Resting BP

# Predicted Feature analysis
ggplot(data = data, aes(x = factor(HeartDisease), fill = Sex))+
  geom_bar()+
  labs(
    title = "Heart Disease by Sex",
    x = "Heart Disease (0: normal, 1: heart disease)",
    y = "Count"
  )
# Predicted feature is distributed normally;
# Sex feature is not; Majority of patients are males;
# Also we can see that females have lower chance of heart disease by just looking at the graph


# -----------------------------------------------------------------------------
# Categorical data
# -----------------------------------------------------------------------------

# Now, let's get deeper into the insights about data through plotting
# I made a custom function to present a categorical data 
get_dist_of_cat <- function(
    feature,
    title,
    x,
    y,
    fill_txt = "0: normal, 1: heart disease",
    fill = factor(data$HeartDisease)) {
  
  ggplot(data = data, aes(x = feature, fill = fill))+
    geom_bar()+
    labs(
      title = title,
      x = x,
      y = "Count",
      fill = fill_txt
    )
}

# -----------------------------------------------------------------------------
# ChestPain ~ HeartDisease
# -----------------------------------------------------------------------------
get_dist_of_cat(
  feature = data$ChestPainType,
  title = "Chest Pain by Heart Disease",
  x = "TA: Typical Angina, ATA: Atypical Angina, NAP: Non-Anginal Pain, ASY: Asymptomatic"
)
# Most of the people have an ASY (Asymptomatic) chest pain
# A big majority of those who experience it, have a heart disease
# This is very important feature for detecting a heart disease


# -----------------------------------------------------------------------------
# FastingBS ~ HeartDisease
# -----------------------------------------------------------------------------
get_dist_of_cat(
  feature = factor(data$FastingBS),
  title = "Fasting Blood Sugar by Heart Disease",
  x = "1: if FastingBS > 120 mg/dl, 0: otherwise"
)
# Here, we can see that less people have > 120mg/dl, 
# however the chance of having a heart disease
# is very high compared to those who have < 120mg/dl

# -----------------------------------------------------------------------------
# RestingECG ~ HeartDisease
# -----------------------------------------------------------------------------
get_dist_of_cat(
  feature = data$RestingECG,
  title = "Resting Electrocardiogram by Heart Disease",
  x = "Resting Electrocardiogram"
)
# Here, we can see that type of Resting Electrocardiogram does not contribute 
# a big effect on the chances of having a heart disease, however 
# individuals with ST type have a slightly higher chance of
# getting disease rather than other types.


# -----------------------------------------------------------------------------
# Exercise-Induced Angina ~ HeartDisease
# -----------------------------------------------------------------------------
get_dist_of_cat(
  feature = data$ExerciseAngina,
  title = "Exercise-Induced Angina by Heart Disease",
  x = "exercise-induced angina [Y: Yes, N: No]"
)
# Here, we can see that individuals who experienced Exercise-Induced Angina 
# have higher chance of getting a heart disease, even though there are 
# less of them

# -----------------------------------------------------------------------------
# ST_Slope ~ HeartDisease
# -----------------------------------------------------------------------------
get_dist_of_cat(
  feature = data$ST_Slope,
  title = "ST Slope by Heart Disease",
  x = "Up: upsloping, Flat: flat, Down: downsloping"
)
# Here, we can see that individuals with Up type of ST Slope have
# lower chance of getting heart disease. 
# Flat and Down types indicate that the chances of getting a heart disease are high

# -----------------------------------------------------------------------------
# Data type covertions
# -----------------------------------------------------------------------------

# Now, let's convert character features to numeric factors
# After, let's see relationships among them
data$Sex <- ifelse(data$Sex == "M", 1, 0)
data$ChestPainType <- as.numeric(factor(data$ChestPainType))
data$RestingECG <- as.numeric(factor(data$RestingECG))
data$ExerciseAngina <- ifelse(data$ExerciseAngina == "Y", 1, 0)
data$ST_Slope <- as.numeric(factor(data$ST_Slope))
str(data)
cor(data)  
# Here, we can see that HeartDisease have a higher correlation 
# with ExerciseAngina and ST_Slope features
# These might be useful features for our model

data$HeartDisease <- factor(data$HeartDisease)
# Convert predicted column to factor for accuracy analysis

# -----------------------------------------------------------------------------
# Numerical Features Distribution Analysis
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Cholesterol
# -----------------------------------------------------------------------------
# Let's see the distribution of the Cholesterol feature
hist(data$Cholesterol)
# Here, we can see that it can be adjusted using log()

nrow(data[data$Cholesterol <= 0, ])
# 172 records contain 0 Cholesterol level
# According to the science, human being suppose to have more than 0
# So, this means that we have a missing data

data <- data[data$Cholesterol > 0, ]
# Removing the data is not a solution, 172 records would be missing 
# This is a big part of our small dataset

# Looks better now, but still needs a fix;
# Let's use log() to distribute data equally

data$Cholesterol <- log(data$Cholesterol)
hist(data$Cholesterol)
# We can see that data is distributed normally and is in a good range


# -----------------------------------------------------------------------------
# MaxHR
# -----------------------------------------------------------------------------
hist(data$MaxHR)
nrow(data[data$MaxHR <= 0, ])
# Data is distributed normally; 0s are not present; We can continue
# No changes needed

# -----------------------------------------------------------------------------
# RestingBP
# -----------------------------------------------------------------------------
hist(data$RestingBP)
# Data distributed normally except few records that contain 0 value
# Let's see the records

data[data$RestingBP <= 0, ]
# As we can see, features: RestingBP, Cholesterol, FastingBS and ExerciseAngina equal to 0 in this record
# I can count it as an outlier, we can get rid of it
data <- data[data$RestingBP > 0, ]
hist(data$RestingBP)

# -----------------------------------------------------------------------------
# Age
# -----------------------------------------------------------------------------
hist(data$Age)
# Data distributed equally normally; No 0s present
# No changes needed

# -----------------------------------------------------------------------------
# Oldpeak
# -----------------------------------------------------------------------------
hist(data$Oldpeak)
nrow(data[data$Oldpeak <= 0, ])
# 381 rows are less or equal to 0.
# It is the feature by itself, that is not a mistake;
# Moreover, data distributed normally, so we can move on


# -----------------------------------------------------------------------------
# Train/Test split
# -----------------------------------------------------------------------------
set.seed(41)
train_data <- NA
test_data <- NA

get_train_test_data <- function() {
  train_index <- sample(x = nrow(data), size = 0.8*nrow(data))
  train_data <<- data[train_index, ]
  test_data <<- data[-train_index, ]
  
}
get_train_test_data()

# -----------------------------------------------------------------------------
# Model Building (all features)
# -----------------------------------------------------------------------------

model <- glm(
  formula = HeartDisease ~ .,
  family = binomial, 
  data = train_data)

summary(model)

# -----------------------------------------------------------------------------
# Accuracy of the model
# -----------------------------------------------------------------------------
library(caret)
# I created a function that can be reused for other models as well,
# it calculates Precision, Recall and F1 score of the model
# Using it, I can measure how well model performs
get_accuracy_scores <- function(predicted, actual) {
  confusion_matrix <- confusionMatrix(predicted, actual)
  confusion_matrix_tbl <- confusion_matrix$table
  
  colnames(confusion_matrix_tbl) <- c("Predicted.True", "Predicted.False")
  rownames(confusion_matrix_tbl) <- c("Actual.True", "Actual.False")
  confusion_matrix_tbl
  
  TP <- confusion_matrix_tbl["Actual.True", ]["Predicted.True"]
  FP <- confusion_matrix_tbl["Actual.False", ]["Predicted.True"]  
  FN <- confusion_matrix_tbl["Actual.True", ]["Predicted.False"]
  
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  F1 = 2 * ((precision * recall) / (precision + recall))
  list(Precision = precision, Recall = recall, F1 = F1)
}

predicted <- predict(model, test_data)
predicted <- factor(ifelse(predicted > 0.5, 1, 0))
get_accuracy_scores(predicted = predicted, actual = test_data$HeartDisease)
# Here, using the custom function we can see that f1 score for the model is 0.850 ~ 85%
# Now, I will choose significant features for the model and see if result will change

# -----------------------------------------------------------------------------
# Model Building (Significant features)
# -----------------------------------------------------------------------------

model_selected_features <- glm(
  formula = HeartDisease ~ Age+Sex+ChestPainType+Cholesterol+ExerciseAngina+Oldpeak+ST_Slope,
  family = binomial, 
  data = train_data)
# Age feature is not significant for the model, so i do not include it
summary(model_selected_features)

predicted <- predict(model_selected_features, test_data)
predicted <- factor(ifelse(predicted > 0.5, 1, 0))
get_accuracy_scores(predicted = predicted, actual = test_data$HeartDisease)

# As we can see, model performed in better way, F1 score is 0.86 ~ 86%, previous result: 85%
# Let's try to get better result using feature engineering


# -----------------------------------------------------------------------------
# Model (Crafted Features)
# -----------------------------------------------------------------------------

data <- data %>% 
  # MaxHR
  mutate(Div_HR_Age = MaxHR / Age) %>% 
  mutate(Div_HR_Cholesterol = MaxHR / Cholesterol) %>%
  mutate(Div_HR_RestingBP = MaxHR / RestingBP) %>% 
  mutate(Prod_HR_ST_Slope = MaxHR * ST_Slope) %>% 
  mutate(Prod_HR_Ch_P = MaxHR * ChestPainType) %>% 
  mutate(Prod_HR_RestingECG = MaxHR * RestingECG) %>% 
  
  # Cholesterol
  mutate(Prod_Chol_Ch_P = Cholesterol * ChestPainType) %>% 
  mutate(Prod_Chol_RestingECG = Cholesterol * RestingECG) %>% 
  mutate(Prod_Chol_ST_Slope = Cholesterol * ST_Slope) %>% 
  
  # Age
  mutate(Div_Age_Chol = Age / Cholesterol) %>% 
  mutate(Prod_Age_ST_Slope = Age * ST_Slope) %>% 
  mutate(Prod_Age_Resting_ECG = Age * RestingECG) %>%
  mutate(Prod_Age_Ch_P = Age * ChestPainType) %>% 
  mutate(AgeGroup = case_when(
    Age < 18 ~ 0, # Child = 0
    Age >= 18 & Age < 60 ~ 1, # Adult = 1
    Age >= 60 ~ 2, # Senior = 2
    T ~ NA)) %>% 
  
  # Oldpeak
  mutate(SuqaredOldpeak = ifelse(Oldpeak > 0, Oldpeak^2, 1)) %>%
  mutate(OldPeakGroup = case_when(
    Oldpeak < 1 ~ 1, # OldPeak is less than 1
    Oldpeak >= 1 & Oldpeak < 3 ~ 2, # OldPeak between 1 and 3
    Oldpeak >= 3 ~ 3, # OldPeak more than 3
    T ~ NA))


get_train_test_data()
model_crafted_features_all <- glm(
  formula = HeartDisease ~ Age+Sex+ChestPainType+Cholesterol+ExerciseAngina+Oldpeak+ST_Slope+Prod_HR_ST_Slope+Prod_Age_ST_Slope+AgeGroup,
  family = binomial, 
  data = train_data)

summary(model_crafted_features_all)

predicted <- predict(model_crafted_features_all, test_data)
predicted <- factor(ifelse(predicted > 0.5, 1, 0))
get_accuracy_scores(predicted = predicted, actual = factor(test_data$HeartDisease))
# Model performed better than previous models, F1 score: 0.852 ~ 85.2% best result: 82%
# Summary of the model tells that features are not significant, however result is better
# Result is ok, but not what I expected. Future improvements needed.
