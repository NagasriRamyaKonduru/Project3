#Ths the R file where we fit the best model as chosen in the modeling file

# BestFitModel.R
# Plumber API for Diabetes Prediction using Random Forest

library(tidyverse)
library(tidymodels)
library(janitor)
library(yardstick)
library(plumber)
library(ggplot2)

tidymodels_prefer()

# ------------------------------------------------------------------------------
# Load and prepare data
# ------------------------------------------------------------------------------

data <- read_csv("data/diabetes_012_health_indicators_BRFSS2015.csv",
                 show_col_types = FALSE)

# Create binary diabetes variable: 0 = No diabetes, 1/2 = (pre)diabetes
data <- data %>%
  mutate(
    Diabetes_binary = ifelse(Diabetes_012 == 0, 0, 1),
    Diabetes_binary = factor(Diabetes_binary, labels = c("No", "Yes"))
  )

# Convert categorical predictors to factors (same as in Modeling.qmd)
data <- data %>%
  mutate(
    HighBP = factor(HighBP, labels = c("No", "Yes")),
    HighChol = factor(HighChol, labels = c("No", "Yes")),
    CholCheck = factor(CholCheck, labels = c("No", "Yes")),
    Smoker = factor(Smoker, labels = c("No", "Yes")),
    Stroke = factor(Stroke, labels = c("No", "Yes")),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, labels = c("No", "Yes")),
    PhysActivity = factor(PhysActivity, labels = c("No", "Yes")),
    Fruits = factor(Fruits, labels = c("No", "Yes")),
    Veggies = factor(Veggies, labels = c("No", "Yes")),
    HvyAlcoholConsump = factor(HvyAlcoholConsump, labels = c("No", "Yes")),
    AnyHealthcare = factor(AnyHealthcare, labels = c("No", "Yes")),
    NoDocbcCost = factor(NoDocbcCost, labels = c("No", "Yes")),
    DiffWalk = factor(DiffWalk, labels = c("No", "Yes")),
    Sex = factor(Sex, labels = c("Female", "Male")),
    GenHlth = factor(GenHlth),
    Education = factor(Education),
    Income = factor(Income)
  )

# ------------------------------------------------------------------------------
# Recipe and final Random Forest model (winner from Modeling.qmd)
# ------------------------------------------------------------------------------

# Same predictors as in your recipe:
# Diabetes_binary ~ HighBP + HighChol + BMI + Age + Smoker +
#                   HeartDiseaseorAttack + PhysActivity + GenHlth + DiffWalk

diabetes_recipe <- recipe(
  Diabetes_binary ~ HighBP + HighChol + BMI + Age + Smoker +
    HeartDiseaseorAttack + PhysActivity + GenHlth + DiffWalk,
  data = data
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

# Use tuned hyperparameters from Modeling.qmd:
# - mtry = 5
# - trees = 200
# - min_n = 5

rf_spec <- rand_forest(
  mtry = 5,
  trees = 200,
  min_n = 5
) %>%
  set_engine("ranger") %>%
  set_mode("classification")

rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(diabetes_recipe)

# Fit final model to the entire dataset
final_rf_fit <- fit(rf_workflow, data = data)

# Precompute defaults: mean for numeric, most common for factors
default_BMI  <- mean(data$BMI, na.rm = TRUE)
default_Age  <- mean(data$Age, na.rm = TRUE)

default_HighBP  <- names(sort(table(data$HighBP), decreasing = TRUE))[1]
default_HighChol <- names(sort(table(data$HighChol), decreasing = TRUE))[1]
default_Smoker  <- names(sort(table(data$Smoker), decreasing = TRUE))[1]
default_HD      <- names(sort(table(data$HeartDiseaseorAttack), decreasing = TRUE))[1]
default_PhysAct <- names(sort(table(data$PhysActivity), decreasing = TRUE))[1]
default_GenHlth <- names(sort(table(data$GenHlth), decreasing = TRUE))[1]
default_DiffWalk <- names(sort(table(data$DiffWalk), decreasing = TRUE))[1]

# ------------------------------------------------------------------------------
# Plumber API definition
# ------------------------------------------------------------------------------

#* @apiTitle Diabetes Random Forest Prediction API
#* @apiDescription API for predicting diabetes risk using a tuned Random Forest model.

# ------------------------------------------------------------------------------
# 1) Prediction endpoint: /pred
# ------------------------------------------------------------------------------

#* Predict probability of diabetes (Yes/No)
#* @param HighBP High blood pressure (No/Yes)
#* @param HighChol High cholesterol (No/Yes)
#* @param BMI Body Mass Index (numeric)
#* @param Age Age category (numeric; same coding as dataset)
#* @param Smoker Smoker (No/Yes)
#* @param HeartDiseaseorAttack History of heart disease or heart attack (No/Yes)
#* @param PhysActivity Physical activity in past 30 days (No/Yes)
#* @param GenHlth General health (1=Excellent ... 5=Poor)
#* @param DiffWalk Difficulty walking (No/Yes)
#* @get /pred
function(
    HighBP = default_HighBP,
    HighChol = default_HighChol,
    BMI = default_BMI,
    Age = default_Age,
    Smoker = default_Smoker,
    HeartDiseaseorAttack = default_HD,
    PhysActivity = default_PhysAct,
    GenHlth = default_GenHlth,
    DiffWalk = default_DiffWalk
) {
  
  new_data <- tibble(
    HighBP = HighBP,
    HighChol = HighChol,
    BMI = as.numeric(BMI),
    Age = as.numeric(Age),
    Smoker = Smoker,
    HeartDiseaseorAttack = HeartDiseaseorAttack,
    PhysActivity = PhysActivity,
    GenHlth = GenHlth,
    DiffWalk = DiffWalk
  ) %>%
    mutate(
      HighBP = factor(HighBP, levels = levels(data$HighBP)),
      HighChol = factor(HighChol, levels = levels(data$HighChol)),
      Smoker = factor(Smoker, levels = levels(data$Smoker)),
      HeartDiseaseorAttack = factor(HeartDiseaseorAttack,
                                    levels = levels(data$HeartDiseaseorAttack)),
      PhysActivity = factor(PhysActivity, levels = levels(data$PhysActivity)),
      GenHlth = factor(GenHlth, levels = levels(data$GenHlth)),
      DiffWalk = factor(DiffWalk, levels = levels(data$DiffWalk))
    )
  
  probs <- predict(final_rf_fit, new_data, type = "prob")
  
  list(
    input = new_data,
    prediction = list(
      prob_No = probs$.pred_No[1],
      prob_Yes = probs$.pred_Yes[1]
    )
  )
}

# Example calls to copy–paste into R (as required by assignment):
# httr::GET("http://localhost:8000/pred")
# httr::GET("http://localhost:8000/pred",
#           query = list(HighBP = "Yes", HighChol = "Yes",
#                        BMI = 32, Age = 55, Smoker = "Yes",
#                        HeartDiseaseorAttack = "No",
#                        PhysActivity = "No", GenHlth = "3",
#                        DiffWalk = "No"))
# httr::GET("http://localhost:8000/pred",
#           query = list(HighBP = "No", HighChol = "No",
#                        BMI = 24, Age = 35, Smoker = "No",
#                        HeartDiseaseorAttack = "No",
#                        PhysActivity = "Yes", GenHlth = "2",
#                        DiffWalk = "No"))

# ------------------------------------------------------------------------------
# 2) Info endpoint: /info
# ------------------------------------------------------------------------------

#* Return basic info about the API
#* @get /info
function() {
  list(
    name = "Nagasri Ramya Konduru",
    github_pages_url = "https://NagasriRamyaKonduru.github.io/Project3/EDA.html"
  )
}


# ------------------------------------------------------------------------------
# 3) Confusion matrix plot endpoint: /confusion
# ------------------------------------------------------------------------------

#* Plot confusion matrix for model on full dataset
#* @get /confusion
#* @serializer png
function() {
  
  preds <- predict(final_rf_fit, data, type = "prob") %>%
    mutate(
      pred_class = if_else(.pred_Yes >= 0.5, "Yes", "No"),
      pred_class = factor(pred_class, levels = levels(data$Diabetes_binary))
    ) %>%
    bind_cols(truth = data$Diabetes_binary)
  
  cm <- yardstick::conf_mat(preds, truth = truth, estimate = pred_class)
  
  autoplot(cm)
}
