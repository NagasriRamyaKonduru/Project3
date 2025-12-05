.libPaths("/usr/local/lib/R/site-library")
library(plumber)
library(ranger)

# Load model
rf_model <- readRDS("final_rf.rds")

#* @get /pred
#* @param BMI
#* @param Age
function(BMI = 25, Age = 40) {
  newdata <- data.frame(
    BMI = as.numeric(BMI),
    Age = as.numeric(Age)
  )
  
  preds <- predict(rf_model, newdata, type="response")
  
  list(
    prob_no = preds$predictions[, "No"],
    prob_yes = preds$predictions[, "Yes"]
  )
}

# Start plumber
pr <- plumber::plumb("api.R")
pr$run(host="0.0.0.0", port=8000)
