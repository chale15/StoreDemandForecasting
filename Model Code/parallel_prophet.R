library(modeltime)
library(timetk)
library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(workflows)
library(forecast)
library(patchwork)
library(ranger)
library(plotly)
library(prophet)
library(parallel)  # Load parallel library

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")

# Load data
train <- vroom("./train.csv")
test <- vroom("./test.csv")

nStores <- max(train$store)
nItems <- max(train$item)

# Define the function that will run for each store and item
process_store_item <- function(s, i, train, test) {
  storeItemTrain <- train %>% filter(store == s, item == i)
  storeItemTest <- test %>% filter(store == s, item == i)
  
  # Cross-validation split
  cv_split <- time_series_split(storeItemTrain, assess = "3 months", cumulative = TRUE)
  
  # Fit the Prophet model
  prophet_model <- prophet_reg() %>% 
    set_engine('prophet') %>% 
    fit(sales ~ date, data = training(cv_split))
  
  # Calibrate the model
  cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))
  
  # Refit the model on the full training data
  full_fit <- cv_results %>% 
    modeltime_refit(data = storeItemTrain)
  
  # Make predictions
  preds <- full_fit %>%
    modeltime_forecast(new_data = storeItemTest, actual_data = storeItemTrain) %>%
    filter(!is.na(.model_id)) %>%
    mutate(id = storeItemTest$id) %>%
    select(id, .value) %>%
    rename(sales = .value)
  
  # Return the predictions
  return(preds)
}

# Create a list of all store-item combinations
store_item_combinations <- expand.grid(store = 1:nStores, item = 1:nItems)

# Set up parallel cluster (number of cores to use)
cl <- makeCluster(detectCores() - 1)  # Using one less core than available
clusterExport(cl, c("train", "test", "process_store_item", "time_series_split", "prophet_reg", "modeltime_calibrate", "modeltime_refit", "modeltime_forecast"))

# Run the process in parallel
all_preds <- parLapply(cl, 1:nrow(store_item_combinations), function(idx) {
  s <- store_item_combinations$store[idx]
  i <- store_item_combinations$item[idx]
  process_store_item(s, i, train, test)
})

# Stop the cluster after computation
stopCluster(cl)

# Combine the results from all parallel jobs into one data frame
all_preds_combined <- do.call(bind_rows, all_preds)

# Write predictions to file
vroom_write(all_preds_combined, file = './Submissions/ProphetKernel1.csv')
