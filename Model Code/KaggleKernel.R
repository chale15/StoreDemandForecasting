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

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")

train <- vroom("./train.csv")
test <- vroom("./test.csv")

nStores <- max(train$store)
nItems <- max(train$item)

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>% filter(store==s, item==i)
    storeItemTest <- test %>% filter(store==s, item==i)

    cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative=TRUE)
    
    prophet_model <- prophet_reg() %>% 
      set_engine('prophet') %>% 
      fit(sales ~ date, data=training(cv_split))
    
    cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))
    
    full_fit <- cv_results %>% 
      modeltime_refit(data = storeItemTrain)
    
    preds <- full_fit %>%
      modeltime_forecast(new_data = storeItemTest, actual_data = storeItemTrain) %>%
      filter(!is.na(.model_id)) %>%
      mutate(id=storeItemTest$id) %>%
      select(id, .value) %>%
      rename(sales=.value)
    
    if(s==1 & i==1){
      all_preds <- preds} 
    else {all_preds <- bind_rows(all_preds, preds)}
  }
}

vroom_write(all_preds, file = './ProphetKernel1.csv')
