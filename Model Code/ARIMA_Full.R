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

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")
#setwd("~/Kaggle/Forecast")

train <- vroom("./train.csv")
test <- vroom("./test.csv")

nStores <- max(train$store)
nItems <- max(train$item)

for(s in 1:nStores){
  for(i in 1:nItems){
    storeItemTrain <- train %>% filter(store==s, item==i)
    storeItemTest <- test %>% filter(store==s, item==i)

    cv_split <- time_series_split(storeItemTrain, assess="3 months", cumulative=TRUE)
    
    my_recipe <- recipe(sales~., data = train) %>% 
      step_date(date, features=c('doy', 'decimal')) %>% 
      step_date(date, features = c('dow', 'month'), label = FALSE) %>% 
      step_range(date_doy, min=0, max=pi) %>%
      step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
      step_lag(date, lag=7) %>% 
      step_mutate_at(c(date_dow, date_month), fn = factor) %>% 
      step_rm(store, item, date_doy)
    
    arima_model <- arima_reg(seasonal_period = 365,
                             non_seasonal_ar = 5,
                             non_seasonal_ma = 5,
                             seasonal_ar = 2,
                             seasonal_ma = 2,
                             non_seasonal_differences = 2,
                             seasonal_differences = 2) %>% 
      set_engine("auto_arima")
    
    arima_workflow <- workflow() %>% 
      add_model(arima_model) %>% 
      add_recipe(my_recipe) %>% 
      fit(data=training(cv_split))
    
    cv_results <- modeltime_calibrate(arima_workflow, new_data = testing(cv_split))
    
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
