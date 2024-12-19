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
library(auto_arima)
library(plotly)
library(prophet)

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")
#setwd("~/Kaggle/Forecast")

fulltrain <- vroom("./train.csv")
fulltest <- vroom("./test.csv")

train <- fulltrain %>% filter(store == 5, item == 25)
test <- fulltest %>% filter(store == 5, item == 25)

cv_split <- time_series_split(train, assess="3 months", cumulative=TRUE)
cv_split %>%  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model <- prophet_reg() %>% 
  set_engine('prophet') %>% 
  fit(sales ~ date, data=training(cv_split))

cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))

cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

p1 <- cv_results %>% modeltime_forecast(new_data = testing(cv_split),
                                        actual_data = training(cv_split)) %>% 
  plot_modeltime_forecast(.interactive = FALSE, .title = 'CV Preds: Store 5 Item 25')

full_fit <- cv_results %>% 
  modeltime_refit(data = train)

p2 <- full_fit %>% 
  modeltime_forecast(new_data = test,
                     actual_data = train) %>% 
  plot_modeltime_forecast(.interactive=FALSE, .title = '3 Month Forecast: Store 5 Item 25')

## Store-Item Combo 2:

train <- fulltrain %>% filter(store == 2, item == 15)
test <- fulltest %>% filter(store == 2, item == 15)

cv_split <- time_series_split(train, assess="3 months", cumulative=TRUE)
cv_split %>%  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(date, sales, .interactive=FALSE)

prophet_model <- prophet_reg() %>% 
  set_engine('prophet') %>% 
  fit(sales ~ date, data=training(cv_split))

cv_results <- modeltime_calibrate(prophet_model, new_data = testing(cv_split))

cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(
    .interactive = FALSE
  )

p3 <- cv_results %>% modeltime_forecast(new_data = testing(cv_split),
                                        actual_data = training(cv_split)) %>% 
  plot_modeltime_forecast(.interactive = FALSE, .title = 'CV Preds: Store 2 Item 15')

full_fit <- cv_results %>% 
  modeltime_refit(data = train)

p4 <- full_fit %>% 
  modeltime_forecast(new_data = test,
                     actual_data = train) %>% 
  plot_modeltime_forecast(.interactive=FALSE, .title = '3 Month Forecast: Store 2 Item 15')

plotly::subplot(p1, p3, p2, p4, nrows = 2, margin = 0.1) %>% layout(showlegend = FALSE, title = '', annotations = list( 
  list( 
    x = 0.2,  
    y = 1.0,  
    text = "CV Preds: Store 5 Item 25",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.8,  
    y = 1,  
    text = "CV Preds: Store 2 Item 15",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),  
  list( 
    x = 0.2,  
    y = 0.4,  
    text = "3 Month Forecast: Store 5 Item 25",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  ),
  list( 
    x = 0.8,  
    y = 0.4,  
    text = "3 Month Forecast: Store 2 Item 15",  
    xref = "paper",  
    yref = "paper",  
    xanchor = "center",  
    yanchor = "bottom",  
    showarrow = FALSE 
  )))

