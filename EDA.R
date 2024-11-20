library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(workflows)
library(forecast)
library(patchwork)
library(ranger)

#Read Data

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")
#setwd("~/Kaggle/Forecast")

train <- vroom("./train.csv")
test <- vroom("./test.csv")

item1 <- train %>% filter(store == 1, item == 25)
plot1 <- item1 %>%
  ggplot(mapping = aes(x=date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = 'Sales Over Time: Store 1 Item 25', 
       x = 'Date', y = 'Sales')

acf_month_1 <- item1 %>% pull(sales) %>% forecast::ggAcf(.)
acf_year_1 <- item1 %>% pull(sales) %>% forecast::ggAcf(., lag.max = 2*365)

item2 <- train %>% filter(store == 5, item == 25)
plot2 <- item2 %>%
  ggplot(mapping = aes(x=date, y = sales)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  labs(title = 'Sales Over Time: Store 5 Item 25', 
       x = 'Date', y = 'Sales')

acf_month_2 <- item2 %>% pull(sales) %>% forecast::ggAcf(.)
acf_year_2 <- item2 %>% pull(sales) %>% forecast::ggAcf(., lag.max = 2*365)


(plot1 + acf_month_1 + acf_year_1)/(plot2 + acf_month_2 + acf_year_2)

my_recipe <- recipe(sales~., data = item1) %>% 
  step_date(date, features=c('doy', 'dow', 'month', 'year', 'decimal')) %>% 
#  step_mutate(sat_after_payday = ifelse((date_dow == 7) && (date_doy %in% c(2,3,4,5,6,7, 16, 17, 18, 19, 20, 21, 22)), 1, 0)) %>% 
#  step_range(date_doy, min=0, max=pi) %>% 
#  step_range(date_dow, min=0, max=pi) %>% 
#  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy),
#              sinDOW = sin(date_dow), cosDOW = cos(date_dow)) %>% 
  step_lag(date, lag=7) %>% 
  step_lag(date, lag=365) #%>% 
#  step_mutate_at(c(date_dow, date_doy, sat_after_payday), fn = factor)

my_recipe1 <- recipe(sales~., data = item1) %>% 
  step_date(date, features='doy') %>% 
  step_date(date, features='dow') %>% 
  step_date(date, features = 'decimal') %>% 
  #  step_mutate(sat_after_payday = ifelse((date_dow == 7) && (date_doy %in% c(2,3,4,5,6,7, 16, 17, 18, 19, 20, 21, 22)), 1, 0)) %>% 
  step_range(date_doy, min=0, max=pi) %>% 
  step_range(date_dow, min=0, max=pi) %>% 
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>% 
  step_mutate(sinDOW = sin(date_dow), cosDOW = cos(date_dow)) %>% 
  step_lag(date, lag=7) %>% 
  step_lag(date, lag=365) %>% 
  step_mutate_at(c(date_dow, date_doy), fn = factor)

rf_model <- rand_forest(mtry=tune(),
                           min_n=tune(),
                           trees=500) %>% 
  set_mode("classification") %>% 
  set_engine("ranger")

workflow_2 <- workflow() %>% 
  add_model(rf_model) %>% 
  add_recipe(my_recipe1)

tuning_grid <- grid_regular(mtry(range=c(1, 9)), min_n(), levels = 3)
folds <- vfold_cv(train, v = 5, repeats=1)

workflow_2 <- workflow() %>% 
  add_recipe(my_recipe) %>% 
  add_model(rt_model)

cv_results <- workflow_2 %>% 
  tune_grid(resamples=folds,
            grid=tuning_param_grid,
            metrics=metric_set(smape))

#best_tune <- cv_results %>% show_best(metric="smape")

cv_results %>% show_best(metric="smape")

