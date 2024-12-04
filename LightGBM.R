library(tidyverse)
library(tidymodels)
library(modeltime)
library(timetk)
library(vroom)
library(embed)
library(bonsai)
library(lightgbm)

item <- vroom("/kaggle/input/demand-forecasting-kernels-only/train.csv")
itemTest <- vroom("/kaggle/input/demand-forecasting-kernels-only/test.csv")

nStores <- max(item$store)
nItems <- max(item$item)

item_recipe <- recipe(sales~., data=item) %>%
  step_date(date, features=c("dow", "month", "decimal", "doy", "year")) %>%
  step_range(date_doy, min=0, max=pi) %>%
  step_mutate(sinDOY=sin(date_doy), cosDOY=cos(date_doy)) %>%
  step_lencode_mixed(all_nominal_predictors(), outcome=vars(sales)) %>%
  step_rm(date, item, store) %>%
  step_normalize(all_numeric_predictors())

boosted_model <- boost_tree(tree_depth=2,
                            trees=1000,
                            learn_rate=0.01) %>%
  set_engine("lightgbm") %>%
  set_mode("regression")

boost_wf <- workflow() %>%
  add_recipe(item_recipe) %>%
  add_model(boosted_model)

for(s in 1:nStores){
  for(i in 1:nItems){
    train <- item %>% filter(store==s, item==i)
    test <- itemTest %>% filter(store==s, item==i)
    
    fitted_wf <- boost_wf %>% fit(data=train)
    
    preds <- predict(fitted_wf, new_data=test) %>%
      bind_cols(test) %>%
      rename(sales=.pred) %>%
      select(id, sales)
    
    if(s==1 && i==1){
      all_preds <- preds
    }
    else{
      all_preds <- bind_rows(all_preds,preds)
    } 
  }
}

vroom_write(x=all_preds, "./submission.csv", delim=",")