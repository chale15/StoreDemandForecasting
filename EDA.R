library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(workflows)
library(forecast)
library(patchwork)

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

