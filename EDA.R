library(tidyverse)
library(tidymodels)
library(embed)
library(vroom)
library(workflows)

#Read Data

setwd("~/Desktop/Fall 2024/Stat 348/GitHubRepos/Store_Forecasting_Kaggle/")
#setwd("~/Kaggle/Forecast")

train <- vroom("./train.csv")
test <- vroom("./test.csv")
