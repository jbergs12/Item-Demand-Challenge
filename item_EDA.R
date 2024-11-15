library(tidymodels)
library(vroom)
library(tidyverse)
library(forecast)
library(patchwork)

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

View(item_train)

sample(1:10, 2) # 10 3
sample(1:50, 2) # 48 5

s10_i48 <- item_train |> 
  filter(store == 10,
         item == 48)

s3_i5 <- item_train |> 
  filter(store == 3,
         item == 5)

time1 <- s10_i48 |> 
  ggplot(mapping=aes(x = date,
                     y = sales)) +
  geom_line() +
  geom_smooth(se=F)

time2 <- s3_i5 |> 
  ggplot(mapping=aes(x = date,
                     y = sales)) +
  geom_line() +
  geom_smooth(se=F)

acfm1 <- s10_i48 |> 
  pull(sales) |> 
  ggAcf()

acfm2 <- s3_i5 |> 
  pull(sales) |> 
  ggAcf()

acfy1 <- s10_i48 |> 
  pull(sales) |> 
  ggAcf(lag.max = 2*365)

acfy2 <- s3_i5 |> 
  pull(sales) |> 
  ggAcf(lag.max = 2*365)

(time1 + acfm1 + acfy1) / (time2 + acfm2 + acfy2)
