library(tidymodels)
library(vroom)
library(embed)
library(modeltime)
library(timetk)

item_train <- vroom("train.csv")
item_test <- vroom("test.csv")

n_stores <- max(item_train$store)
n_items <- max(item_train$item)

all_preds <- data.frame()

for(i in 1:n_stores){
  for(j in 1:n_items){
    
    SI_train <- item_train |> 
      filter(store == i,
             item == j)
    
    SI_test <- item_test |> 
      filter(store == i,
             item == j)
    
    CV_split <- time_series_split(SI_train,
                                  assess="3 months",
                                  cumulative = T)
    
    CV_split |> 
      tk_time_series_cv_plan() |> 
      plot_time_series_cv_plan(date,
                               sales,
                               .interactive = F)
    
    item_prophet <- prophet_reg() |> 
      set_engine(engine = "prophet") |> 
      fit(sales ~ date, data = training(CV_split))
    
    CV_results <- modeltime_calibrate(item_prophet,
                                      new_data = testing(CV_split))
    
    fullfit <- CV_results |> 
      modeltime_refit(data=SI_train)
    
    preds <- fullfit |> 
      modeltime_forecast(
        new_data = SI_test,
        actual_data = SI_train
      ) |>
      filter(!is.na(.model_id)) |>
      mutate(id=SI_test$id) |>
      select(id, .value) |> 
      rename(sales = .value)
    
    if(i == 1 & j == 1){
      all_preds <- preds
    } else{
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

nrow(all_preds)

vroom_write(all_preds, file = "submission.csv", delim = ",")
