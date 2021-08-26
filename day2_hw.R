#1. Add ggplot2::mpg dataset.
library(ggplot2)
library(tidyverse) 
library(data.table)
library(rstudioapi)
library(skimr)
library(inspectdf)
library(mice)
library(plotly)
library(highcharter)
library(recipes) 
library(caret) 
library(purrr) 
library(graphics) 
library(Hmisc) 
library(glue)
mpg
#2. Make data ready for analysis doing preprocessing techniques.

mpg %>% 
  inspect_na() %>% 
  filter(pcnt<30) %>% 
  pull(col_name) -> variables

mpg <- mpg %>% select(variables)
mpg

df.num <- mpg %>%
  select_if(is.numeric) %>%
  select(cty,everything())

df.chr <- mpg %>%
  select_if(is.character)


df.num %>% inspect_na()

df.num_mice <- df.num %>% mice(method='rf', seed=123)
df.num <- df.num_mice %>% complete()


df.chr %>% inspect_na()


# One Hote Encoding ----
df.chr <- dummyVars(" ~ .", data = df.chr) %>% 
  predict(newdata = df.chr) %>% 
  as.data.frame()

df <- cbind(df.chr,df.num) %>%
  select(cty,everything())

df

#3. Fit Generalized Linear Model using H2O in R.
library(h2o)
h2o.init()



h2o_data <- df %>% as.h2o()


# Splitting the data ----
h2o_data <- h2o_data %>% h2o.splitFrame(ratios = 0.8, seed = 123)
train <- h2o_data[[1]]
test <- h2o_data[[2]]

target <- 'cty'
features <- df %>% select(-cty) %>% names()


# Fitting h2o model ----
model <- h2o.glm(
  x = features, y = target,
  training_frame = train,
  validation_frame = test,
  nfolds = 10, seed = 123,
  lambda = 0, compute_p_values = T)

model@model$coefficients_table %>%
  as.data.frame() %>%
  dplyr::select(names,p_value) %>%
  mutate(p_value = round(p_value,3)) %>%
  .[-1,] %>%
  arrange(desc(p_value))

#4. Run GLM using following modelling structure. cty ~ year + cyl + displ.


#5. Print coefficients table and give interpretation of results.
#6. Name your final homework Script as “Fuel_Economy_Data_Analysis”.