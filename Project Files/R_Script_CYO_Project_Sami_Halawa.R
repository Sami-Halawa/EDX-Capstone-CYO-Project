# Creating a function to calculate the RMSLE

RMSLE <- function(a){
  RMSE(log(test_set$SalePrice),log(a))
} 


## Installing packages, if required. Note: this process may take a few minutes. 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org",
                                         dependencies = TRUE)

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org",
                                     dependencies = TRUE)

if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org",
                                          dependencies = TRUE)

if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org",
                                         dependencies = TRUE)

if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org",
                                     dependencies = TRUE)

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org",
                                     dependencies = TRUE)

if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org",
                                      dependencies = TRUE)

if(!require(downloader)) install.packages("downloader", repos = "http://cran.us.r-project.org",
                                          dependencies = TRUE)

if(!require(RCurl)) install.packages("RCurl", repos = "http://cran.us.r-project.org",
                                     dependencies = TRUE)

if(!require(kableExtra)) install.packages("knitExtra", repos = "http://cran.us.r-project.org",
                                          dependencies = TRUE)

if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org",
                                     dependencies = TRUE)

if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org",
                                        dependencies = TRUE)

if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org",
                                          dependencies = TRUE)

if(!require(arm)) install.packages("arm", repos = "http://cran.us.r-project.org",
                                   dependencies = TRUE)

if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org",
                                       dependencies = TRUE)

if(!require(gbm)) install.packages("gbm", repos = "http://cran.us.r-project.org",
                                   dependencies = TRUE)

if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org",
                                            dependencies = TRUE)


# Loading packages

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(dplyr)
library(readr)
library(downloader)
library(RCurl)
library(readxl)
library(kableExtra)
library(knitr)
library(randomForest)
library(reshape2)
library(rpart.plot)
library(arm)
library(xgboost)
library(gbm)
library(randomForest)




########## IMPORTING DATA 



# Reading in the data from Github

#### TEST data

test_url <- "https://raw.githubusercontent.com/Sami-Halawa/edx-Capstone-CYO-Project/main/test.csv"

test <- tempfile()
download.file(test_url, test)

test <- read_csv(test)

# Converting to a dataframe 

test <- as.data.frame(test)

### TRAIN data

train_url <- "https://raw.githubusercontent.com/Sami-Halawa/edx-Capstone-CYO-Project/main/train.csv"

train <- tempfile()
download.file(train_url, train)

train <- read_csv(train)

# Converting to a dataframe 

train <- as.data.frame(train)



##### PREPROCESSING

# Snapshot of the train set

str(train)

summary(train)


# Checking for missing values on a column by column basis

test_na <- data.frame(NAs=colSums(is.na(test))) %>% filter(NAs>0) %>% arrange(desc(NAs))

test_na 

train_na <- data.frame(NAs=colSums(is.na(train))) %>% filter(NAs>0) %>% arrange(desc(NAs))

train_na 




# UPDATING MISSING VALUES in TRAIN SET

train <- train %>%
  mutate(PoolQC = ifelse(is.na(PoolQC), "None", PoolQC),
         MiscFeature = ifelse(is.na(MiscFeature), "None", MiscFeature),
         Alley = ifelse(is.na(Alley), "None", Alley),Fence = ifelse(is.na(Fence), "None", Fence),
         FireplaceQu = ifelse(is.na(FireplaceQu), "None", FireplaceQu),
         GarageFinish = ifelse(is.na(GarageFinish), "None", GarageFinish),
         GarageQual = ifelse(is.na(GarageQual), "None", GarageQual),
         GarageCond = ifelse(is.na(GarageCond), "None", GarageCond),
         GarageType = ifelse(is.na(GarageType), "None", GarageType),
         BsmtCond = ifelse(is.na(BsmtCond), "None", BsmtCond),
         BsmtQual = ifelse(is.na(BsmtQual), "None", BsmtQual),
         BsmtExposure = ifelse(is.na(BsmtExposure), "None", BsmtExposure),
         BsmtFinType1 = ifelse(is.na(BsmtFinType1), "None", BsmtFinType1),
         BsmtFinType2 = ifelse(is.na(BsmtFinType2), "None", BsmtFinType2),
         MasVnrType = ifelse(is.na(MasVnrType), "None", MasVnrType),
         MSZoning = ifelse(is.na(MSZoning), "RL", MSZoning),
         Utilities = ifelse(is.na(Utilities), "Allpub", Utilities),
         Functional = ifelse(is.na(Functional), "Typ", Functional),
         Exterior1st = ifelse(is.na(Exterior1st), "VinylSd", Exterior1st),
         Exterior2nd = ifelse(is.na(Exterior2nd), "VinylSd", Exterior2nd),
         KitchenQual = ifelse(is.na(KitchenQual), "None", KitchenQual),
         SaleType = ifelse(is.na(SaleType), "WD", SaleType),
         LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
         GarageYrBlt = ifelse(is.na(GarageYrBlt), 0, GarageYrBlt),
         MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea),
         BsmtFullBath = ifelse(is.na(BsmtFullBath), 0, BsmtFullBath),
         BsmtHalfBath = ifelse(is.na(BsmtHalfBath), 0, BsmtHalfBath),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1), 0, BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2), 0, BsmtFinSF2),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF), 0, BsmtUnfSF),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF), 0, TotalBsmtSF),
         GarageCars = ifelse(is.na(GarageCars), 0, GarageCars),
         GarageArea = ifelse(is.na(GarageArea), 0, GarageArea),
         Electrical = ifelse(is.na(Electrical), "SBrkr", Electrical))

# New total number of NAs

sum(is.na(train))


# UPDATING MISSING VALUES in TEST SET

test <- test %>%
  mutate(PoolQC = ifelse(is.na(PoolQC), "None", PoolQC),
         MiscFeature = ifelse(is.na(MiscFeature), "None", MiscFeature),
         Alley = ifelse(is.na(Alley), "None", Alley),Fence = ifelse(is.na(Fence), "None", Fence),
         FireplaceQu = ifelse(is.na(FireplaceQu), "None", FireplaceQu),
         GarageFinish = ifelse(is.na(GarageFinish), "None", GarageFinish),
         GarageQual = ifelse(is.na(GarageQual), "None", GarageQual),
         GarageCond = ifelse(is.na(GarageCond), "None", GarageCond),
         GarageType = ifelse(is.na(GarageType), "None", GarageType),
         BsmtCond = ifelse(is.na(BsmtCond), "None", BsmtCond),
         BsmtQual = ifelse(is.na(BsmtQual), "None", BsmtQual),
         BsmtExposure = ifelse(is.na(BsmtExposure), "None", BsmtExposure),
         BsmtFinType1 = ifelse(is.na(BsmtFinType1), "None", BsmtFinType1),
         BsmtFinType2 = ifelse(is.na(BsmtFinType2), "None", BsmtFinType2),
         MasVnrType = ifelse(is.na(MasVnrType), "None", MasVnrType),
         MSZoning = ifelse(is.na(MSZoning), "RL", MSZoning),
         Utilities = ifelse(is.na(Utilities), "Allpub", Utilities),
         Functional = ifelse(is.na(Functional), "Typ", Functional),
         Exterior1st = ifelse(is.na(Exterior1st), "VinylSd", Exterior1st),
         Exterior2nd = ifelse(is.na(Exterior2nd), "VinylSd", Exterior2nd),
         KitchenQual = ifelse(is.na(KitchenQual), "None", KitchenQual),
         SaleType = ifelse(is.na(SaleType), "WD", SaleType),
         LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
         GarageYrBlt = ifelse(is.na(GarageYrBlt), 0, GarageYrBlt),
         MasVnrArea = ifelse(is.na(MasVnrArea), 0, MasVnrArea),
         BsmtFullBath = ifelse(is.na(BsmtFullBath), 0, BsmtFullBath),
         BsmtHalfBath = ifelse(is.na(BsmtHalfBath), 0, BsmtHalfBath),
         BsmtFinSF1 = ifelse(is.na(BsmtFinSF1), 0, BsmtFinSF1),
         BsmtFinSF2 = ifelse(is.na(BsmtFinSF2), 0, BsmtFinSF2),
         BsmtUnfSF = ifelse(is.na(BsmtUnfSF), 0, BsmtUnfSF),
         TotalBsmtSF = ifelse(is.na(TotalBsmtSF), 0, TotalBsmtSF),
         GarageCars = ifelse(is.na(GarageCars), 0, GarageCars),
         GarageArea = ifelse(is.na(GarageArea), 0, GarageArea), 
         Electrical = ifelse(is.na(Electrical), "SBrkr", Electrical))

# New total number of NAs

sum(is.na(test))



### Converting Character Columns to Factors

# Generating a list of character columns

## Test Set 

col_names <- colnames(test[,sapply(test,class)=="character"])

# Converting these columns to factors

test[col_names] <- lapply(test[col_names] , factor)

## Train Set

# Generating a list of character columns

col_names_2 <- colnames(train[,sapply(train,class)=="character"])

# Converting these columns to factors

train[col_names_2] <- lapply(train[col_names_2] , factor)





############ Creating the validation set



set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use ‘set.seed(1)‘

test_index <- createDataPartition(y=train$SalePrice, times=1, p=0.1, list=FALSE)

sales_data <-train[-test_index,]

temp <- train[test_index,]


# Making sure that the key variables in the validation set are also in the sales_data set

validation <- temp %>% 
  semi_join(sales_data, by = "OverallQual") %>%
  semi_join(sales_data, by = "GrLivArea") %>%
  semi_join(sales_data, by = "GarageCars")


# Adding rows removed from validation set back into the sales_data set

removed <- anti_join(temp, validation)

sales_data <- rbind(sales_data, removed)







###### Creating the new Train and Test datasets

# Partitioning the data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use ‘set.seed(1)‘

test_index <- createDataPartition(y=sales_data$SalePrice, times=1, p=0.1, list=FALSE)  

# Creating the train and test sets

train_set <-sales_data[-test_index,]

test_set <- sales_data[test_index,]

# Ensuring column names are valid

colnames(test_set) <- make.names(colnames(test_set))
colnames(train_set) <- make.names(colnames(train_set))

# Removing OverallCond, GrLivArea, GarageCars from the test set that do not appear in the training set

test_set <- test_set %>% 
  semi_join(train_set, by="OverallQual") %>%
  semi_join(train_set, by="GrLivArea") %>% 
  semi_join(train_set, by="GarageCars") 




######## Analysis


# Number of distinct property Ids in the train set

a <- train_set %>% 
  group_by(Id) 

nrow(a)


# Grouping sale prices

sp_grouped <- train_set %>% 
  group_by(SalePrice) %>% 
  summarize(n=n()) 

# Producing a Histogram of Sale prices

sp_grouped %>% 
  ggplot(aes(SalePrice)) + 
  geom_histogram(bins = 50, color = "black") + 
  scale_x_log10()

# Summary of sale prices

summary(sp_grouped)





############## Correlation Matrix ##############


# Generating the correlation matrix 

cor_train <- round(cor(train_set[, !sapply(train_set, is.factor)]),2)

# Producing a heat map of correlations

melted <- melt(cor_train, na.rm = TRUE)

# Heatmap

ggplot(data = melted, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 7, hjust = 1), axis.text.y = element_text(vjust = 1, 
                                                                                    size = 7, hjust = 1))+
  coord_fixed()


# Producing a table of variables correlated with Sale Price

price_cor <- melted %>% filter(Var1=="SalePrice") %>% arrange(desc(value))
price_cor %>% knitr:: kable()




# Top 10 features correlated with SalePrice sorted by absolute correlation

price_cor <- price_cor %>% filter(Var2 !="SalePrice") %>% arrange(desc(abs(value)))

head(price_cor,10,value)



########## Predictive Models ##################



#### 1) Mean model

# Calculating the mean rating

mu <- mean(train_set$SalePrice)

mu


# Calculating the RMSLE using our predefined function

RMSLE_mu <- RMSLE(mu)

# Creating a data frame to store RMSEs

results <- tibble(Model="Mean Rating", RMSLE=format(round(RMSLE_mu,5),nsmall=5))
results %>% knitr::kable()





### 2) Adding in Overall Quality effect 



### Overall Quality effect model


# Generating the estimates of b_q

b_q <- train_set %>% 
  group_by(OverallQual) %>% 
  summarise(n=n(), b_q = mean(SalePrice-mu))


# Creating a plot of b_q

qplot(b_q, data=b_q, bins=20, color=I("black"))

# Predicted sale price

pred_qual <- mu + test_set %>%
  left_join(b_q, by='OverallQual') %>%
  pull(b_q)




# Calculating the RMSLE using our predefined function

RMSLE_qual <- RMSLE(pred_qual)


# Adding results to our dataframe

results <- rbind(results, c(Model="Overall Quality effect", 
                            RMSLE=format(round(RMSLE_qual,5),nsmall=5)))


results %>% knitr::kable()




################# 3) Overall Quality and Gross Living Area model


### Overall Quality & Gross Living Area effect model

# Generating the estimates of b_gla

gla_avg <- train_set %>% 
  left_join(b_q, by='OverallQual') %>%
  group_by(GrLivArea) %>% 
  summarise(b_gla=mean(SalePrice - mu - b_q))


pred_ql <- test_set %>% 
  left_join(b_q, by='OverallQual') %>%
  left_join(gla_avg, by='GrLivArea') %>%
  mutate(prediction = mu + b_q + b_gla) %>%
  pull(prediction)


# Calculating the RMSLE using our predefined function

RMSLE_ql <- RMSLE(pred_ql)

# Adding results to our dataframe

results <- rbind(results, c(Model="Overall Quality & Gross Living Area effect", 
                            RMSLE=format(round(RMSLE_ql ,5),nsmall=5)))

results %>% knitr::kable()



############ 4) Overall Quality and Garage Cars effect


### Overall Quality and Garage Cars effect model

# Generating the estimates of b_gc

gc_avg <- train_set %>% 
  left_join(b_q, by='OverallQual') %>%
  group_by(GarageCars) %>% 
  summarise(b_gc=mean(SalePrice - mu - b_q))


# Creating a plot of b_gc

qplot(b_gc, data=gc_avg, bins=30, color=I("black"))

# Generating predictions

pred_qg <- test_set %>% 
  left_join(b_q, by='OverallQual') %>%
  left_join(gc_avg, by='GarageCars') %>%
  mutate(prediction = mu + b_q + b_gc) %>%
  pull(prediction)


# Calculating the RMSLE using our predefined function

RMSLE_qg <- RMSLE(pred_qg)

# Adding results to our dataframe

results <- rbind(results, c(Model="Overall Quality & Garage Cars effect", 
                            RMSLE=format(round(RMSLE_qg ,5),nsmall=5)))

results %>% knitr::kable()


#################### PREDICTIVE MODELS - Advanced:

# Generating the tree

tree <- rpart(SalePrice ~., data=train_set)

# Plotting the tree

rpart.plot(tree, extra = 101)



# Training our model

train_rpart <- train(SalePrice ~., method="rpart", data=train_set)

# Predictions

pred_rpart <- predict(train_rpart, test_set, type="raw")

# Calculating the RMSLE

RMSLE_rpart <- RMSLE(pred_rpart)

results <- rbind(results, c(Model="R Part", 
                            RMSLE=format(round(RMSLE_rpart, 5),nsmall=5)))

results %>% knitr::kable()



# Generating Variable Importance

varImp(train_rpart)


# Generating a list of predictor names

ind <- !(train_rpart$finalModel$frame$var == "<leaf>")

tree_terms <- 
  train_rpart$finalModel$frame$var[ind] %>%
  unique() %>%
  as.character()

tree_terms



################################## Random Forest


# # Determining optimal number of trees

t <- seq(1,20,1)

set.seed(1,sample.kind = "Rounding")

RMSLES <- sapply(t, function(t){
  
  set.seed(1,sample.kind = "Rounding")
  train_rf <- train(SalePrice ~., method="rf", data=train_set, ntree=t)
  
  pred_rf <- predict(train_rf, test_set, type="raw")
  
  return(RMSLE(pred_rf))
})

# Plotting the results

qplot(t, RMSLES)

# Determining the number of trees that minimises the RMSLE

t_opt <- t[which.min(RMSLES)]

t[which.min(RMSLES)]

RMSLE_rf <- min(RMSLES)


# Adding RMSLE to the table

results <- rbind(results, c(Model="Random Forest", 
                            RMSLE=format(round(RMSLE_rf, 5),nsmall=5)))

results %>% knitr::kable()






# Generating the predictions for Random Forest for use later

train_rf <- train(SalePrice ~., method="rf", data=train_set, ntree=t_opt)

pred_rf <- predict(train_rf, test_set, type="raw")

pred_rf <- unname(pred_rf)


# Generating Variable Importance

varImp(train_rf)






########################## KNN

# Generating the model and estimates

set.seed(1, sample.kind="Rounding")

train_knn <- train(SalePrice ~., method="knn", data=train_set)

pred_knn <- predict(train_knn, test_set, type="raw")

# Calculating the RMSLE

RMSLE_knn <- RMSLE(pred_knn)


# Adding RMSLE to the table

results <- rbind(results, c(Model="KNN", 
                            RMSLE=format(round(RMSLE_knn, 5),nsmall=5)))

results %>% knitr::kable()



# Generating Variable Importance

varImp(train_knn)






############## Stochastic Gradient Boosting


### Please note that this part of the code may take several minutes to run!

# Train control to ensure reproducibility 

set.seed(321, sample.kind = "Rounding")

seeds <- vector(mode = "list", length = 51)

for(i in 1:50) seeds[[i]] <- sample.int(1000, 20)

seeds[[51]] <- sample.int(1000, 1)


my_cont <- trainControl(number= 5, seeds=seeds)



# Applying a Stochastic gradient boost

set.seed(1, sample.kind = "Rounding")

train_gb <- train(SalePrice ~., method="gbm", data=train_set,trControl=my_cont)

pred_gb <- predict(train_gb, test_set, type="raw")

# RMSLE

RMSLE_gb <- RMSLE(pred_gb)


# Adding RMSLE to the table

results <- rbind(results, c(Model="Stochastic Gradient Boosting", 
                            RMSLE=format(round(RMSLE_gb, 5),nsmall=5)))

results %>% knitr::kable()


# Generating Variable Importance

varImp(train_gb)



######################### Ensemble



# Computing our estimate of Sale Prices

pred_ensemble <- exp((log(pred_gb) + log(pred_rf))/ 2) 

# Calculating the RMSLE

RMSLE_ensemble <- RMSLE(pred_ensemble)

# Adding RMSLE to the table

results <- rbind(results, c(Model="Ensemble", 
                            RMSLE=format(round(RMSLE_ensemble, 5),nsmall=5)))

results %>% knitr::kable()





################ RESULTS


# Generating the Results table to compare the performance of the various models

results %>% arrange(desc(RMSLE)) %>% knitr::kable()




############# Performance against the Validation set


# Generating the predictions by Stochastic Gradient Boosting


### Please note that this part of the code may take several minutes to run!

# Train control to ensure reproducibility 

set.seed(321, sample.kind = "Rounding")

seeds <- vector(mode = "list", length = 51)

for(i in 1:50) seeds[[i]] <- sample.int(1000, 20)

seeds[[51]] <- sample.int(1000, 1)


my_cont <- trainControl(number= 5, seeds=seeds)



# Applying a Stochastic gradient boost

set.seed(1, sample.kind = "Rounding")

train_gb_v <- train(SalePrice ~., method="gbm", data=sales_data,trControl=my_cont)

pred_gb_v <- predict(train_gb_v, validation, type="raw")



### Generating our Random Forest estimates


# Training our final model on the Sales Data

train_rf_v <- train(SalePrice ~., method="rf", data=sales_data, ntree=t_opt)

# Generating Predictions

pred_rf_v <- predict(train_rf_v, validation, type="raw")

pred_rf_v <- unname(pred_rf_v)





### Combining the estimates to produce the Ensemble estimates

# Computing the ensemble estimates of Sale Prices

pred_ensemble_v <- exp((log(pred_gb_v) + log(pred_rf_v))/ 2) 

# Calculating the RMSLE

RMSLE_ensemble_v <- RMSE(log(validation$SalePrice), log(pred_ensemble_v))


# Adding RMSLE to the table

results <- rbind(results, c(Model="Ensemble on Validation set", 
                            RMSLE=format(round(RMSLE_ensemble_v, 5),nsmall=5)))

results %>% knitr::kable()






# Top 10 features correlated with SalePrice sorted by absolute correlation

price_cor <- price_cor %>% arrange(desc(abs(value)))

head(price_cor,10,value) %>% knitr::kable()

