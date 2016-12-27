source("cleanse_data.R")
source("Models2.R") # fit and predicts the different models
source("Calc_Month_Prices.R") # calcualtes the median monthly sales price per neighborhood
source("outliers.R") # removes outlier house sales
source("remove_factor_vars.R") # converts all the factor variables to indicator variables
library(caret)
library(doMC)
registerDoMC(cores = 7)

# read in data
house <- read.table("train.csv", header = TRUE, sep=',')
house_test <- read.table("test.csv", header = TRUE, sep=',')

# imput the missing values in both the train and prediction data sets using MICE
# variables to keep to merge on after imputations
vars_keep <- c("Id", "SalePrice")
house_keep <- house[, names(house) %in% vars_keep, drop = F]
train_vars_keep <- c("Id") # keeps track of the houses in the train data set
train_keep <- house_test[, names(house_test) %in% train_vars_keep, drop = F]

cols.dont.want <- c("SalePrice")
house_train <- house[, ! names(house) %in% cols.dont.want, drop = F]

# merge the train and test data sets together to imput missing values.
# We are able to merge them together since we are not predicting the values into the future and all 
# sales are from the same time period in both data sets.
combined <- rbind(house_train, house_test)
combined <- data_prep(combined) # clean the data set and add feature variables
combined <- month_prices(combined) # calcualte median monthly sales price per neighborhood

# add the new variables to their respective data frames
house <- merge(x = house_keep, y = combined, by = "Id", all.x = TRUE)
house_test <- merge(x = train_keep, y = combined, by = "Id", all.x = TRUE)

# remove outliers, the outliers ID numbers where found using 
# regression anlaysis and reviewing the their rstudent values
# removing houses that have unsusual characteristics
house <- remove_outliers(house) 

# transform SalesPrice
house$price_min_misc = house$SalePrice - house$MiscVal
house$Log_price_new = log(house$price_min_misc)

# use Boruta feature selection code below to find informative features

#model_drop <- c("MiscVal", "SalePrice", "Id", "price_min_misc")
#house_boruta <- house[, (! names(house) %in% model_drop)] # keep
#names(house_boruta)
##library(Boruta)
#bor.results <- Boruta(house_boruta, house_boruta$Log_price_new, maxRuns=200)
#plot(bor.results)  
#data.frame(bor.results$finalDecision)
#names(bor.results)
#CONFIRMED_ATTR <- getSelectedAttributes(bor.results)

# model cols where chosen using a Boruta feature selection - code commented out above
keep <- c("LotFrontage",           "LotArea"  ,             "OverallQual",           "OverallCond"   ,        "YearBuilt"  ,          
          "MasVnrArea",            "TotalBsmtSF",           "X1stFlrSF"  ,           "X2ndFlrSF"      ,       "GrLivArea"  ,          
          "FullBath",              "HalfBath"    ,          "BedroomAbvGr",          "TotRmsAbvGrd"   ,       "Fireplaces"  ,         
          "GarageArea",            "no_GarageFlag",         "Has2ndFloor"  ,         "HasMasVnrArea"  ,       "HasWoodDeckSF",        
          "HasOpenPorchSF",        "NewerDwelling" ,        "LotShape_num"  ,        "GarageType_num" ,       "PavedDrive_num",       
          "ExterQual_num",         "BsmtQual_num"   ,       "HeatingQC_num"  ,       "KitchenQual_num" ,      "FireplaceQu_num",      
          "BsmtExposure_num",      "BsmtFinType1_num",      "GarageFinish_num",      "Neighborhood_bin" ,     "total_porchSF"   ,     
          "medPrice_Neighborhood", "Log_price_new", 'Id', "GrLivArea", "MiscVal", "SalePrice") 

house_w_features <- house[, (names(house) %in% keep)] # keep
house_test_models <- house_test[, (names(house_test) %in% keep)] # keep
# prepare the data for the model
model_drop <- c("MiscVal", "SalePrice", "Id")

######################
# create actual fit and test submissions
model <- house_w_features[, ! names(house_w_features) %in% model_drop, drop = F]
var_all <- c("price_min_misc", "MiscVal", "SalePrice", "Id")#c("price_min_misc", "price_new_SF", "GrLivArea", "MiscVal", "SalePrice", "Id")
model_foba <- house[, ! names(house) %in% var_all, drop = F]

drop_prices <- c("MiscVal", "SalePrice", "Id")
house_test_actual <- house_test_models[, ! names(house_test_models) %in% drop_prices, drop = F]
house_test_actFoba <- house_test[, ! names(house_test) %in% drop_prices, drop = F]
test_predColss <- c("GrLivArea", "MiscVal")
house_test_pred <- house_test_models[,  names(house_test_models) %in% test_predColss, drop = F]
#####################
## remove factors for foba model
test_noFactor <- remove_factors(house_test_actual)
house_test_actFoba_noFact <- remove_factors(house_test_actFoba)

source("Models2.R") 

# fit and predict the GBM model
gbmFit <- fit_gbm(model, house_test_actual, house_test_pred) # RMSE: 20289.22, r-squared: 0.9279818
mean(gbmFit$resample$RMSE) # RMSECV: reg price - misc; 0.1222432
gbm_pred <- predict(gbmFit, house_test_actual)
gbm_pred2 <- exp(gbm_pred)  + house_test_pred$MiscVal
summary(gbmFit)

# fit and predict the FOBA model
fobaFit <- fit_foba(model_foba, house_test_actFoba, house_test_pred) # RMSE: 23182.47, r-squared: 0.910276
mean(fobaFit$resample$RMSE) # RMSECV: 0.1252527; reg price - misc; 0.1232678
foba_pred <- predict(fobaFit, house_test_actFoba_noFact)
foba_pred2 <- exp(foba_pred)  + house_test_pred$MiscVal

# review the correlation of the two models
model_preds_actual = cbind(gbm_pred2, foba_pred2)  
cor(model_preds_actual, model_preds_actual)

# write files to submit to kaggle
submission_test <- cbind("Id" = house_test$Id, "SalePrice" = foba_pred2 + gbm_pred2) / 2)
write.csv(submission_test, file = "foba_gbm_ensemble.csv", row.names=FALSE)

