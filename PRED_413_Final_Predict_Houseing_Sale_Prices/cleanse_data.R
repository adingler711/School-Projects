install.packages("mice")
library(mice)

data_prep <- function(train_raw) {

        train_raw$MSSubClass <- as.factor(train_raw$MSSubClass)
        train_raw$RemodSaleYr <- replace(train_raw$YearRemodAdd, train_raw$YearRemodAdd == train_raw$YrSold, 1)
        train_raw$RemodSaleYr <- replace(train_raw$RemodSaleYr, train_raw$YearRemodAdd != train_raw$YrSold, 0)
        train_raw$new_house <-  replace(train_raw$YearRemodAdd, train_raw$YrSold == train_raw$YearBuilt, 1)
        train_raw$new_house <-  replace(train_raw$new_house, train_raw$YrSold != train_raw$YearBuilt, 0)
        train_raw$YearRemodAdd <- replace(train_raw$YearRemodAdd, train_raw$YearRemodAdd == train_raw$YearBuilt, 0)
        train_raw$GarageYrBlt <- replace(train_raw$GarageYrBlt, train_raw$GarageYrBlt == train_raw$YearBuilt, 0)
        train_raw$Peak_Season <- ifelse(as.numeric(train_raw$MoSold) < 4.0, 0,
                                        ifelse(as.numeric(train_raw$MoSold) > 7.0, 0, 1))
        train_raw$Sales_condition_priceDiff <- ifelse(train_raw$SaleCondition == 'Normal', 0,
                                        ifelse(train_raw$SaleCondition == 'Partial', 0, 1))
        train_raw$Pre_built <- ifelse(train_raw$SaleCondition == 'Partial', 1, 0)
        train_raw$YearBuilt <- train_raw$YrSold - train_raw$YearBuilt
        train_raw$YearRemodAdd <- train_raw$YrSold - train_raw$YearRemodAdd
        train_raw$GarageYrBlt <- train_raw$YrSold - train_raw$GarageYrBlt
        train_raw$MoSold <- as.factor(train_raw$MoSold)
        train_raw$YrSold <- as.factor(train_raw$YrSold)
        
        levels(train_raw$PoolQC) <- c(levels(train_raw$PoolQC), 0)
        train_raw$PoolQC[train_raw$PoolArea == 0] <- 0
        
        # Use MiscVal to impute MiscFeature
        levels(train_raw$MiscFeature) <- c(levels(train_raw$MiscFeature), 0)
        train_raw$MiscFeature[train_raw$MiscVal == 0] <- 0
        
        # Use GarageArea to impute GarageQual, GarageFinish, GarageCond, GarageYrBlt, GarageType
        levels(train_raw$GarageQual) <- c(levels(train_raw$GarageQual), 0)
        train_raw$GarageQual[train_raw$GarageArea == 0] <- 0
        levels(train_raw$GarageFinish) <- c(levels(train_raw$GarageFinish), 0)
        train_raw$GarageFinish[train_raw$GarageArea == 0] <- 0
        levels(train_raw$GarageCond) <- c(levels(train_raw$GarageCond), 0)
        train_raw$GarageCond[train_raw$GarageArea == 0] <- 0
        levels(train_raw$GarageType) <- c(levels(train_raw$GarageType), 0)
        train_raw$GarageType[train_raw$GarageArea == 0] <- 0
        levels(train_raw$GarageYrBlt) <- c(levels(train_raw$GarageYrBlt), 0)
        train_raw$GarageYrBlt[train_raw$GarageArea == 0] <- 0
        train_raw$no_GarageFlag <- train_raw$GarageArea # created a no garage flag
        train_raw$no_GarageFlag[train_raw$no_GarageFlag == 0] <- 1
        train_raw$no_GarageFlag[train_raw$no_GarageFlag > 1] <- 0
                
        # Use TotalBsmtSF to impute BsmtExposure BsmtFinType2 BsmtQual, BsmtCond BsmtFinType1
        levels(train_raw$BsmtExposure) <- c(levels(train_raw$BsmtExposure), 0)
        train_raw$BsmtExposure[train_raw$TotalBsmtSF == 0] <- 0
        levels(train_raw$BsmtFinType2) <- c(levels(train_raw$BsmtFinType2), 0)
        train_raw$BsmtFinType2[train_raw$TotalBsmtSF == 0] <- 0
        levels(train_raw$BsmtQual) <- c(levels(train_raw$BsmtQual), 0)
        train_raw$BsmtQual[train_raw$TotalBsmtSF == 0] <- 0
        levels(train_raw$BsmtCond) <- c(levels(train_raw$BsmtCond), 0)
        train_raw$BsmtCond[train_raw$TotalBsmtSF == 0] <- 0
        levels(train_raw$BsmtFinType1) <- c(levels(train_raw$BsmtFinType1), 0)
        train_raw$BsmtFinType1[train_raw$TotalBsmtSF == 0] <- 0
        
        # Use Fireplaces to impute FireplaceQu        
        levels(train_raw$FireplaceQu) <- c(levels(train_raw$FireplaceQu), 0)
        train_raw$FireplaceQu[train_raw$Fireplaces == 0] <- 0
        
        # impute variables that have nulls with zero that logically make sense
        levels(train_raw$Alley) <- c(levels(train_raw$Alley), 'No_Alley')
        train_raw$Alley[is.na(train_raw$Alley) == TRUE] <- 'No_Alley'
        
        levels(train_raw$Fence) <- c(levels(train_raw$Fence), 0)
        train_raw$Fence[is.na(train_raw$Fence) == TRUE] <- 0
        
        train_raw$Has2ndFloor <- ifelse(train_raw$X2ndFlrSF == 0, 0, 1)
        train_raw$HasMasVnrArea <- ifelse(train_raw$MasVnrArea == 0, 0, 1)
        train_raw$HasWoodDeckSF <- ifelse(train_raw$WoodDeckSF == 0, 0, 1)
        train_raw$HasOpenPorchSF <- ifelse(train_raw$OpenPorchSF == 0, 0, 1)
        train_raw$HasEnclosedPorch <- ifelse(train_raw$EnclosedPorch == 0, 0, 1)
        train_raw$HasScreenPorch <- ifelse(train_raw$ScreenPorch == 0, 0, 1)
        train_raw$HasX3SsnPorch <- ifelse(train_raw$X3SsnPorch == 0, 0, 1)
        train_raw$NewerDwelling <- ifelse(train_raw$MSSubClass == 20, 1,
                                          ifelse(train_raw$MSSubClass == 60, 1,
                                                 ifelse(train_raw$MSSubClass == 120, 1, 0)))
       
        train_raw$LotShape_num <- ifelse(train_raw$LotShape == "Reg", 1,
                                       ifelse(train_raw$LotShape != 'Reg', 0, NA))
        
        train_raw$LandContour_num <- ifelse(train_raw$LandContour == "Lvl", 1,
                                          ifelse(train_raw$LandContour != 'Lvl', 0, NA))
        
        train_raw$LandSlope_num <- ifelse(train_raw$LandSlope == "Gtl", 1,
                                             ifelse(train_raw$LandSlope != 'Gtl', 0, NA))
        
        train_raw$Electrical_num <- ifelse(train_raw$Electrical == "SBrkr", 1,
                                           ifelse(train_raw$Electrical != 'SBrkr', 0, NA))
        
        train_raw$GarageType_num <- ifelse(train_raw$GarageType == "Detchd", 1,
                                            ifelse(train_raw$GarageType != 'Detchd', 0, NA))
        
        train_raw$PavedDrive_num <- ifelse(train_raw$PavedDrive == "Y", 1,
                                            ifelse(train_raw$PavedDrive != 'Y', 0, NA))
        
        # change quality factor variables to numerical
        train_raw$ExterQual_num <- ifelse(train_raw$ExterQual == 'Po', 1,
                                           ifelse(train_raw$ExterQual == "Fa", 2,
                                                  ifelse(train_raw$ExterQual == 'TA', 3, 
                                                         ifelse(train_raw$ExterQual == 'Gd', 4, 
                                                                ifelse(train_raw$ExterQual == 'Ex', 5,NA)))))
        
        train_raw$ExterCond_num <- ifelse(train_raw$ExterCond == 'Po', 1,
                                           ifelse(train_raw$ExterCond == "Fa", 2,
                                                  ifelse(train_raw$ExterCond == 'TA', 3, 
                                                         ifelse(train_raw$ExterCond == 'Gd', 4, 
                                                                ifelse(train_raw$ExterCond == 'Ex', 5,NA)))))
        
        train_raw$BsmtQual_num <- ifelse(train_raw$BsmtQual == 'Po', 1,
                                           ifelse(train_raw$BsmtQual == "Fa", 2,
                                                  ifelse(train_raw$BsmtQual == 'TA', 3, 
                                                         ifelse(train_raw$BsmtQual == 'Gd', 4, 
                                                                ifelse(train_raw$BsmtQual == 'Ex', 5,
                                                                       ifelse(train_raw$BsmtQual == 0, 0, NA))))))
        
        train_raw$BsmtCond_num <- ifelse(train_raw$BsmtCond == 'Po', 1,
                                          ifelse(train_raw$BsmtCond == "Fa", 2,
                                                 ifelse(train_raw$BsmtCond == 'TA', 3, 
                                                        ifelse(train_raw$BsmtCond == 'Gd', 4, 
                                                               ifelse(train_raw$BsmtCond == 'Ex', 5,
                                                                      ifelse(train_raw$BsmtCond == 0, 0, NA))))))
        
        
        train_raw$HeatingQC_num <- ifelse(train_raw$HeatingQC == 'Po', 1,
                                          ifelse(train_raw$HeatingQC == "Fa", 2,
                                                 ifelse(train_raw$HeatingQC == 'TA', 3, 
                                                        ifelse(train_raw$HeatingQC == 'Gd', 4, 
                                                               ifelse(train_raw$HeatingQC == 'Ex', 5,NA)))))
        
        train_raw$KitchenQual_num <- ifelse(train_raw$KitchenQual == 'Po', 1,
                                           ifelse(train_raw$KitchenQual == "Fa", 2,
                                                  ifelse(train_raw$KitchenQual == 'TA', 3, 
                                                         ifelse(train_raw$KitchenQual == 'Gd', 4, 
                                                                ifelse(train_raw$KitchenQual == 'Ex', 5,NA)))))
        
        train_raw$FireplaceQu_num <- ifelse(train_raw$FireplaceQu == 'Po', 1,
                                             ifelse(train_raw$FireplaceQu == "Fa", 2,
                                                    ifelse(train_raw$FireplaceQu == 'TA', 3, 
                                                           ifelse(train_raw$FireplaceQu == 'Gd', 4, 
                                                                  ifelse(train_raw$FireplaceQu == 'Ex', 5, 
                                                                         ifelse(train_raw$FireplaceQu == 0, 0,NA))))))
        
        train_raw$GarageQual_num <- ifelse(train_raw$GarageQual == 'Po', 1,
                                             ifelse(train_raw$GarageQual == "Fa", 2,
                                                    ifelse(train_raw$GarageQual == 'TA', 3, 
                                                           ifelse(train_raw$GarageQual == 'Gd', 4, 
                                                                  ifelse(train_raw$GarageQual == 'Ex', 5, 
                                                                         ifelse(train_raw$GarageQual == 0, 0, NA))))))
        
        train_raw$PoolQC_num <- ifelse(train_raw$PoolQC == 'Po', 1,
                                            ifelse(train_raw$PoolQC == "Fa", 2,
                                                   ifelse(train_raw$PoolQC == 'TA', 3, 
                                                          ifelse(train_raw$PoolQC == 'Gd', 4, 
                                                                 ifelse(train_raw$PoolQC == 'Ex', 5,
                                                                        ifelse(train_raw$PoolQC == 0, 0, NA))))))
        
        train_raw$GarageCond_num <- ifelse(train_raw$GarageCond == 'Po', 1,
                                         ifelse(train_raw$GarageCond == "Fa", 1,
                                                ifelse(train_raw$GarageCond == 'TA', 2, 
                                                       ifelse(train_raw$GarageCond == 'Gd', 3, 
                                                              ifelse(train_raw$GarageCond == 'Ex', 3, 
                                                                     ifelse(train_raw$GarageCond == 1, 0, NA))))))
        
        train_raw$BsmtExposure_num <- ifelse(train_raw$BsmtExposure == 'No', 1,
                                         ifelse(train_raw$BsmtExposure == "Mn", 2,
                                                ifelse(train_raw$BsmtExposure == 'Av', 3, 
                                                       ifelse(train_raw$BsmtExposure == 'Gd', 4, 
                                                              ifelse(train_raw$BsmtExposure == 0, 0, NA)))))
        
        train_raw$BsmtFinType1_num <- ifelse(train_raw$BsmtFinType1 == 'Unf', 1,
                                         ifelse(train_raw$BsmtFinType1 == "LwQ", 2,
                                                ifelse(train_raw$BsmtFinType1 == 'Rec', 3, 
                                                       ifelse(train_raw$BsmtFinType1 == 'BLQ', 4, 
                                                              ifelse(train_raw$BsmtFinType1 == 'ALQ', 5, 
                                                                     ifelse(train_raw$BsmtFinType1 == 'GLQ', 6,
                                                                            ifelse(train_raw$BsmtFinType1 == 0, 0, NA)))))))
        
        train_raw$BsmtFinType2_num <- ifelse(train_raw$BsmtFinType2 == 'Unf', 1,
                                              ifelse(train_raw$BsmtFinType2 == "LwQ", 2,
                                                     ifelse(train_raw$BsmtFinType2 == 'Rec', 3, 
                                                            ifelse(train_raw$BsmtFinType2 == 'BLQ', 4, 
                                                                   ifelse(train_raw$BsmtFinType2 == 'ALQ', 5, 
                                                                          ifelse(train_raw$BsmtFinType2 == 'GLQ', 6,
                                                                                 ifelse(train_raw$BsmtFinType2 == 0, 0, NA)))))))
        
        train_raw$Functional_num <- ifelse(train_raw$Functional == 'Sal', 1,
                                              ifelse(train_raw$Functional == "Sev", 2,
                                                     ifelse(train_raw$Functional == 'Maj2', 3, 
                                                            ifelse(train_raw$Functional == 'Maj1', 4, 
                                                                   ifelse(train_raw$Functional == 'Mod', 5, 
                                                                          ifelse(train_raw$Functional == 'Min2', 6,
                                                                                 ifelse(train_raw$Functional == 'Min1', 7,
                                                                                        ifelse(train_raw$Functional == 'Typ', 8, NA))))))))
        
        train_raw$GarageFinish_num <- ifelse(train_raw$GarageFinish == "Unf", 1,
                                            ifelse(train_raw$GarageFinish == 'RFn', 2, 
                                                   ifelse(train_raw$GarageFinish == 'Fin', 3, 0)))
        
        
        train_raw$Fence_num <- ifelse(train_raw$Fence == "MnWw", 1,
                                              ifelse(train_raw$Fence == 'GdWo', 2, 
                                                     ifelse(train_raw$Fence == 'MnPrv', 3, 
                                                            ifelse(train_raw$Fence == 'GdPrv', 4, 0))))
        
        # identified similar neighborhoods by revewing their median monthly sales price over the 4 year time period
        train_raw$Neighborhood_bin <- ifelse(train_raw$Neighborhood == "MeadowV", 6,
                                              ifelse(train_raw$Neighborhood == 'IDOTRR', 4, 
                                                     ifelse(train_raw$Neighborhood == 'BrDale', 4, 
                                                            ifelse(train_raw$Neighborhood == 'OldTown', 5, 
                                                                   ifelse(train_raw$Neighborhood == 'Edwards', 5, 
                                                                          ifelse(train_raw$Neighborhood == 'BrkSide', 5, 
                                                                                 ifelse(train_raw$Neighborhood == 'Blueste', 2,
                                                                                        ifelse(train_raw$Neighborhood == 'SWISU', 2,
                                                                                               ifelse(train_raw$Neighborhood == 'NAmes', 2,
                                                                                                      ifelse(train_raw$Neighborhood == 'NPkVill', 7,
                                                                                                             ifelse(train_raw$Neighborhood == 'Mitchel', 7,
                                                                                                                    ifelse(train_raw$Neighborhood == 'SawyerW', 2,
                                                                                                                           ifelse(train_raw$Neighborhood == 'Gilbert', 3,
                                                                                                                                  ifelse(train_raw$Neighborhood == 'NWAmes', 3,
                                                                                                                                         ifelse(train_raw$Neighborhood == 'Blmngtn', 3,
                                                                                                                                                ifelse(train_raw$Neighborhood == 'CollgCr', 1,
                                                                                                                                                       ifelse(train_raw$Neighborhood == 'ClearCr', 1,
                                                                                                                                                              ifelse(train_raw$Neighborhood == 'Crawfor', 1,
                                                                                                                                                                     ifelse(train_raw$Neighborhood == 'Veenker', 9,
                                                                                                                                                                            ifelse(train_raw$Neighborhood == 'Somerst', 9,
                                                                                                                                                                                   ifelse(train_raw$Neighborhood == 'Timber', 9,
                                                                                                                                                                                          ifelse(train_raw$Neighborhood == 'StoneBr', 10,
                                                                                                                                                                                                 ifelse(train_raw$Neighborhood == 'NoRidge', 8,
                                                                                                                                                                                                        8)))))))))))))))))))))))
        train_raw$total_porchSF <- train_raw$OpenPorchSF + train_raw$EnclosedPorch + train_raw$ScreenPorch + train_raw$X3SsnPorch + train_raw$WoodDeckSF
        
        # drop the columns that are not needed anymore
        drop <- c('BsmtFullBath', 'BsmtHalfBath', 'PavedDrive', 'LotShape', 'LandContour', 'LandSlope', 'Electrical', 
                  'ExterQual', 'ExterCond', 'BsmtQual','BsmtCond', 'HeatingQC', 'KitchenQual', 'FireplaceQu', 'GarageQual', 
                  'PoolQC', 'BsmtExposure', 'BsmtFinType1', 'BsmtFinType2', 'Functional', 'GarageFinish', 'Fence', 
                  'GarageCond', 'GarageType', 'PavedDrive', 'SaleCondition', 'Neighborhood', 'MiscFeature', 'Street', 
                  'GarageCars', 'MSSubClass', 'GarageQual_num', 'BsmtFinSF1', 'BsmtFinSF2', 'BsmtUnfSF', 'OpenPorchSF', 
                  'EnclosedPorch', 'ScreenPorch', 'X3SsnPorch', 'WoodDeckSF') # Fireplace_Qunum, 
        
        train_raw = train_raw[,!(names(train_raw) %in% drop)]
        
        # impute the missing values using the MICE package
        imp.train_raw <- mice(train_raw, m = 1, method='cart', printFlag = FALSE, maxit = 5)

        miceOutput <- complete(imp.train_raw) 
        
       return(miceOutput)
}