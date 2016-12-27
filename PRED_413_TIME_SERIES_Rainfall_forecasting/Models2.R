
fitControl <- trainControl(
        method = "repeatedcv",
        number = 5, 
        ## repeated 5 times
        repeats = 5, 
        allowParallel = TRUE)#,
        ## Adaptive resampling information:
        #adaptive = list(min = 10, 
        #                alpha = 0.05, 
        #                method = "gls",
        #                complete = TRUE))

fit_gbm <- function(data2, test, test_model) {
        
        #data2 <- train_data
        gbmGrid <-  expand.grid(interaction.depth = 25, #c(5:30), # 12 old; 25 was selected as optimal
                                n.trees = 4000,# c(5000, 10000), #20000, 30000, 40000, 50000), 
                                shrinkage = 0.01, #c(0.01, 0.005),
                                n.minobsinnode = 7) #c(1, 3, 5, 7, 10))
        
        gbmFit <- train(Log_price_new ~ ., data = data2,
                        method = "gbm",
                        trControl = fitControl,
                        preProc = c("center", "scale"),
                        ## Now specify the exact models 
                        ## to evaluate:
                        tuneGrid = gbmGrid,
                        maximize=FALSE)
        
        #trellis.par.set(caretTheme()) # RMSE: 0.1180177 Rsquared: 0.8063755
        #plot(gbmFit)
        
        #gbm_pred <- predict(gbmFit, test)
        #gbm_pred2 <- (exp(gbm_pred) * test_model$GrLivArea) + test_model$MiscVal
        
        return(gbmFit)
}

fit_foba <- function(data, test_foba, test_model) {
        
        #data <- train_foba
        cols <- names(data)
        data_types <- sapply(cols,function(x){class(data[[x]])})
        unique_data_types <- unique(data_types)
        # Separate attributes by data type
        DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
        names(DATA_ATTR_TYPES) <- unique_data_types
        
        keep <- c(DATA_ATTR_TYPES$integer, DATA_ATTR_TYPES$numeric, 'Log_price_new')
        house_w_features_noFactor = data[,(names(data) %in% keep)]
        test_noFactor <- test_foba[,(names(test_foba) %in% keep)]
        
        trainxDrop <- c("Log_price_new")
        trainX <-  house_w_features_noFactor[, ! names(house_w_features_noFactor) %in% trainxDrop, drop = F]
        trainY <- house_w_features_noFactor$Log_price_new
        
        fobaGrid <-  expand.grid(lambda = 5e-5, #c(1e-5, 1e-6, 1e-4)*5,
                                 k = 37)#c(60:100)) #c(30:55))
        
        fobaFit2 <- train(x = trainX, y = trainY,
                          method = "foba",
                          trControl = fitControl,
                          preProc = c("center", "scale"),
                          ## Now specify the exact models 
                          ## to evaluate:
                          tuneGrid = fobaGrid)
        
        #trellis.par.set(caretTheme())
        #plot(fobaFit2) #  0.1208971  0.7945351
        
        #foba_pred <- predict(fobaFit2, test_noFactor)
        #foba_pred2 <- (exp(foba_pred) * test_model$GrLivArea) + test_model$MiscVal
        
        return(fobaFit2)
}


fit_knn <- function(data, test, test_model) {
        #data <- train_data
        cols <- names(data)
        data_types <- sapply(cols,function(x){class(data[[x]])})
        unique_data_types <- unique(data_types)
        # Separate attributes by data type
        DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
        names(DATA_ATTR_TYPES) <- unique_data_types
        
        keep <- c(DATA_ATTR_TYPES$integer, DATA_ATTR_TYPES$numeric, 'Log_price_new')
        house_w_features_noFactor = data[,(names(data) %in% keep)]
        test_noFactor <- test[,(names(test) %in% keep)]
        
        trainxDrop <- c("Log_price_new")
        trainX <-  house_w_features_noFactor[, ! names(house_w_features_noFactor) %in% trainxDrop, drop = F]
        trainY <- house_w_features_noFactor$Log_price_new
        
        knnGrid <-  expand.grid(k = 10)# c(5:30))
        

        knnFit <- train(trainX, trainY,
                        method = "knn",
                        trControl = fitControl,
                        preProc = c("center", "scale"),
                        tuneGrid = knnGrid)
        
        #maximize=FALSE)
        #trellis.par.set(caretTheme()) # 0.1538953, 
        #plot(knnFit)
        
        #knn_pred <- predict(knnFit, test_noFactor)
        #knn_pred2 <- (exp(knn_pred) * test_model$GrLivArea) + test_model$MiscVal
        
        return(knnFit)
}


fit_xgb <- function(data2, test, test_model) {
        #data2 <- train_data
        xgb_GRID <-  expand.grid(nrounds= 1500,  # 1300 -> 1500, 100
                                 max_depth= 3,# c(1:5), 
                                 eta= 0.05, #c(1:10)/100 , # learning rate
                                 gamma= 0, #c(1:10)/100 ,
                                 colsample_bytree= 0.205, #(10:30)/100, # 0.21
                                 min_child_weight = 9, #c(1:10),
                                 subsample = 1)
        

        xgboost <- train(Log_price_new ~ ., data = data2,
                         method = "xgbTree",
                         trControl = fitControl,
                         verbose = FALSE,
                         preProc = c("center", "scale"),
                         ## Now specify the exact models 
                         ## to evaluate:
                         tuneGrid = xgb_GRID,
                         maximize=FALSE)
        
        #trellis.par.set(caretTheme()) # RMSE: 0.1228894  Rsquared: 0.7880107
        #plot(xgboost)
        
        #xgb_pred <- predict(xgboost, test)
        #xgb_pred2 <- (exp(xgb_pred) * test_model$GrLivArea) + test_model$MiscVal
        
        return(xgboost)
}


fit_rf <- function(data2, test, test_model) {
        
        #data2 <- train_data
        rf.grid <- data.frame(.mtry = 53)#c(1:55))#,
        #.maxdepth = c(10:15)) # mtry = 55
        
        rf.tune.1 <- train(Log_price_new ~ ., data = data2,
                           method = "rf", # rfRules
                           metric = "RMSE",
                           trControl = fitControl,
                           tuneGrid = rf.grid,
                           maximize=FALSE)
        
        #trellis.par.set(caretTheme())
        #plot(fobaFit2)
        
        #rf_pred <- predict(rf.tune.1, test)
        #rf_pred2 <- (exp(rf_pred) * test_model$GrLivArea) + test_model$MiscVal
        
        return(rf.tune.1)
}

fit_rpart <- function(data2, test, test_model) {
        #data2 <- train_data
        rpart.grid <- data.frame(maxdepth = 14)#c(10:15)) 
        
        rpartFit <- train(Log_price_new ~ ., data = data2,
                          method = "rpart2",
                          metric = "RMSE",
                          trControl = fitControl,
                          tuneGrid = rpart.grid,
                          maximize=FALSE)
        
        #rpart_pred <- predict(rpartFit, test)
        #rpart_pred2 <- exp(rpart_pred) * test_model$GrLivArea + test_model$MiscVal
        
        return(rpartFit)
}


fit_lm <- function(data2, test, test_model) {
        
        #data2 <- model      
        set.seed(711)
        lmFit <- train(Log_price_new ~ ., data = data2,
                       method = "lmStepAIC")
        #metric = "RMSE",
        #trControl = fitControl,
        #tuneGrid = rpart.grid,
        #maximize=FALSE)
        
        #lm_pred <- predict(lmFit, test)        
        #lm_pred2 <- (exp(lm_pred) * test_model$GrLivArea) + test_model$MiscVal
        # RMSE: 0.1222668  
        # Rsquared: 0.7934283
        # residuals
        
        #mylm=lm(Log_price_new_SF~., data=data2)
        #mystep1=stepAIC(mylm, direction="both")
        
        #qqnorm(residuals(mystep1))
        #qqline(residuals(mystep1))
        
        # fitted vs residuals
        #plot(fitted.values(mystep1),residuals(mystep1))
        #abline(h=0)
        #abline(h=2*sqrt(5511),col="red")
        #abline(h=-2*sqrt(5511),col="red")
        #cook <- cooks.distance(mystep1)
        #plot(cook)
        #rstudents <- rstudent(mystep1)
        #rstudents[rstudents > 3]
        #rstudents[rstudents < -3] # 589: -5.049094 Rstudent
        
        return(lmFit)
}


#cv.values <- resamples(list(bench_gbm = gbmFit,
#                            bench_xgboost = xgboost,
#                            bench_rf = rf.tune.1,
#                            bench_ranger = ranger_fit
#))
#dotplot(cv.values, metric = "RMSE", main="Resamples Model 1")
#modelCor(cv.values)
