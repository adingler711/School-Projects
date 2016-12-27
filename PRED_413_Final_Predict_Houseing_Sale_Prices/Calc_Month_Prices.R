# calcualte the median monthly sales price per neighborhood
month_prices <- function(homes) {

        train_data <- read.table("train.csv", header = TRUE, sep=',')
        # go back and change this to compare samples with normalized price -- wilcox test
        train_data$Neighborhood_bin <- ifelse(train_data$Neighborhood == "MeadowV", 6,
                                             ifelse(train_data$Neighborhood == 'IDOTRR', 4, 
                                                    ifelse(train_data$Neighborhood == 'BrDale', 4, 
                                                           ifelse(train_data$Neighborhood == 'OldTown', 5, 
                                                                  ifelse(train_data$Neighborhood == 'Edwards', 5, 
                                                                         ifelse(train_data$Neighborhood == 'BrkSide', 5, 
                                                                                ifelse(train_data$Neighborhood == 'Blueste', 2,
                                                                                       ifelse(train_data$Neighborhood == 'SWISU', 2,
                                                                                              ifelse(train_data$Neighborhood == 'NAmes', 2,
                                                                                                     ifelse(train_data$Neighborhood == 'NPkVill', 7,
                                                                                                            ifelse(train_data$Neighborhood == 'Mitchel', 7,
                                                                                                                   ifelse(train_data$Neighborhood == 'SawyerW', 2,
                                                                                                                          ifelse(train_data$Neighborhood == 'Gilbert', 3,
                                                                                                                                 ifelse(train_data$Neighborhood == 'NWAmes', 3,
                                                                                                                                        ifelse(train_data$Neighborhood == 'Blmngtn', 3,
                                                                                                                                               ifelse(train_data$Neighborhood == 'CollgCr', 1,
                                                                                                                                                      ifelse(train_data$Neighborhood == 'ClearCr', 1,
                                                                                                                                                             ifelse(train_data$Neighborhood == 'Crawfor', 1,
                                                                                                                                                                    ifelse(train_data$Neighborhood == 'Veenker', 9,
                                                                                                                                                                           ifelse(train_data$Neighborhood == 'Somerst', 9,
                                                                                                                                                                                  ifelse(train_data$Neighborhood == 'Timber', 9,
                                                                                                                                                                                         ifelse(train_data$Neighborhood == 'StoneBr', 10,
                                                                                                                                                                                                ifelse(train_data$Neighborhood == 'NoRidge', 8,
                                                                                                                                                                                                       8)))))))))))))))))))))))
        
        
        
        
        
        
        
        #boxplot(formula = SalePrice ~ Neighborhood_bin, data = train_data)
        train_data <- train_data[(train_data$SalePrice < 700000), ]
        #train_data <- train_data[train_data$GrLivArea < 4000, ]
        outlierList <- c(186, 1389)
        train_data <- train_data[!train_data$Id %in% outlierList ,]
        #train_data <- train_data[(train_data$FullBath > 0), ] # 9 houses in test and only 3 in train have 0 FullBaths
        #train_data <- train_data[(train_data$LotArea < 60000), ] # no lot in the test data set in >~ 50K
        
        # train_data[(train_data$SalePrice)  > 400000 & (train_data$Neighborhood_bin == 5), ] # id = 186
        # train_data[((train_data$SalePrice > 350000) & (train_data$Neighborhood_bin == 3)), ] # id = 1389
        
        train_data$price_min_misc = train_data$SalePrice - train_data$MiscVal
        train_data$price_new_SF = train_data$price_min_misc / train_data$GrLivArea
        train_data$Log_price_new_SF <- log(train_data$price_new_SF)
        
        train_data$Dates = paste(train_data$MoSold, 1, train_data$YrSold, sep="/")
        train_data$new_date <- as.Date(train_data$Dates, "%m/%d/%Y")
        cols.dont.want <- "Dates"
        train_data <- train_data[, ! names(train_data) %in% cols.dont.want, drop = F]
        
        ts_data <- data.frame(train_data$Log_price_new_SF, train_data$new_date, train_data$Neighborhood_bin)
        colnames(ts_data) <- c("Log_price_new_SF", "new_date", 'Neighborhood_bin')
        
        ts_date_groupby <- aggregate(data = ts_data, ts_data$Log_price_new_SF ~ ts_data$Neighborhood_bin, FUN = median) 
        colnames(ts_date_groupby) <- c('Neighborhood_bin', 'Log_price_new_SF')
        
        homes_data = homes
        homes_data$Dates = paste(homes_data$MoSold, 1, homes_data$YrSold, sep="/")
        homes_data$new_date <- as.Date(homes_data$Dates, "%m/%d/%Y")
        cols.dont.want <- "Dates"
        homes_data <- homes_data[, ! names(homes_data) %in% cols.dont.want, drop = F]
        date_list <- unique(as.list(ts_date_groupby$new_date))
        Neighborhood_list <- unique(as.list(ts_date_groupby$Neighborhood_bin))
        
        test <- data.frame(homes_data$new_date, homes_data$Id, homes_data$Neighborhood_bin)
        colnames(test) <- c("Date", "Id", "Neighborhood_bin")
          
        results <- data.frame()
        for (Neighborhood in Neighborhood_list) {
                data_filted <- (test[test$Neighborhood_bin == Neighborhood, ])
                med_price <- ts_date_groupby[ts_date_groupby$Neighborhood_bin == Neighborhood, ]$Log_price_new_SF
                med_price <- rep(med_price, length(data_filted[,1]))
                med_price_list = cbind(data_filted, med_price)  
                results <- rbind(results, med_price_list)                 
        }
       
        results_df <- data.frame(results$Id, results$med_price)
        colnames(results_df) <- c("Id", "medPrice_Neighborhood")
        test_merged <- merge(x = homes, y = results_df, by = "Id", all.x = TRUE)

        return(test_merged)
}
