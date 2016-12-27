

remove_outliers <- function(data) {
        # remove outliers -- also get them from the regression approach
        data <- data[data$GrLivArea < 4000, ]
        #data <- data[(data$SalePrice < 700000), ]
        #data <- data[(data$FullBath > 0), ] # 9 houses in test and only 3 in train have 0 FullBaths
        data <- data[(data$LotArea < 60000), ] # no lot in the test data set in >~ 50K
        #outlierList <- c(1325, 31, 463, 633, 969, 589)
        #data <- data[!data$Id %in% outlierList, ]
        
        return(data)
}

