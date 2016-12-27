remove_factors <- function(data) {
        
        #data <- train_data
        cols <- names(data)
        data_types <- sapply(cols,function(x){class(data[[x]])})
        unique_data_types <- unique(data_types)
        # Separate attributes by data type
        DATA_ATTR_TYPES <- lapply(unique_data_types,function(x){ names(data_types[data_types == x])})
        names(DATA_ATTR_TYPES) <- unique_data_types
        
        keep <- c(DATA_ATTR_TYPES$integer, DATA_ATTR_TYPES$numeric, 'Log_price_new_SF')
        house_w_features_noFactor = data[,(names(data) %in% keep)]
        
        return(house_w_features_noFactor)
}