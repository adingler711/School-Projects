# This file is to clean the rain data before using it in the hybrid / ensemble model

data_prep <- function(data, outlier_threshold = 2.5) {
        
        raindata$DATE = as.Date(strptime(raindata$DATE, "%Y%m%d"))
        raindata$DATE_ym = as.Date(as.yearmon(raindata$DATE, '%Ym%'))
        raindata$DATE_m = as.numeric(format(raindata$DATE_ym,"%m"))
        
        #aggregate the rain data to monthly rainfail per month instead of daily
        rain_Monthly_Aggregate <- aggregate(data = raindata, PRCP ~ DATE_ym, FUN = sum)
        DATE_m <- as.numeric(format(raindata$DATE_ym,"%m"))
        rain_Monthly_Aggregate$days_in_month <- aggregate(data = raindata, PRCP ~ DATE_ym, FUN = length)[,2]
        rain_Monthly_Aggregate$avg_rain_per_day <- rain_Monthly_Aggregate$PRCP / rain_Monthly_Aggregate$days_in_month
             
        # For each month, find the months mean and standard deviation of rainfall
        jan <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 1,]
        feb <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 2,]
        mar <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 3,]
        apr <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 4,]
        may <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 5,]
        jun <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 6,]
        jul <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 7,]
        aug <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 8,]
        sep <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 9,]
        oct <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 10,]
        nov <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 11,]
        dec <- rain_Monthly_Aggregate[as.numeric(strftime(rain_Monthly_Aggregate$DATE_ym, "%m")) %in% 12,]
        jan_top <- mean(jan$avg_rain_per_day) + (outlier_threshold * sd(jan$avg_rain_per_day))
        feb_top <- mean(feb$avg_rain_per_day) + (outlier_threshold * sd(feb$avg_rain_per_day))
        mar_top <- mean(mar$avg_rain_per_day) + (outlier_threshold * sd(mar$avg_rain_per_day))
        apr_top <- mean(apr$avg_rain_per_day) + (outlier_threshold * sd(apr$avg_rain_per_day))
        may_top <- mean(may$avg_rain_per_day) + (outlier_threshold * sd(may$avg_rain_per_day))
        jun_top <- mean(jun$avg_rain_per_day) + (outlier_threshold * sd(jun$avg_rain_per_day))
        jul_top <- mean(jul$avg_rain_per_day) + (outlier_threshold * sd(jul$avg_rain_per_day))
        aug_top <- mean(aug$avg_rain_per_day) + (outlier_threshold * sd(aug$avg_rain_per_day))
        sep_top <- mean(sep$avg_rain_per_day) + (outlier_threshold * sd(sep$avg_rain_per_day))
        oct_top <- mean(oct$avg_rain_per_day) + (outlier_threshold * sd(oct$avg_rain_per_day))
        nov_top <- mean(nov$avg_rain_per_day) + (outlier_threshold * sd(nov$avg_rain_per_day))
        dec_top <- mean(dec$avg_rain_per_day) + (outlier_threshold * sd(dec$avg_rain_per_day))
        
        # using the innputed outlier_threshold trim the outlier months to adjust for unusual rainfall
        trimmed <- matrix(0,length(rain_Monthly_Aggregate$avg_rain_per_day),1)
        for(i in 1:length(rain_Monthly_Aggregate$avg_rain_per_day)) { 
                
                temp <- rain_Monthly_Aggregate[i,]
                
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 9) {
                                trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > sep_top, sep_top)
                }
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 10) {
                                trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > oct_top, oct_top)
                }
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 11) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > nov_top, nov_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 12) {
                                trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > dec_top, dec_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 1) {
                                trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > jan_top, jan_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 2) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > feb_top, feb_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 3) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > mar_top, mar_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 4) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > apr_top, apr_top)
                }
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 5) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > may_top, may_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 6) {
                       trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > jun_top, jun_top)
                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 7) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > jul_top, jul_top)

                } 
                if (as.numeric(strftime(temp$DATE_ym, "%m")) %in% 8) {
                        trimmed[i] <- replace(temp$avg_rain_per_day, temp$avg_rain_per_day > aug_top, aug_top)
                }     
        }
        rain_Monthly_Aggregate$PRCP_avg_trimmed <- as.numeric(trimmed)
        return(rain_Monthly_Aggregate)
}