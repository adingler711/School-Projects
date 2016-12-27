library(forecast)
library(forecastHybrid)
source("prepare_data.R") # cleanse the data

raindata <- read.table("competitiondata.csv", header=TRUE, sep=',')
rain_cleaned <- data_prep(raindata, outlier_threshold = 3.0) # threshold is in standard deviations

# convert the monthly rainfall to a time series data format
rain_Monthly_Aggregate_TS_train <- ts(rain_cleaned$PRCP_avg_trimmed, frequency = 12, start = c(1946, 9))

# create indicator variables for the in-sample data set
fit_months <- seq(as.Date("1946-09-01"), as.Date("2014-07-31"), by="month")
fit_months_df <- data.frame(fit_months)
fit_months_df$month <- format(fit_months_df, '%m')
fit_months_df$year <- format(fit_months_df, '%Y')
months <- data.matrix(fit_months_df$month)
year <- data.matrix(fit_months_df$year)

# create indicator variables for the out of sample data set
pred_months <- seq(as.Date("2014-08-01"), as.Date("2016-07-31"), by="month")
pred_months_df <- data.frame(pred_months)
pred_months_df$month <- format(pred_months_df, '%m')
pred_indicator_months <- data.matrix(pred_months_df$month)
pred_months_df$year <- format(pred_months_df, '%Y')
pred_indicator_year <- data.matrix(pred_months_df$year)

##### predicting the hold out set, i.e., next 24 months
lambda_BoxCox <- BoxCox.lambda(rain_Monthly_Aggregate_TS_train)

z <- fourier(ts(rain_Monthly_Aggregate_TS_train, frequency=12), K=5)
zf <- fourier(ts(rain_Monthly_Aggregate_TS_train, frequency=12), K=5, h=24)

### add xreg lags as inputs to some of the models
quickModel <- hybridModel(y = rain_Monthly_Aggregate_TS_train, models = "aest", lambda = lambda_BoxCox,
                          weights = "cv.errors", errorMethod = 'RMSE',
                          #weights = "insample.errors", errorMethod = 'MASE',
                          a.args = list(max.p = 12, max.q = 12, approximation = FALSE, 
                                        allowdrift=TRUE, ic = 'aicc', xreg=data.frame(z, months)),
                          e.args = list(ic = 'aicc'),# opt.crit = "amse", nmse = 24, model = "ZZZ"),
                          #n.args = list(repeats = 50, decay = 0.9, maxit=300, reg=data.frame(z, months)), 
                          s.args = list(robust = TRUE, s.window = 3, method = "arima", xreg=data.frame(z, months)),
                          t.args = list(seasonal.periods = 3), # use.arma.errors = FALSE
                          cvHorizon = 24, windowSize = 34,
                          horizonAverage = TRUE) 

quick_fcast <- forecast(quickModel, h=24, lambda = lambda_BoxCox, xreg = data.frame(zf, pred_indicator_months))

### daily forecast to month prediction ###
pred_days2 <- seq(as.Date("2014-08-01"), as.Date("2016-07-31"), by="days")
pred_days_df2 <- data.frame(pred_days2)
pred_days_df2$ym <- as.Date(as.yearmon(pred_days_df2$pred_days2, '%Ym%'))
pred_toMonth_mult2 <- aggregate(data = pred_days_df2, pred_days2 ~ ym, FUN = length)[,2]
pred_month2 <- quick_fcast$mean * pred_toMonth_mult2

# review the correlation of the resulting models
#quick_model_outputs <- quick_fcast$pointForecasts * pred_toMonth_mult2
#cor(quick_model_outputs)

raindata_submission <- read.table("submissionshell.csv", header=TRUE, sep=',')
raindata_submission$Forecast <- round(pred_month2, 2)
write.csv(raindata_submission, file = "raindata_submission_quickmodelC.csv", row.names=FALSE)
