library(fma)
library(fpp)
library(forecast)

jobs_companies_d <- read.csv("datasets/jobs&HousingIndex&Demo.csv")
jobs_companies_d["location"] <- NA
jobs_companies_d$location <- with(jobs_companies_d, paste0(City, State))
jobs_companies_d <- jobs_companies_d[order(jobs_companies_d$Year),]
head(jobs_companies_d)
test_d <- data.frame(jobs_companies_d[which(jobs_companies_d$Year > 2012),]) 
train_d <- data.frame(jobs_companies_d[which(jobs_companies_d$Year < 2013),])
train_d
xreg <- cbind(train_d$Home.Value.Index, train_d$households, train_d$median_household_income)
y <- cbind(train_d$Year, train_d$Num.of.Jobs)

f_cast <- list()
for(loc in train_d$location) {
  xreg_d <- NULL
  city_d <- NULL
  y_d <- NULL
  y <- NULL
  fcast <- NULL
  fit.model <- NULL
  count <- 0  
  city_d <- train_d[which(train_d$location == loc),]
  if(nrow(city_d) == 6){
    xreg_d <- ts(cbind(city_d$Home.Value.Index, city_d$households, city_d$median_household_income))
    y_d <- city_d$Num.of.Jobs
    y <- ts(y_d, start = 2007)
    plot(y)
    fit.model <- auto.arima(y, xreg = xreg_d)
    fcast <- forecast(fit.model, h = 3)
    f_cast.append(fcast)
  }
}

f_cast
