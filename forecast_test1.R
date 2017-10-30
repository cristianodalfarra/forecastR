# http://code.markedmondson.me/googleAnalyticsR/setup.html
# http://code.markedmondson.me/googleAnalyticsR/v4.html

# https://github.com/cristianodalfarra/forecastR


## setup
library(googleAnalyticsR)

## This should send you to your browser to authenticate your email.
## Authenticate with an email that has access to the Google Analytics View you want to use.
ga_auth()

## get your accounts
account_list <- google_analytics_account_list()
#ga_id <- account_list[GA_ID,'viewId']
## test

ga_id <- 43015466

library(highcharter)
library(googleAnalyticsR)
library(forecast)

#########  GA data

gadata = NULL
gadata <- google_analytics_4(ga_id, 
                             date_range = c("2015-01-01", "2017-10-30"),
                             metrics = "users", 
                             dimensions = c("yearMonth"),
                             max = -1)
gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')



# Convert the data to be officially "time-series" data
ga_ts <- ts(gadata[2], start = c(2015,01), end = c(2017,10), frequency = 12)



#######test1
# Compute the Holt-Winters filtering for the data
#forecast1 <- HoltWinters(ga_ts_aggr)
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 12 months of the blog sessions
hchart(forecast(forecast1, h = 12))
df <- summary(f)

#######test2
library(forecast)
f <- forecast(ga_ts)
hchart(forecast(f, h = 12))
df <- summary(f)

############################################## PROPHETE FACEBOOK RESEARCH ####################
# http://pbpython.com/prophet-overview.html
# https://cran.r-project.org/web/packages/prophet/vignettes/quick_start.html
##########test3 prophete
library(forecast)
gadata = NULL
gadata <- google_analytics_4(ga_id, 
                             date_range = c("2015-01-01", "2017-10-30"),
                             metrics = "pageviews", 
                             dimensions = c("yearMonth"),
                             max = -1)
gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
colnames(gadata)[1] <- "ds"
colnames(gadata)[2] <- "y"

library(prophet)

m <- prophet(gadata)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
plot(m, forecast)
prophet_plot_components(m, forecast)


# https://github.com/facebook/prophet/issues/275

future <- make_future_dataframe(m, periods = 12)

dfTs <- xts(df, as.Date(paste("01", rownames(df)), "%d %b %Y"))

### test only################################

vv = boxplot.stats(validi$conv3[which(validi$category =="CASE")])$out

###################################################

#temp <- data_fetch_all[ which(data_fetch_all$date >= today-15),]


# I was running into this error as well. Installing rstan and Rcpp on my mac machine fixed it:
#   install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
# install.packages(c("Rcpp", "rstan"), type = "source")


library(dplyr)
df <- read.csv('C:/Users/Utente/Downloads/file1.csv') %>%
  mutate(y = log(y))
m <- prophet(df)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)
