# http://code.markedmondson.me/googleAnalyticsR/setup.html
# http://code.markedmondson.me/googleAnalyticsR/v4.html

# https://github.com/cristianodalfarra/forecastR



# #########  GA data
# 
# gadata = NULL
# gadata <- google_analytics_4(ga_id, 
#                              date_range = c("2015-01-01", "2017-10-30"),
#                              metrics = "pageviewsPerSession", 
#                              dimensions = c("yearMonth"),
#                              max = -1)
# gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
# 
# 
# 
# # Convert the data to be officially "time-series" data
# ga_ts <- ts(gadata[2], start = c(2015,01), end = c(2017,10), frequency = 12)
# 
# 
# 
# #######test1
# # Compute the Holt-Winters filtering for the data
# #forecast1 <- HoltWinters(ga_ts_aggr)
# forecast1 <- HoltWinters(ga_ts)
# # Generate a forecast for next 12 months of the blog sessions
# hchart(forecast(forecast1, h = 14))
# df <- summary(f)
# 
# #######test2
# library(forecast)
# f <- forecast(ga_ts)
# hchart(forecast(f, h = 14))
# df <- summary(f)
# yhat = df[1]
# write.csv(yhat, file = "MyData.csv")
############################################## PROPHETE FACEBOOK RESEARCH ####################
# http://pbpython.com/prophet-overview.html
# https://cran.r-project.org/web/packages/prophet/vignettes/quick_start.html
##########test3 prophete
# library(forecast)
# gadata = NULL
# gadata <- google_analytics_4(ga_id, 
#                              date_range = c("2015-01-01", "2017-10-30"),
#                              metrics = "pageviews", 
#                              dimensions = c("date"),
#                              max = -1)
# #gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
# colnames(gadata)[1] <- "ds"
# colnames(gadata)[2] <- "y"
# 
# library(prophet)
# 
# m <- prophet(gadata)
# future <- make_future_dataframe(m, periods = 365)
# forecast <- predict(m, future)  #forecast
# plot(m, forecast)
# prophet_plot_components(m, forecast)
# p =  summary(forecast)
# 
# # https://github.com/facebook/prophet/issues/275



### test only################################

# vv = boxplot.stats(validi$conv3[which(validi$category =="CASE")])$out

###################################################

#temp <- data_fetch_all[ which(data_fetch_all$date >= today-15),]


# I was running into this error as well. Installing rstan and Rcpp on my mac machine fixed it:
#   install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies=TRUE)
# install.packages(c("Rcpp", "rstan"), type = "source")

# 
# library(dplyr)
# df <- read.csv('C:/Users/Utente/Downloads/file1.csv') %>%
#   mutate(y = log(y))
# m <- prophet(df)
# future <- make_future_dataframe(m, periods = 365)
# forecast <- predict(m, future)
# 
# 
# pageviewsPerSession


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


##################################################
######## funzione di estrazione e calcolo#########
##################################################

## setup

ga.data.extract <- function(metrica) {
  
  
  gadata = NULL
  gadata <- google_analytics_4(ga_id, 
                               date_range = c("2015-01-01", "2017-10-30"),
                               metrics = metrica, 
                               dimensions = c("yearMonth"),
                               max = -1)
  gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
  
  
  
  # Convert the data to be officially "time-series" data
  ga_ts <- ts(gadata[2], start = c(2015,01), end = c(2017,10), frequency = 12)
  
  forecast1 <- HoltWinters(ga_ts)
  # Generate a forecast for next 12 months of the blog sessions
  p= hchart(forecast(forecast1, h = 30)) %>%
    hc_title(text = metrica )
  df <- summary(forecast1)
  return(list(gadata,p))
  }

# metriche https://developers.google.com/analytics/devguides/reporting/core/dimsmets#q=time&mode=web&cats=user,session,traffic_sources,adwords,goal_conversions,platform_or_device,geo_network,system,social_activities,page_tracking,content_grouping,internal_search,site_speed,app_tracking,event_tracking,ecommerce,social_interactions,user_timings,exceptions,content_experiments,custom_variables_or_columns,time,doubleclick_campaign_manager,audience,adsense,ad_exchange,doubleclick_for_publishers,doubleclick_for_publishers_backfill,lifetime_value_and_cohorts,channel_grouping,related_products,doubleclick_bid_manager,doubleclick_search

##########################################################
gadata = NULL
ga.data.extract("sessions")
ga.data.extract("users")
ga.data.extract("pageviews")
ga.data.extract("pageviewsPerSession")
ga.data.extract("bounceRate")
ga.data.extract("avgSessionDuration")



############################################################
####################### risposta eventi#####################
############################################################
campaign_filter <- dim_filter(dimension="eventAction",operator="REGEXP",expressions="rispo")

my_filter_clause <- filter_clause_ga4(list(campaign_filter))

gadata <- google_analytics_4(ga_id,date_range = c("2015-01-01","2017-10-30"),
                                 metrics = c("totalEvents"),
                                 dimensions = c("yearMonth"),
                                 dim_filters = my_filter_clause,
                                 anti_sample = FALSE)

gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')



# Convert the data to be officially "time-series" data
ga_ts <- ts(gadata[2], start = c(2015,01), end = c(2017,10), frequency = 12)

forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 12 months of the blog sessions
hchart(forecast(forecast1, h = 30)) %>%
  hc_title(text = "risposta" )
df <- summary(forecast1)

#############################################################
##@@@@@@@@@@@@@@@ telefono eventi HW ########################
#############################################################
campaign_filter <- dim_filter(dimension="eventAction",operator="REGEXP",expressions="telef")

my_filter_clause <- filter_clause_ga4(list(campaign_filter))

gadata <- google_analytics_4(ga_id,date_range = c("2015-01-01","2017-10-30"),
                             metrics = c("totalEvents"),
                             dimensions = c("yearMonth"),
                             dim_filters = my_filter_clause,
                             anti_sample = FALSE)

gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
# Convert the data to be officially "time-series" data
ga_ts <- ts(gadata[2], start = c(2015,01), end = c(2017,10), frequency = 12)
forecast1 <- HoltWinters(ga_ts)
# Generate a forecast for next 12 months of the blog sessions
hchart(forecast(forecast1, h = 14)) %>%
  hc_title(text = "telefono" )
p <- predict(forecast1, 14, prediction.interval = TRUE)
plot(forecast1, p)


############################################################
############# telefono eventi PROPHETE #####################
############################################################
campaign_filter <- dim_filter(dimension="eventAction",operator="REGEXP",expressions="telef")

my_filter_clause <- filter_clause_ga4(list(campaign_filter))


gadata = NULL
gadata <- google_analytics_4(ga_id,date_range = c("2015-01-01","2017-10-30"),
                             metrics = c("totalEvents"),
                             dimensions = c("date"),
                             dim_filters = my_filter_clause,
                             anti_sample = FALSE)
#gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
colnames(gadata)[1] <- "ds"
colnames(gadata)[2] <- "y"

library(prophet)

m <- prophet(gadata)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)  #forecast
plot(m, forecast)
prophet_plot_components(m, forecast)
p =  summary(forecast)
############################################################
########### risposta eventi PROPHETE #######################
############################################################
campaign_filter <- dim_filter(dimension="eventAction",operator="REGEXP",expressions="rispost")

my_filter_clause <- filter_clause_ga4(list(campaign_filter))


gadata = NULL
gadata <- google_analytics_4(ga_id,date_range = c("2015-01-01","2017-10-30"),
                             metrics = c("totalEvents"),
                             dimensions = c("date"),
                             dim_filters = my_filter_clause,
                             anti_sample = FALSE)
#gadata[1]=as.Date(paste0(as.character(gadata$yearMonth), '01'), format='%Y%m%d')
colnames(gadata)[1] <- "ds"
colnames(gadata)[2] <- "y"

gadata= gadata[gadata[,2] < 100000,] 
library(prophet)
m <- prophet(gadata)
future <- make_future_dataframe(m, periods = 365)
forecast <- predict(m, future)  #forecast
plot(m, forecast)
prophet_plot_components(m, forecast)
p =  summary(forecast)



