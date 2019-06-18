#Meteomatics Weather API Connector

#Set working directory
setwd("") #edit your own path
#Connecting with the query_api_time.R
source('query_api_PowerBI.R')

#Choose if timeseries or domain
request_type = "timeseries" #"domain","timeseries"

#Data
username = "powerbi-community"
password = "Alepafume675"

time_zone = "Europe/Berlin"
startdate = ISOdatetime(year = strtoi(strftime(today(),'%Y')),
                        month = strtoi(strftime(today(),'%m'), 10),
                        day = strtoi(strftime(today(),'%d'), 10),
                        hour = 00, min = 00, sec = 00, tz = "UTC")
enddate = ISOdatetime(year = strtoi(strftime(today(),'%Y')),
                      month = strtoi(strftime(today(),'%m'), 10),
                      day = strtoi(strftime(today(),'%d'), 10)+1,
                      hour = 00, min = 00, sec = 00, tz = "UTC")
interval = "PT1H"

if (request_type == "timeseries"){
  parameters = "t_2m:C,relative_humidity_1000hPa:p" #different parameters
  coordinate = "47.11,11.47" #Point and line
} else {
  #startdate is used
  parameters = "t_2m:C" #only one parameter
  coordinate = "47.9,5.7_45.8,10.7:0.1,0.1" #Rectangle
}

#Data from the API
output = query_api(username, password, startdate, enddate, interval, parameters, coordinate, time_zone = time_zone)
