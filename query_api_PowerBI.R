#Program for Meteomatics_Weather_API
#Packages
library(httr)
library(data.table)
library(lubridate)

#def variable
startdate_query = strftime(startdate,format='%Y-%m-%dT%H:%M:%OSZ', tz = "UTC")
enddate_query = strftime(enddate,format='%Y-%m-%dT%H:%M:%OSZ', tz = "UTC")
model = {}
ensSelect = {}

#URL
query = if(request_type == "timeseries")
{
  sprintf("https://%s:%s@api.meteomatics.com/%s--%s:%s/%s/%s/csv", username, password, startdate_query, enddate_query, interval, parameters, coordinate)
} else {
  sprintf("https://%s:%s@api.meteomatics.com/%s/%s/%s/csv", username, password, startdate_query, parameters, coordinate)
}

#Data for PowerBI
if (request_type == "timeseries"){
#Timeseries
  #Data from the API
  api_timeseries = function(path)
  {
    resp1 = GET(query)
    con = textConnection(content(resp1,"text"))
    parsed1 = read.csv(con, sep = ";")
    structure(
      list(content = parsed1)
    )
  }
  
  result = api_timeseries("/(startdate_query)---(enddate_query):(interval)/(parameters)/(coordinate)/csv?")
  
  #Data
  query_api = function(username, password, startdate, enddate, interval, parameters, coordinate)
  {
    df = as.data.frame(result$content)
    #Numbers of col and row
    r = nrow(df)
    c = ncol(df)
    #Number of Parameters
    n_r = match("validdate", names(df))+1
    n_p = ncol(df[n_r:c])
    #Date
    df$validdate = as.POSIXct(df$validdate,format='%Y-%m-%dT%H:%M:%OSZ', tz = 'UTC')
    df$validdate = with_tz(df$validdate, tzone = time_zone)
    #Right shape parameters
    parameter = data.frame(stack(df[n_r:c]))
    parameter = parameter[c(2,1)]
    #Length of Date (double so many time as parameters are there)
    df_timeseries = as.data.frame(rep(df$validdate, n_p))
    #Rename and put togheter
    colnames(df_timeseries) = c('Date')
    df_timeseries['Lat'] = as.data.frame(rep(df$lat, n_p))
    df_timeseries["Lon"] = as.data.frame(rep(df$lon, n_p))
    df_timeseries['Parameter']= parameter$ind
    df_timeseries['Value']= parameter$values
    return(df_timeseries)
  }
  
}else{
#Domain (Rectangle)
  #Data from the API
  api_domain = function(path)
  {
    resp1 = fread(query)
    structure(
      list(content = resp1)
    )
  }
  
  result = api_domain("/(startdate_query)/(parameters)/(coordinate)/csv?")
  
  #Data
  query_api = function(username, password, startdate, enddate, interval, parameters, coordinate)
  {
    df = as.data.frame(result$content)
    #Numbers of col and row
    r = nrow(df)
    c = ncol(df)
    #Lon
    Lon = rep(c(t(df[1,2:c])), r-1)
    df_domain = data.frame(Lon)
    #Lat
    Lat = rep(df[2:r,1], each = c-1)
    df_domain["Lat"] = data.frame(as.numeric(Lat))
    #Values
    df_domain["Values"] = data.frame(Values = c(t(df[2:c,2:r])))
    return(df_domain)
  }
}
