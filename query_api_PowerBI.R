#Program for Meteomatics_Weather_API
#Packages
library(httr)
library(data.table)
library(lubridate)
library(ggplot2)
source('VERSION.R')

query_user_features = function(username, password)
{
  r=GET(sprintf('https://%s:%s@api.meteomatics.com/user_stats_json', username, password))
  j=jsonlite::fromJSON(content(r, 'text'))
  res <- logical(3)
  names(res) <- c('area request option', 'historic request option', 'model select option')
  res['area request option'] = j$`user statistics`$`area request option`
  res['historic request option'] = j$`user statistics`$`historic request option`
  res['model select option'] = j$`user statistics`$`model select option`
  return(res)
}

api_timeseries = function(query)
{
  resp1 = GET(query)
  con = textConnection(content(resp1,"text"))
  parsed1 = read.csv(con, sep = ";")
  structure(
    list(content = parsed1)
  )
}
  
api_domain = function(query)
{
  resp1 = fread(query, skip=2, fill = TRUE)
  structure(
    list(content = resp1)
  )
}

query_api = function(username, password, startdate, enddate, 
                     interval, parameters, coordinate, 
                     request_type="timeseries",
                     model="mix", time_zone="UTC"
)
{
  #def Variabeln
  startdate_query = strftime(startdate,format='%Y-%m-%dT%H:%M:%OSZ', tz='UTC')
  enddate_query = strftime(enddate,format='%Y-%m-%dT%H:%M:%OSZ', tz='UTC')
  ensSelect = {}
  #URL
  if(request_type == "timeseries")
  {
    query = sprintf("https://%s:%s@api.meteomatics.com/%s--%s:%s/%s/%s/csv?model=%s&connector=PowerBI_connector_%s", 
                    username, password, startdate_query, enddate_query, interval, parameters, coordinate, model, VERSION
    )
  
    result = api_timeseries(query)
    #Data in new Dataframe
    df = as.data.frame(result$content)
    #Numbers of col and row
    r = nrow(df)
    c = ncol(df)
    #Number of Parameters
    n_r = match("validdate", names(df))+1
    n_p = ncol(df[n_r:c])
    #Dates in right shape
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

  } else {
    query = sprintf("https://%s:%s@api.meteomatics.com/%s/%s/%s/csv?model=%s&connector=PowerBI_connector_v%s",
                    username, password, startdate_query, parameters, coordinate, model, VERSION
    )
    
    result = api_domain(query)
    
    #New Dataframe
    df = as.data.frame(result$content)
    #Numbers of columns and rows
    r = nrow(df)
    c = ncol(df)
    #Longitude
    Lon = rep(c(t(df[1,2:c])), r-1)
    df_domain = data.frame(Lon)
    #Latitude
    Lat = rep(df[1:r,1], each = c-1)
    df_domain["Lat"] = data.frame(as.numeric(Lat))
    #Values
    df_domain["Values"] = data.frame(Values = c(t(df[1:r,2:c])))
    mm = matrix(unlist(df_domain["Values"]), ncol = c -1 , byrow = TRUE)
    image(z=t(mm[nrow(mm):1,]))
    return(df_domain)
    
    } 
  
}
