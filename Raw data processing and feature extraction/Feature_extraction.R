#NYUS.2 feature generation
##Please contact hw692@cornell.edu for any additional questions.

#Due to the limited availability of hourly temperature data, this script aims to extract features from daily temperature data.
#Load libraries

require(dplyr)                                    
require(readr)   
require(tidyverse)
require(ggpubr)
require(wesanderson)
require(pracma)
require(data.table)
require(weathermetrics)
require(measurements)
require(naniar)
require(ggplot2)
require(ggpubr)
require(dormancyR)
require(chron)
require(ggrepel)
require(geosphere)
require(chillR)
require(lubridate)
require(caret)
require(fruclimadapt)


#Load cultivars
load('Cultivars_NYUS_2_1.Rdata')
cultivars_to_choose_from = gsub('Cultivar.','',Cultivars)

#Load a demo raw temperature file
#The demo temperature file includes three columns: one date column, one max daily temperature column and one min daily temperature column
all_data = read_csv('daily_temperature_data_example.csv')
all_data$tmax = as.numeric(all_data$tmax)
all_data$tmin = as.numeric(all_data$tmin)

all_data$date = mdy(all_data$date)
colnames(all_data)
colnames(all_data)[2:3] = c('Tmax','Tmin')


#As Tmax and Tmin are is in Fahrenheit, the first step is to change the unit to Celsius
all_data$Tmax = fahrenheit.to.celsius(all_data$Tmax)
all_data$Tmin = fahrenheit.to.celsius(all_data$Tmin)




###Functions

#inverse_EWA is the function to compute REWMA temperatures
inverse_EWA = function(datalist,window=10){
  datalist_rev = rev(datalist)
  datalist_rev_EWA = movavg(datalist_rev,n = window,type = 'e')
  datalist_EWA = rev(datalist_rev_EWA)
  datalist_EWA = c(rep(NA,window), datalist_EWA)
  datalist_EWA = datalist_EWA[1:length(datalist)]
  return(datalist_EWA)
}

#Weather_feature_generation is the function to compute all the features need for NYUS.2
#This function requires three inputs:
#First, a df similar with 'all_data' that have a 'date' column (properly formatted for date datatype) and two numeric columns ('Tmin' and 'Tmax') containing daily maximum and minimum temperatures
#Second, the latitude of the site of the temperature data
#Third, your cultivar of interest for the prediction of freezing tolerance. Please copy and paste a cultivar from 'cultivars_to_choose_from'

Weather_feature_generation = function(df,latitude = 43.00, cultivar = 'Riesling'){
  
  #Data filtering to exclude NAs and outliers
  df$date <- as.Date(df$date)
  df <- df %>% 
    arrange(date)
  
  df = filter(df, !is.na(Tmin), !is.na(Tmax))
  df$Year <- as.numeric(format(df$date, format = "%Y"))
  df$Month <-  as.numeric(format(df$date, format = "%m"))
  df$Day <-  as.numeric(format(df$date, format = "%d"))
  df <- filter(df, Month %in% c(1,2,3,4,8,9,10,11,12))
  df <-df %>%
    group_by(Year) %>%
    mutate(ind = sum(is.na(Tmin))) %>%
    group_by(Year) %>%
    filter(!any(ind >= 10)) %>%
    select(-ind)
  df <- filter(df, !is.na(Tmax) | is.na(Tmin), !is.na(Tmax) & !is.na(Tmin),!Tmax > 100, !Tmin < -100, !Tmin > Tmax)
  
  ##If the input df have fewer than 20 row left after filtering, the feature extraction could not be accomplished
  if (nrow(df) < 20) {
    df = NULL
  }
  
  else
  {
    
    #Estimate hourly temperature based on daily temperature variation
    data_all_hourly = stack_hourly_temps(df,latitude = latitude)[[1]]
    data_all_hourly$DOY = yday(data_all_hourly$date)
    data_all_hourly$Date = data_all_hourly$date
    data_all_hourly_1 = data.frame(Year = data_all_hourly$Year,
                                   Month = data_all_hourly$Month,
                                   Day = data_all_hourly$Day,
                                   DOY = data_all_hourly$DOY,
                                   Hour = data_all_hourly$Hour,
                                   Temp = data_all_hourly$Temp)
    
    
    #Chilling models computation
    CU <- chilling_units(data_all_hourly$Temp, summ = F)
    Utah<- modified_utah_model(data_all_hourly$Temp, summ = F)
    NC<-north_carolina_model(data_all_hourly$Temp, summ = F)
    GDH_10 <- GDH_linear(data_all_hourly_1, Tb = 10, Topt = 25, Tcrit = 36)
    GDH_7 <- GDH_linear(data_all_hourly_1, Tb = 7, Topt = 25, Tcrit = 36)
    GDH_4 <- GDH_linear(data_all_hourly_1, Tb = 4, Topt = 25, Tcrit = 36)
    GDH_0 <- GDH_linear(data_all_hourly_1, Tb = 0, Topt = 25, Tcrit = 36)
    DP <- Dynamic_Model(data_all_hourly$Temp, summ = F)
    
    
    CU = if_else(CU < 0, 0, CU)
    Utah = if_else(Utah < 0, 0 ,Utah)
    NC = if_else(NC < 0,0, NC)
    
    All_chilling_data <- data.frame(date = data_all_hourly$date,
                                    Month = format(data_all_hourly$date,format = "%b"),
                                    Year = format(data_all_hourly$date,format = "%Y"),
                                    CU = CU,
                                    Utah = Utah,
                                    NC = NC,
                                    DP = DP)
    
    All_chilling_data$Year  = as.numeric(All_chilling_data$Year)
    
    All_chilling_data$CU1 <- ifelse(All_chilling_data$Month %in% c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr'),All_chilling_data$CU, 0 )
    All_chilling_data$Utah1 <- ifelse(All_chilling_data$Month %in% c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr'),All_chilling_data$Utah, 0 )
    All_chilling_data$NC1 <- ifelse(All_chilling_data$Month %in% c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr'),All_chilling_data$NC, 0 )
    GDH_10$GDH <- ifelse(GDH_10$Month %in% c(1,2,3,4),GDH_10$GDH, 0 )
    GDH_7$GDH <- ifelse(GDH_7$Month %in% c(1,2,3,4),GDH_7$GDH, 0 )
    GDH_4$GDH <- ifelse(GDH_4$Month %in% c(1,2,3,4),GDH_4$GDH, 0 )
    GDH_0$GDH <- ifelse(GDH_0$Month %in% c(1,2,3,4),GDH_0$GDH, 0 )
    All_chilling_data$DP1 <- ifelse(All_chilling_data$Month %in% c('Sep','Oct','Nov','Dec','Jan','Feb','Mar','Apr'),All_chilling_data$DP, 0 )
    
    
    
    All_chilling_data$dormant_season <- ifelse(All_chilling_data$Month %in% c('Sep','Oct','Nov','Dec'), paste0(All_chilling_data$Year,'-',All_chilling_data$Year + 1),
                                               ifelse(All_chilling_data$Month %in% c('Jan','Feb','Mar','Apr'),paste0(All_chilling_data$Year -1,'-',All_chilling_data$Year),NA))
    
    All_chilling_data <- All_chilling_data[order(All_chilling_data$date),]
    
    
    
    Chilling_data_summary <-  All_chilling_data %>%
      group_by(dormant_season) %>%
      reframe(Utah= cumsum(Utah1),
                CU = cumsum(CU1),
                NC = cumsum(NC1),
                #GDD_10 = cumsum(GDD_10_1),
                #GDD_7 = cumsum(GDD_7_1),
                #GDD_4 = cumsum(GDD_4_1),
                #GDD_0 = cumsum(GDD_0_1),
                DP = cumsum(DP1),
                date = date)
    
    Chilling_data_summary_daily <-  Chilling_data_summary %>%
      group_by(date) %>%
      summarise(
        CU = max(CU),
        NC = max(NC),
        Utah = max(Utah),
        #GDD_10 = max(GDD_10),
        #GDD_7 = max(GDD_7),
        #GDD_4 = max(GDD_4),
        #GDD_0 = max(GDD_0),
        DP = max(DP)) %>%
      arrange(date)
    
    #GDHs computation
    GDHs = data.frame(date = as.Date(GDH_10$Date),
                      GDH10_1 = GDH_10$GDH,
                      GDH_7_1 = GDH_7$GDH,
                      GDH_4_1 = GDH_4$GDH,
                      GDH_0_1 = GDH_0$GDH)

    GDHs = GDHs %>%
      mutate(season = ifelse(format(GDHs$date,format = "%b") %in% c('Sep','Oct','Nov','Dec'), 
                             paste0(as.numeric(format(GDHs$date,format = "%Y")),"-",as.numeric(format(GDHs$date,format = "%Y")) + 1),
                             paste0(as.numeric(format(GDHs$date,format = "%Y")) - 1,"-",as.numeric(format(GDHs$date,format = "%Y"))))) %>%
      group_by(season) %>%
      reframe(GDH_10 =cumsum(GDH10_1),
                GDH_7 =cumsum(GDH_7_1),
                GDH_4 = cumsum(GDH_4_1),
                GDH_0 = cumsum(GDH_0_1),
                date = date) %>%
      arrange(date) %>%
      select(-season)
    
    
    #Combined chilling models and GDHs
    cumulative_feature_summary_daily <- merge(Chilling_data_summary_daily,GDHs, by = c('date'))
    
    
    #Daily temperature descriptors
    data_all_daily_T <-  data_all_hourly  %>%
      group_by(date) %>%
      summarise(#ID = station[1],
        lat = latitude,
        #lon = lon[1],
        #ST = State[1],
        Year = mean(Year),
        Month = mean(Month),
        Day = mean(Day),
        min_temp = min(Temp),
        max_temp = max(Temp),
        within_day_range_temp = max(Temp) - min(Temp),
        Naive_average_temp = mean(Temp),
        Median_temp = median(Temp))
    
    
    #REWMAs
    window_size = c(2,4,6,8,10,12,14,16,18,20)
    
    #min
    min_data_number = length(inverse_EWA(data_all_daily_T$min_temp, window = window_size[2]))
    min_REWA = data.frame(NA_col = rep(NA, min_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('min_REWMA_',window_size[i],"day"))
      #assign(vname, inverse_EWA(data_all_daily_T$min_temp, window = window_size[i]))
      #col = inverse_EWA(data_all_daily_T$min_temp, window = window_size[i])
      min_REWA[,i] = inverse_EWA(data_all_daily_T$min_temp, window = window_size[i])
      colnames(min_REWA)[i] = vname
    }
    
    #max
    max_data_number = length(inverse_EWA(data_all_daily_T$max_temp, window = window_size[2]))
    max_REWA = data.frame(NA_col = rep(NA, max_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('max_REWMA_',window_size[i],"day"))
      #assign(vname, inverse_EWA(data_all_daily_T$max_temp, window = window_size[i]))
      #col = inverse_EWA(data_all_daily_T$max_temp, window = window_size[i])
      max_REWA[,i] = inverse_EWA(data_all_daily_T$max_temp, window = window_size[i])
      colnames(max_REWA)[i] = vname
    }
    
    #mean
    mean_data_number = length(inverse_EWA(data_all_daily_T$Naive_average_temp, window = window_size[2]))
    mean_REWA = data.frame(NA_col = rep(NA, mean_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('mean_REWMA_',window_size[i],"day"))
      #assign(vname, inverse_EWA(data_all_daily_T$mean_temp, window = window_size[i]))
      #col = inverse_EWA(data_all_daily_T$mean_temp, window = window_size[i])
      mean_REWA[,i] = inverse_EWA(data_all_daily_T$Naive_average_temp, window = window_size[i])
      colnames(mean_REWA)[i] = vname
    }
    
    
    
    
    #EWMAs
    
    #min
    min_data_number = length(movavg(data_all_daily_T$min_temp, n = window_size[2],type = 'e'))
    min_EWA = data.frame(NA_col = rep(NA, min_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('min_EWMA_',window_size[i],"day"))
      min_EWA[,i] = movavg(data_all_daily_T$min_temp, n = window_size[i],type = 'e')
      colnames(min_EWA)[i] = vname
    }
    
    
    #max
    max_data_number = length(movavg(data_all_daily_T$max_temp, n = window_size[2],type = 'e'))
    max_EWA = data.frame(NA_col = rep(NA, max_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('max_EWMA_',window_size[i],"day"))
      max_EWA[,i] = movavg(data_all_daily_T$max_temp, n = window_size[i],type = 'e')
      colnames(max_EWA)[i] = vname
    }
    
    
    #mean
    mean_data_number = length(movavg(data_all_daily_T$Naive_average_temp, n = window_size[2],type = 'e'))
    mean_EWA = data.frame(NA_col = rep(NA, mean_data_number))
    for (i in 1:length(window_size)) {
      vname = print(paste0('mean_EWMA_',window_size[i],"day"))
      mean_EWA[,i] = movavg(data_all_daily_T$Naive_average_temp, n = window_size[i],type = 'e')
      colnames(mean_EWA)[i] = vname
    }
    
    
    
    
    #Output
    df_out = merge(data_all_daily_T, cumulative_feature_summary_daily, by = c('date'))
    df_out = data.frame(df_out,
                        min_EWA,
                        min_REWA,
                        max_EWA,
                        max_REWA,
                        mean_EWA,
                        mean_REWA)
    
    df_out = filter(df_out, !Month == 8)
    
    
    #Add cultivar features in the df
    selected_cultivar = cultivar 
    selected_cultivar_colname = paste0('Cultivar.',selected_cultivar)

    df_out$season <-  ifelse(df_out$Month %in% c(9,10,11,12), paste0(df_out$Year,'-',df_out$Year + 1),
                         ifelse(df_out$Month %in% c(1,2,3,4),paste0(df_out$Year -1,'-',df_out$Year),NA))
    df_out <- filter(df_out,!is.na(season))
    df_out[Cultivars] = 0
    col_number = which(colnames(df_out) == selected_cultivar_colname)
    df_out[,col_number] = 1
    
    
    #Add Days_in_season in the df
    df_out$Days_in_season = as.numeric(df_out$date - if_else(as.numeric(month(df_out$date)) %in% c(9,10,11,12), ymd(paste0(year(df_out$date),"-",9,"-",01)),
                                                                                 ymd(paste0(year(df_out$date)-1,"-",9,"-",01))))
    
    #Deleted unnecessary columns
    colnames(df_out)[which(colnames(df_out) == 'date')] = 'Date'
    df_out = df_out[,-which(colnames(df_out) %in% c('lat','Year','Month','Day','DP','season','Median_temp'))]
    
    #Output 
    return(df_out)
  }
  
}


#This is an example of feature extraction. You need to identify the latitude of the vineyard and the cultivar for the feature extraction.
cultivars_to_choose_from
all_data_feature_extracted <- Weather_feature_generation(all_data,latitude = 43.0606, cultivar = 'Riesling')

write_csv(all_data_feature_extracted, file = 'daily_temperature_data_example_feature_extracted.csv')


