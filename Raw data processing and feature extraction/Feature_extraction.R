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
load('Cultivars.Rdata')

#Load a demo raw temperature file
#The demo temperature file includes three columns: one date column, one max daily temperature column and one min daily temperature column
all_data = read_csv('daily_temperature_data_example.csv')
all_data$date = mdy(all_data$date)
colnames(all_data)
colnames(all_data)[2:3] = c('Tmax','Tmin')


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

#Weather_feature_generation is the function to compute all the daily and cumulative temperature descriptors

Weather_feature_generation = function(df){
  
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
  
  if (nrow(df) < 20) {
    df = NULL
  }
  
  else
  {
    
    
    data_all_hourly = stack_hourly_temps(df,latitude = df$lat[1])[[1]]
    data_all_hourly$DOY = yday(data_all_hourly$betterDates)
    data_all_hourly$Date = data_all_hourly$betterDates
    data_all_hourly_1 = data.frame(Year = data_all_hourly$Year,
                                   Month = data_all_hourly$Month,
                                   Day = data_all_hourly$Day,
                                   DOY = data_all_hourly$DOY,
                                   Hour = data_all_hourly$Hour,
                                   Temp = data_all_hourly$Temp)
    #chilling_computation
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
    
    All_chilling_data <- data.frame(betterDates = data_all_hourly$betterDates,
                                    Month = format(data_all_hourly$betterDates,format = "%b"),
                                    Year = format(data_all_hourly$betterDates,format = "%Y"),
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
    
    All_chilling_data <- All_chilling_data[order(All_chilling_data$betterDates),]
    
    
    
    Chilling_data_summary <-  All_chilling_data %>%
      group_by(dormant_season) %>%
      summarise(Utah= cumsum(Utah1),
                CU = cumsum(CU1),
                NC = cumsum(NC1),
                #GDD_10 = cumsum(GDD_10_1),
                #GDD_7 = cumsum(GDD_7_1),
                #GDD_4 = cumsum(GDD_4_1),
                #GDD_0 = cumsum(GDD_0_1),
                DP = cumsum(DP1),
                betterDates = betterDates)
    
    
    GDHs = data.frame(betterDates = as.Date(GDH_10$Date),
                      GDH10_1 = GDH_10$GDH,
                      GDH_7_1 = GDH_7$GDH,
                      GDH_4_1 = GDH_4$GDH,
                      GDH_0_1 = GDH_0$GDH)
    #GDHs <- filter(GDHs,!format(GDHs$betterDates,format = "%b") == 'Aug') 
    
    GDHs = GDHs %>%
      mutate(season = ifelse(format(GDHs$betterDates,format = "%b") %in% c('Sep','Oct','Nov','Dec'), 
                             paste0(as.numeric(format(GDHs$betterDates,format = "%Y")),"-",as.numeric(format(GDHs$betterDates,format = "%Y")) + 1),
                             paste0(as.numeric(format(GDHs$betterDates,format = "%Y")) - 1,"-",as.numeric(format(GDHs$betterDates,format = "%Y"))))) %>%
      group_by(season) %>%
      summarise(GDH_10 =cumsum(GDH10_1),
                GDH_7 =cumsum(GDH_7_1),
                GDH_4 = cumsum(GDH_4_1),
                GDH_0 = cumsum(GDH_0_1),
                betterDates = betterDates) %>%
      arrange(betterDates) %>%
      select(-season)
    
    GDHs = GDHs[,-c(1,6)]
    
    
    Chilling_data_summary_daily <-  Chilling_data_summary %>%
      group_by(betterDates) %>%
      summarise(
        CU = max(CU),
        NC = max(NC),
        Utah = max(Utah),
        #GDD_10 = max(GDD_10),
        #GDD_7 = max(GDD_7),
        #GDD_4 = max(GDD_4),
        #GDD_0 = max(GDD_0),
        DP = max(DP)) %>%
      arrange(betterDates)
    
    Chilling_data_summary_daily <- bind_cols(Chilling_data_summary_daily,GDHs)
    
    
    #Daily_T
    data_all_daily_T <-  data_all_hourly  %>%
      group_by(betterDates) %>%
      summarise(#ID = station[1],
        lat = lat[1],
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
    
    #reverse EWAs
    
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
    df_out = merge(data_all_daily_T, Chilling_data_summary_daily, by = c('betterDates'))
    df_out = data.frame(df_out,
                        min_EWA,
                        min_REWA,
                        max_EWA,
                        max_REWA,
                        mean_EWA,
                        mean_REWA,
                        photoperiod = daylength(df_out$lat[1],yday(df_out$betterDates)),
                        Location = df$Site[1])
    
    df_out = filter(df_out, !Month == 8)
  }
  
}

autogluon_input_transformation = function(df,cv_of_interest) {
  
  selected_cultivar = cv_of_interest  
  selected_cultivar_colname = paste0('Cultivar.',selected_cultivar)
  
  df <- df[,!colnames(df) %in% c('photoperiod.Sunset','photoperiod.Sunrise')]
  
  colnames(df)[41] <- 'photoperiod.Daylength'
  
  df$season <-  ifelse(df$Month %in% c(9,10,11,12), paste0(df$Year,'-',df$Year + 1),
                       ifelse(df$Month %in% c(1,2,3,4),paste0(df$Year -1,'-',df$Year),NA))
  
  df <- filter(df,!is.na(season))
  
  df[Cultivars] = 0
  
  col_number = which(colnames(df) == selected_cultivar_colname)
  
  df[,col_number] = 1
  return(df)
  
}

#############measurement data processing


measurement_data = read_csv(file = 'LTE All Years(fixed).csv')
measurement_data$Date = mdy(measurement_data$Date)

LT50_data <- measurement_data %>%
  group_by(Date,Cultivar,Weather_station) %>%
  filter(!(abs(LTE - median(LTE)) > 2*sd(LTE))) %>%
  summarise(LTE = mean(LTE))

unique(LT50_data$Cultivar)

#########weather_data processing
BR_weather = read_csv(file = 'Bear River.csv')
Canard_weather = read_csv(file = 'Canard.csv')
Clarence_weather = read_csv(file = 'Clarence.csv')
Grand_Pre_weather = read_csv(file = 'Grand Pre.csv')
Mahone_Bay_weather = read_csv(file = 'Mahone Bay.csv')
Northville_weather = read_csv(file = 'Northville.csv')

BR_weather$lat = 44.572
Canard_weather$lat  = 45.134
Clarence_weather$lat = 44.926
Grand_Pre_weather$lat = 45.099
Mahone_Bay_weather$lat = 44.453
Northville_weather$lat = 45.137

BR_weather$Weather_station = "Bear_river"
Canard_weather$Weather_station = "Canard"
Clarence_weather$Weather_station = "Clarence"
Grand_Pre_weather$Weather_station = "Grand_pre"
Mahone_Bay_weather$Weather_station = "Mahone_bay"
Northville_weather$Weather_station = "Northville"

weather_data = bind_rows(BR_weather,Canard_weather,Clarence_weather,
                         Grand_Pre_weather,Mahone_Bay_weather,Northville_weather)
weather_data = weather_data[,-1]

Kentville_weather = read_csv(file = 'Kentville.csv')
Kentville_weather <- tidyr::separate(Kentville_weather,dtTIMESTAMP, c("Date", "Hour"), sep = " ")
Kentville_weather$Date = mdy(Kentville_weather$Date)
Kentville_weather$Weather_station = "Kentville"
Kentville_weather$lat = 45.062
  
weather_data = bind_rows(weather_data,Kentville_weather)

weather_data = weather_data %>%
  group_by(Date,Weather_station,lat) %>%
  summarise(Tmin = min(Temp),
            Tmax = max(Temp))

weather_data$Date = as.Date(weather_data$Date)


ggplot(weather_data, aes(x = Date, y= Tmax)) +
  geom_point() + 
  facet_wrap(~Weather_station)



#fix the holes
check = filter(weather_data, Weather_station == 'Canard')

Canard_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-10-12') & Date < as.Date('2020-12-04')) |
                    (Weather_station == 'Kentville' & Date > as.Date('2022-04-12') & Date < as.Date('2022-11-15')))

Canard_fix$Weather_station = 'Canard'

check = filter(weather_data, Weather_station == 'Bear_river')

BR_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-09-19') & Date < as.Date('2020-12-07')))
BR_fix$Weather_station = 'Bear_river'

check = filter(weather_data, Weather_station == 'Clarence')

Clarence_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-07-30') & Date < as.Date('2020-12-07')) |
                      (Weather_station == 'Kentville' & Date > as.Date('2022-03-14') & Date < as.Date('2022-11-15')))
Clarence_fix$Weather_station = 'Clarence'

check = filter(weather_data, Weather_station == 'Grand_pre')

Grand_pre_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-08-20') & Date < as.Date('2020-12-04')) |
                      (Weather_station == 'Kentville' & Date > as.Date('2022-04-11') & Date < as.Date('2022-11-15')))
Grand_pre_fix$Weather_station = 'Grand_pre'

check = filter(weather_data, Weather_station == 'Kentville')
Kentville_fix = filter(weather_data, (Weather_station == 'Bear_river' & Date > as.Date('2018-08-10') & Date < as.Date('2018-10-01')))
Kentville_fix$Weather_station = 'Kentville'

check = filter(weather_data, Weather_station == 'Mahone_bay')
Mahone_bay_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-08-09') & Date < as.Date('2020-12-04')) |
                         (Weather_station == 'Kentville' & Date > as.Date('2022-04-12') & Date < as.Date('2023-01-03')))
Mahone_bay_fix$Weather_station = 'Mahone_bay'

check = filter(weather_data, Weather_station == 'Northville')
Northville_fix = filter(weather_data, (Weather_station == 'Kentville' & Date > as.Date('2020-07-31') & Date < as.Date('2020-12-04')) |
                          (Weather_station == 'Kentville' & Date > as.Date('2022-04-11') & Date < as.Date('2022-11-15')))
Northville_fix$Weather_station = 'Northville'

weather_data = bind_rows(weather_data, BR_fix, Canard_fix,Clarence_fix,Grand_pre_fix,Kentville_fix,Mahone_bay_fix,Northville_fix)


ggplot(weather_data, aes(x = Date, y= Tmax)) +
  geom_point() + 
  facet_wrap(~Weather_station)

colnames(weather_data)[which(colnames(weather_data) == 'Weather_station')] = 'Site'
colnames(weather_data)[which(colnames(weather_data) == 'Date')] = 'betterDates'

feature_extracted_NS_data <-data.frame()

for (i in 1:length(unique(weather_data$Site))) {
  a = unique(weather_data$Site)[i]
  check = filter(weather_data, Site == a)
  check_1 <- Weather_feature_generation(check)
  feature_extracted_NS_data = rbind(feature_extracted_NS_data, check_1)
}


colnames(LT50_data)[which(colnames(LT50_data) == 'Date')] = 'betterDates'
colnames(LT50_data)[which(colnames(LT50_data) == 'Weather_station')] = 'Location'

feature_extracted_NS_data_with_label <- merge(LT50_data,feature_extracted_NS_data, by = c('betterDates','Location'))

unique(feature_extracted_NS_data_with_label$Cultivar)

dmy = dummyVars("~Cultivar", data = feature_extracted_NS_data_with_label)
trsf <- data.frame(predict(dmy,newdata = feature_extracted_NS_data_with_label))

feature_extracted_NS_data_with_label <- cbind(feature_extracted_NS_data_with_label,trsf)
feature_extracted_NS_data_with_label <- feature_extracted_NS_data_with_label[,!colnames(feature_extracted_NS_data_with_label) %in% c('Cultivar')]
colnames(feature_extracted_NS_data_with_label) <- gsub('Cultivar','Cultivar.',colnames(feature_extracted_NS_data_with_label))
feature_extracted_NS_data_with_label$Location <- 'NS'


new_dataset <- bind_rows(all_data,feature_extracted_NS_data_with_label)
colnames(new_dataset)
new_dataset <- new_dataset[,-c(121:127)]

colnames(new_dataset)

new_dataset[,77:121]<- new_dataset[,77:121] %>%
  mutate_all(funs(replace_na(.,0)))

#feature_extracted_quebec_data_with_label_for_model_validation <- filter(new_dataset, Location == 'QC')


#write_csv(as.data.frame(feature_extracted_quebec_data_with_label_for_model_validation),file = "QC_FT_data_2020_2023_with_all_weather_feature_with_DP_EWA_REWA.csv")
 


write_csv(new_dataset,file = "combined_NY_WA_WI_MI_BC_QC_PA_TX_NS_new_data_with_DP_EWA_REWA_GDHs.csv")

colnames(new_dataset)
Cultivars = colnames(new_dataset)[77:121]

save(Cultivars,file = 'Cultivars.Rdata')



#further_manipulation
final_data = read_csv('combined_NY_WA_WI_MI_BC_QC_PA_TX_NS_new_data_with_DP_EWA_REWA_GDHs_final.csv')


#if_else(as.numeric(month(mdy(final_data$betterDates))) %in% c(9,10,11,12), ymd(paste0(year(mdy(final_data$betterDates)),"-",9,"-",01)),
#       ymd(paste0(year(mdy(final_data$betterDates))-1,"-",9,"-",01)))

final_data$Days_in_season = as.numeric(mdy(final_data$betterDates) - if_else(as.numeric(month(mdy(final_data$betterDates))) %in% c(9,10,11,12), ymd(paste0(year(mdy(final_data$betterDates)),"-",9,"-",01)),
                                                                             ymd(paste0(year(mdy(final_data$betterDates))-1,"-",9,"-",01))))
final_data$Days_in_season
write_csv( final_data,file = 'combined_NY_WA_WI_MI_BC_QC_PA_TX_NS_new_data_with_DP_EWA_REWA_GDHs_final_doy.csv' )






