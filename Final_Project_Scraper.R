library(rjson)
library(httr)
library(XML)
library(stringr)
library(lubridate)
library(tidyverse)

# This script scrape data from Trakus T-Charts

#Login
login <- function(){
  r <- POST("http://tnetwork.trakus.com/tnet/Login.aspx", body = list(TEXT_Username="JamesMundy",TEXT_Password="jamba1ayA",BUTTON_Submit="Log+In"))
  a<-cookies(r)
  ASP.NET_SessionID <-a[7][[1]][1]
  userCredentials <-a[7][[1]][2]
  ASPAUTH <-a[7][[1]][3]
}

#Convert Fraction to doubles
mixedToFloat <- function(x){
  is.integer  <- grepl("^\\d+$", x)
  is.fraction <- grepl("^\\d+\\/\\d+$", x)
  is.mixed    <- grepl("^\\d+ \\d+\\/\\d+$", x)
  #stopifnot(all(is.integer | is.fraction | is.mixed))
  
  numbers <- strsplit(x, "[ /]")
  
  ifelse(is.integer,  as.numeric(sapply(numbers, `[`, 1)),
         ifelse(is.fraction, as.numeric(sapply(numbers, `[`, 1)) /
                  as.numeric(sapply(numbers, `[`, 2)),
                as.numeric(sapply(numbers, `[`, 1)) +
                  as.numeric(sapply(numbers, `[`, 2)) /
                  as.numeric(sapply(numbers, `[`, 3))))
}

getLengths <- function(x){
  case_when(
    x == "Neck" ~ 0.25,
    x == "Head" ~ 0.1,
    x == "Nose" ~ 0.05,
    nchar(x) > 0 ~ mixedToFloat(x),
    TRUE ~ 0
  )
  
}



#Match Abbreviation with VenueID
start <- function(data,race_date)
{
  trk <- str_to_lower(data)
  d <- mdy(race_date)
  filedate <- str_replace_all(toString(d),"-","")
  filename <- str_c(data,filedate,".csv", sep = '')
  
  
  setwd(file.path("C:","Users", "mutue", "OneDrive", "Documents", "TimeForm", "Data", "TCharts"))
  l <- list(CD = 18,BEL = 23,DM = 10,GP = 20,TAM = 21,AQU = 24,SA = 4,SAR = 25,KEE = 7)
  cat(l[[data]],race_date)
  login()
  race_program(l[[data]],race_date,filename)
}

#Go to specific date and track
race_program <- function(V_ID,race_date,filename){
  
  tchartConn <<-file(filename,"w")
  
  url <-paste(c('http://tnetwork.trakus.com/tnet/t_RaceDay.aspx?VenueID=',toString(V_ID),'&Type=TBRED&Date=',toString(race_date)),collapse='')
  r <- GET(url)
  temp<-content(r, "text")
  doc <- htmlParse(temp)
  #Grab all event id
  temp_event_id_list<-xpathSApply(doc,"//a[contains(@href,'t_Recap.aspx?EventID=')]/@href")
  temp_event_id_list<-substr(temp_event_id_list,22,40)
  event_id_list <- list()
  counter<-1
  #Scrape through each event
  for( i in temp_event_id_list)
  {
    event_id_list[counter]<-i
    counter<-counter+1
    race_detail(i, tchartConn)
  }
  close(tchartConn, type ='w')
  
}

#Scrape Race in every range
race_detail <- function(event_id, f){
  
  url <- paste(c('http://tnetwork.trakus.com/tnet/t_Recap.aspx?EventID=',toString(event_id),'&PostSelect=0'),collapse='')
  r <- GET(url)
  temp<-content(r, "text")
  doc <- htmlParse(temp)
  race_info<-xpathSApply(doc,"//*[@class=\"recapTextHeader\"]/td/div")[[1]]
  race_info<-toString(xmlValue(race_info))
  #race_info<-paste(c(toString(xmlValue(race_info[1]$text)),toString(xmlValue(race_info[3]$i)),toString(xmlValue(race_info[5]$text)),toString(xmlValue(race_info[7]$text)),toString(xmlValue(race_info[9]$text))),collapse='|')
  race_info <- str_replace_all(race_info,"(<).*?(>)","") 
  race_info <- str_replace_all(race_info, "(\\s{3,200})"," ")

  
  track_name <- str_extract(race_info,'.+?(?=\\sRace)')
  race_number <- str_extract(race_info,'(Race\\s\\d+)')
  purse <- str_replace(str_extract(race_info, "(\\$\\d+)"),"(\\$)","")
  race_date <- str_extract(race_info, "([A-Z][a-z]+\\s\\d+\\,\\s\\d{4})")
  race_date <- mdy(race_date)
  race_date <- toString(race_date)
  distance <- str_extract(race_info,'(\\d\\sFurlongs|\\d\\.\\d Furlongs|\\d\\s+\\d\\/\\d{1,2}\\sMiles)')
  surface <- str_extract(race_info,'(Dirt|Turf|Sythetic|Outer Turf|Inner Turf)')
  
  final_select<-length(xpathSApply(doc,"//*[@id=\"PostSelect\"]/option"))
  print(final_select)
  #Loop through each segment
  for(range_select in 2:final_select)
  {
    url <- paste(c('http://tnetwork.trakus.com/tnet/t_Recap.aspx?EventID=',toString(event_id),'&PostSelect=',toString(range_select)),collapse='')
    print(url)
    r <- GET(url)
    temp<-content(r, "text")
    doc <- htmlParse(temp)
    
    horse_num  <- x <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[2]/img",xmlAttrs)
    horse_num <- horse_num[3,]
    horse_num <- str_sub(horse_num,-6,-4)
    horse_num <- str_extract(horse_num,'[0-9ABC]{1,2}')
    
    
    
    horse_name <- x <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[3]",xmlValue)
    horse_time <- x <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[4]",xmlValue)
    horse_dist <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[5]",xmlValue)
    horse_rail <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[6]",xmlValue)
    horse_avg <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[7]",xmlValue)
    horse_ahead <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[8]",xmlValue)
    horse_ahead <- str_replace(horse_ahead,'(\\dP\\d:)',"")
    horse_ahead <- str_replace(horse_ahead,'(\\()',"")
    horse_ahead <- str_replace(horse_ahead,'(\\))',"")
    horse_ahead <- getLengths(horse_ahead)
    ifelse(horse_ahead == "","0",horse_ahead)
    
    
    
    horse_cu_time <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[11]",xmlValue)
    horse_cu_peak <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[12]",xmlValue)
    horse_cu_dist <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[13]",xmlValue)
    horse_cu_delta <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[14]",xmlValue)
    horse_cu_delta <- str_replace_all(horse_cu_delta,'---','0')
    horse_cu_avg <-xpathSApply(doc,"//tr[@bgcolor=\"white\"]/td[15]",xmlValue)
    
    for(count in 1:length(horse_name))
    {
      
      
      cat(track_name, race_number, purse, race_date, distance, surface, count, range_select, horse_num[count], horse_name[count], horse_time[count],horse_dist[count],horse_rail[count],horse_avg[count],horse_ahead[count],horse_cu_time[count],horse_cu_peak[count],horse_cu_dist[count],horse_cu_delta[count],horse_cu_avg[count],'\n')
      writeLines(paste(c(track_name, race_number, purse, race_date, distance, surface, count, range_select, horse_num[count], horse_name[count], horse_time[count],horse_dist[count],horse_rail[count],horse_avg[count],horse_ahead[count],horse_cu_time[count],horse_cu_peak[count],horse_cu_dist[count],horse_cu_delta[count],horse_cu_avg[count]),collapse = ','), f)
  
    }
  }
}


start("AQU","12/8/2019")

