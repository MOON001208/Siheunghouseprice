library(XML)
library(httr)
library(data.table)
library(stringr)
library(tidyverse)
#법정동코드 41930

#요청기간
datelist <- seq(from = as.Date('2013-01-01'),
                to = as.Date('2024-12-31'),
                by= '1 month')
datelist <- format(datelist, format='%Y%m')

length(datelist)

#api요청해서 데이터 수집
service_key <- ''

url_list <- list()
cnt <- 0

for(i in 1:length(datelist)){
  cnt <- cnt +1
  
  url_list[cnt] <- paste0("https://apis.data.go.kr/1613000/RTMSDataSvcAptTrade/getRTMSDataSvcAptTrade?",
                          "LAWD_CD=",41390,
                          "&DEAL_YMD=",datelist[i],
                          "&numOfRows=",10000,
                          "&serviceKey=",service_key)
}
url_list
browseURL(paste0(url_list[48]))

raw_data <- list()
root_Node <- list()
total <- list()
dir.create('call_api')


for(i in 1:length(url_list)){
  xml_data <- GET(url_list[[i]], add_headers(Accept='application/xml','User-Agent'='Mozilla/5.0'))
  raw_data[[i]] <- xmlTreeParse(xml_data, useInternalNodes = TRUE, encoding='UTF-8')
  root_Node[[i]] <- xmlRoot(raw_data[[i]])
  
  items <- root_Node[[i]][[2]][['items']]
  size <- xmlSize(items)
  
  item <- list()
  item_temp_dt <- data.table()
  Sys.sleep(.1)
  
  for(m in 1:size){
    item_temp <- xmlSApply(items[[m]],xmlValue)
    item_temp_dt <- data.table(
      year=item_temp[10],#거래년도
      month = item_temp[9], #거래 월
      day = item_temp[8], #거래 일
      price = item_temp[7], #거래금액
      code = item_temp[18], #지역 코드
      dong_nm = item_temp[20], #법정동
      jibun = item_temp[15], #지번
      con_year = item_temp[3], #건축 연도
      apt_nm = item_temp[2], #아파트 이름
      area = item_temp[13], #전용면적
      floor = item_temp[14]) #층수
    item[[m]] <- item_temp_dt}
  apt_bind <- apt_bind <- rbindlist(list(apt_bind, rbindlist(item)), fill = TRUE)
}
write.csv(apt_bind, './apr_price.csv')
