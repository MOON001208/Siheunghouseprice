library(tidyverse)
price <- read_csv('price_last.csv')
price |> filter(dong_nm == '신천동' & apt_nm =='동진1')
head(price$juso_jibun,2)

apt_juso <- select(price, juso_jibun) |> distinct()
head(apt_juso)

add_list <- list()
cnt <- 0
kakao_key <- ''

library(httr)
library(data.table)
library(rjson)



for(i in 1:nrow(apt_juso)){
  #예외 처리구문 
  tryCatch({ #오류가 발생하여도 반복문을 멈추지 않고 진행하는 함수
    lon_lat <- GET(url='https://dapi.kakao.com/v2/local/search/address.json', #이거는 카카오 디벨롭 공식문서 참조
                   query = list(query=apt_juso[i,]),
                   add_headers(Authorization = paste0("KakaoAK ",kakao_key)))
    #위경도만 추출하여 저장
    coordxy <- lon_lat %>% content(as='text') %>% fromJSON()
    cnt <- cnt + 1
    add_list[[cnt]] <- data.table(apt_juso = apt_juso[i,],
                                  coord_x = coordxy$documents[[1]]$address$x,
                                  coord_y = coordxy$documents[[1]]$address$y)
    message <- paste0("[",i,"/",nrow(apt_juso),"]번째(",
                      round(i/nrow(apt_juso)*100, 2)," %) [",apt_juso[i,],"] 지오 코딩 중:
                      X=", add_list[[cnt]]$coord_x, "/ Y=",add_list[[cnt]]$coord_y)
    cat(message, "\n\n")
  }, error = function(e){cat("ERROR: ",conditionMessage(e),"\n")}
  )
}

juso_geocoding <- rbindlist(add_list)
juso_geocoding <- juso_geocoding |> mutate(across(c(coord_x, coord_y),as.numeric))
str(juso_geocoding)
colSums(is.na(juso_geocoding))
save(juso_geocoding,file='juso_geocoding.rdata')
write.csv(juso_geocoding, file='juso_geocoding.csv', row.names = F)
read_csv("juso_geocoding.csv")
