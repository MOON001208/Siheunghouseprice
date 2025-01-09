library(sp)
library(sf)
library(tidyverse)
price <- read_csv('price_last.csv')
load('juso_geocoding.rdata')
geocoding <- juso_geocoding |> rename(juso_jibun = apt_juso.juso_jibun)
apt_price <- left_join(price,geocoding)

str(apt_price)

colSums(is.na(apt_price))

apt_price <- na.omit(apt_price)


#좌푯값 할당
library(sp)

coordinates(apt_price) <- ~coord_x + coord_y #좌표값 할당

proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs" #좌표계(CRS) 정의
#+proj=longlat: 이 데이터는 경도(longitude)와 위도(latitude) 좌표계에 기반을 둡니다.

#+datum=WGS84: 이 데이터는 WGS84 기준 좌표계 (World Geodetic System 1984)를 사용합니다.

#+no_defs: 기본 설정을 사용하지 않습니다.

library(sf)
geo_apt_price <- st_as_sf(apt_price)
str(geo_apt_price)
save(geo_apt_price,file='geo_apt_price.rdata')
write.csv(geo_apt_price,file='geo_apt_price.csv', row.names = F)
