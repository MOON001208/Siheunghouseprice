library(tidyverse)
library(sf)
load("grid_price.rdata")
load('grid.rdata')

########7-2###########
#일정기간동안 가장 많이 가격이 오른 지역은 어디일까
kde_before <- st_drop_geometry(grid_price) |> filter(ymd<="2020-12-31") |> 
  group_by(juso_jibun) |> summarise(before = mean(py))
kde_after <- st_drop_geometry(grid_price) |> filter(ymd>='2021-01-01') |> 
  group_by(juso_jibun) |> summarise(after=mean(py))


kde_diff <- merge(kde_before,kde_after)
kde_diff$diff <- round(((kde_diff$after - kde_diff$before)/
                          (kde_diff$before)*100),0)


kde_diff <- kde_diff[kde_diff$diff>0,] #상승지역만 추출
kde_hot <- merge(grid_price,kde_diff,by="juso_jibun") |>distinct(juso_jibun, diff,before, after, st_transform.grid_filtered..st_crs.geo_apt_price..)
class(kde_hot)
#save(kde_hot, file='kde_hot_merge.rdata')
#load('kde_hot_merge.rdata')

head(kde_hot,2)
kde_hot |> ggplot(aes(fill=diff)) + 
  geom_sf() +
  scale_fill_gradient(low='white',high='red')


library(sp) 
kde_hot_sp <- as(st_geometry(kde_hot), "Spatial") # sf형 => sp형 변환
x <- coordinates(kde_hot_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_hot_sp)[,2] 

l1 <- bbox(kde_hot_sp)[1,1] - (bbox(kde_hot_sp)[1,1]*0.001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_hot_sp)[1,2] + (bbox(kde_hot_sp)[1,2]*0.001)
l3 <- bbox(kde_hot_sp)[2,1] - (bbox(kde_hot_sp)[2,1]*0.001)
l4 <- bbox(kde_hot_sp)[2,2] + (bbox(kde_hot_sp)[1,1]*0.001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4))  # 경계지점 기준 외곽선 만들기(bounding-box)
plot(win)                                      # 확인

# [5단계: 밀도 그래프 변환하기]

p <- ppp(x, y, window=win, marks=kde_hot$diff) # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_hot$diff,      # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p), 
                 kernel = 'gaussian')
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리

#---# [6단계: 픽셀 -> 레스터 변환]

d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA  # 노이즈 제거
library(raster)         # install.packages("raster")
raster_hot <- raster(d) # 레스터 변환
plot(raster_hot) #  확인

#---# [7단계: 클리핑]
shp1 <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01106/NF_A_G01106.shp')
# 시흥시 전체 경계 결합
siheung_boundary <- st_union(shp1)

siheung_boundary_utm <- st_transform(siheung_boundary, crs = "+proj=longlat +datum=WGS84 +no_defs") |> as("Spatial")

raster_hot <- crop(raster_hot, extent(siheung_boundary_utm))            # 외곽선 클리핑
crs(raster_hot) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84  
                        +towgs84=0,0,0")  # 좌표계 정의
plot(raster_hot)   #  확인
plot(siheung_boundary_utm, col=NA, border = "red", add=TRUE)

library(leaflet)
leaflet() |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addPolygons(data=siheung_boundary_utm,weight=3,color='red',fill=NA) |> 
  addRasterImage(raster_hot,
                 colors=colorNumeric(c('yellow','blue','green','red'),
                                     values(raster_hot),na.color='transparent'),
                 opacity=0.4)
save(raster_hot,file='kde_hot.rdata')
#rm(list=ls())



