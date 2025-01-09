library(sf)
library(tidyverse)
load('grid_siheung.rdata')
load('geo_apt_price.rdata')
shp1 <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01106/NF_A_G01106.shp')
unique(geo_apt_price$dong_nm)
unique(shp1$NAME)


#price <- st_join(geo_apt_price, shp1, join= st_intersects)
#이게 좌표계가 달라서 조인이 안됌

st_crs(shp1)
st_crs(geo_apt_price)
st_crs(grid_filtered)

#좌표계변경
shp1 <- st_transform(shp1, st_crs(geo_apt_price))
grid <- st_transform(grid_filtered, st_crs(geo_apt_price)) |> st_sf()
st_crs(grid)

price <- st_join(geo_apt_price, shp1, join= st_intersects) |> select(-c(17,18,19,20,21,22,23))


ggplot() +
  geom_sf(data = geo_apt_price, color = "blue", size = 0.5, alpha = 0.7) +
  geom_sf(data = shp1, fill = "transparent", color = "red") +
  labs(title = "Geo Apt Price vs 행정동 경계")
geo_apt_price |> filter(st_coordinates(geometry)[,1]>127.0 &
                          st_coordinates(geometry)[,2]<37) -> who2
price2 <- geo_apt_price |> filter(st_coordinates(geometry)[,1] < 127.0 &
                                            st_coordinates(geometry)[,2] > 37.0 & st_coordinates(geometry)[,2] < 37.5)


price2 |> filter(st_coordinates(geometry)[,1]>126.8 &
                  st_coordinates(geometry)[,2] < 37.33) ->who
who$geometry
ggplot() +
  geom_sf(data= who) +
  geom_sf(data=shp1)

price2_last <- price2 |> filter(st_coordinates(geometry)[,1] < 126.8963)

select(price2_last, dong_nm,juso_jibun) |> slice_head(n=30)

ggplot() +
  geom_sf(data = price2_last, color = "blue", size = 0.5, alpha = 0.7) +
  geom_sf(data= grid, fill='transparent',color='red')+
  coord_sf(xlim = c(126.6, 126.9), ylim = c(37.2, 37.48)) +
  labs(title = "Geo Apt Price vs 행정동 경계")


#grid_apt_price <- st_join(price2_last, st_sf(grid), join = st_intersects)

#save(grid_apt_price, file='grid_apt_price.rdata')
save(grid, file='grid.rdata')
save(price2_last, file='price2_last.rdata')
#--------------------------------------------------------#
load("price2_last.rdata")
load('grid.rdata')

price <- price2_last |> group_by(juso_jibun) |> mutate(avg_price = mean(py)) |> ungroup()
grid_price <- st_join(grid, price, join=st_intersects) 
colSums(is.na(grid_price))
grid_price <- na.omit(grid_price)

grid_price |> 
  ggplot() +
  geom_sf(aes(fill=avg_price)) +
  scale_fill_gradient(low='white',high='red',space="Lab")+
  theme_minimal() +
  labs(title = "시흥시 아파트 평균 가격 (0.5km Grid)") +
  theme(legend.position = "right")


#save(grid_price,file='grid_price.rdata')

##지도경계그리기
library(sp)
price_sp <- as(st_geometry(price), "Spatial") #sf=>sp변환
x <- coordinates(price_sp)[,1]
y <- coordinates(price_sp)[,2]


l1 <- bbox(price_sp)[1,1] - (bbox(price_sp)[1,1] * 0.001)
l2 <- bbox(price_sp)[1,2] + (bbox(price_sp)[1,2] * 0.001)
l3 <- bbox(price_sp)[2,1] - (bbox(price_sp)[2,1] * 0.001)
l4 <- bbox(price_sp)[2,2] + (bbox(price_sp)[2,2] * 0.001)

library(spatstat)
win <- owin(xrange=c(l1,l2),yrange=c(l3,l4))
plot(win)

#밀도 그래프 표시하기
p <- ppp(x, y, window = win)
d <- density.ppp(p, weight=price$avg_price,
                 sigma=bw.diggle(p),
                 kernel = 'gaussian')
plot(d)
d <- density.ppp(p, weights=price$avg_price, # 가중치 추가 
                 sigma=bw.diggle(p), kernel='gaussian')

#커널밀도함수에서 의미있는 데이터는 상위 25%정도
#이를 제거
d[d < quantile(d)[4] + (quantile(d)[4] *0.1)] <- NA #노이즈제거
library(raster)
raster_high <- raster(d)
plot(raster_high)

##시흥시 외곽경계생성
shp1 <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01106/NF_A_G01106.shp')
# 시흥시 전체 경계 결합
siheung_boundary <- st_union(shp1)

ggplot(data = siheung_boundary) +
  geom_sf(fill = "lightblue", color = "black") +
  labs(title = "Siheung City Boundary") +
  theme_minimal()

siheung_boundary_utm <- st_transform(siheung_boundary, crs = "+proj=longlat +datum=WGS84 +no_defs") |> as("Spatial")
st_crs(siheung_boundary_utm)

raster_high <- crop(raster_high, extent(siheung_boundary_utm))#외곽선 자르기
crs(raster_high) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs") #좌표계정의
plot(raster_high)
plot(siheung_boundary_utm, col=NA, border="red",add=T)

library(terra)
library(leaflet)


leaflet() |> 
  #--#기본지도불러오기
  addProviderTiles(providers$CartoDB.Positron) |> 
  #--# 서울시 경계선 불러오기
  addPolygons(data=siheung_boundary_utm, weight=3, color='red',fill=NA) |> 
  #--# 래스터이미지불러오기
  addRasterImage(raster_high,
                 colors=colorNumeric(c('blue','green','yellow','red'),
                                     values(raster_high),na.color = "transparent"),
                 opacity = 0.4)

save(raster_high, file='raster_high.rdata')
st_crs(raster_high)
st_crs(siheung_boundary_utm)
raster_high[is.na(raster_high)] <- 0
print(res(raster_high))  # 해상도 확인
print(extent(raster_high))  # 범위 확인


