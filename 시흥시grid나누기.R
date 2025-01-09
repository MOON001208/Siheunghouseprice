# 필요한 패키지 설치 및 로드
library(sf)
library(raster)
library(sp)
#shp <- st_read('./시흥시_행정동경계/siheung_boundary.shp',options="ENCODING=WINDOWS-1252")
#법정동
shp1 <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01106/NF_A_G01106.shp')
#행정동
#shp2 <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01109/NF_A_G01109.shp')

# 셰이프 파일 읽기
shp <- st_read('./시흥시행정동/(B031)국가기본공간정보(경기도 시흥시)_NF_A_G01106/NF_A_G01106.shp')


# 셰이프 파일을 EPSG:5186 좌표계로 변환
shp1 <- st_transform(shp1, crs = 5186)

# 0.5km x 0.5km 그리드 생성
grid <- st_make_grid(
  shp1, 
  cellsize = c(500,500),  # 0.5km x 0.5km
  square = TRUE
)

# 생성된 그리드의 좌표계 확인
st_crs(grid)
# 시흥시 경계와 교차하는 그리드 필터링
grid_filtered <- st_intersection(grid, shp1)
ggplot() +
  geom_sf(data=grid_filtered)
# 결과 시각화
plot(st_geometry(shp1), col = "lightblue", main = "0.5km Grid Over 시흥시")
plot(st_geometry(grid_filtered), add = TRUE, border = "red")

save(grid_filtered, file='grid_siheung.rdata')
