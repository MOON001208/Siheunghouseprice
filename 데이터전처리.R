library(tidyverse)
price <- read_csv('apr_price.csv') |> select(-1,-code)

#결측값 확인
colSums(is.na(price)) #결측값 없다.

#시계열 분석을 위한 날짜 만들기
price <- price |> mutate(ymd= make_date(year,month,day))

price$ym <- floor_date(price$ymd, "month")

str(price)

price <- price |> mutate(juso_jibun = 
                           paste0(dong_nm, jibun, apt_nm))

price <- price |> mutate(py =
                           round(price/area *3.3,0))

price$cnt <- 1

write.csv(price, './price_last.csv', row.names = F)
read_csv('price_last.csv')

