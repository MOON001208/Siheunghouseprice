library(devtools); library(sf); library(purrr); library(dplyr); library(DT) 
library(lattice); library(latticeExtra); library(lubridate)
library(ggplot2); library(ggfortify); library(ggrepel); library(showtext) 
library(leaflet); library(leaflet.extras); library(raster); library(shiny) 
library(mapview); library(mapedit); library(grid)
load('grid_siheung.rdata') #시흥지도
load("grid_price.rdata") #공간좌표아파트가격
load('siheung_boundary.rdata') #시흥경계
load('circle_marker.rdata')

#grid <- as(grid_filtered,'Spatial')#sp형 변환 
#grid <- as(grid,"sfc") #sfc변환
# 좌표계를 WGS84로 변환
# 다각형을 중심으로 변환하여 점으로 사용
grid_price_points <- st_centroid(grid_price)

grid <- st_transform(grid_filtered, crs = "+proj=longlat +datum=WGS84 +no_defs")
grid <- grid[which(sapply(st_contains(st_sf(grid),grid_price_points),length) >0)] #아파트만 포함된 그리드 추출출
plot(grid)
st_crs(grid_filtered)
st_crs(grid_price_points)
st_crs(grid_price)
st_crs(grid)
#---#이상치 설정(하위10%, 상위 90%지점)
#pcnt_10 <- as.numeric(quantile(grid_price$py, probs=seq(.1,.9,by=.1))[1])
#pcnt_90 <- as.numeric(quantile(grid_price$py, probs=seq(.1,.9,by=.1))[9])

circle.colors <- sample(x=c('red','green','blue'),size=1000, replace=T)

p <- leaflet() |>
  addTiles(options = providerTileOptions(minZoom = 9, maxZoom = 18)) |>
  #--#시흥시 경계선 불러오기
  addPolygons(
    data = siheung_boundary_utm,
    weight = 3,
    color = 'red',stroke=T,,fillOpacity = 0) |>
  #--#마커 클러스터링 불러오기
  addCircleMarkers(
    data = grid_price_points,
    lng = unlist(
      map(
        grid_price_points$st_transform.grid_filtered..st_crs.geo_apt_price..,
        1
      )
    ),
    #lng경도, lat 위도
    lat = unlist(
      map(
        grid_price_points$st_transform.grid_filtered..st_crs.geo_apt_price..,
        2
      )
    ),
    radius = 10,
    stroke = F,
    fillOpacity = 0.6,
    fillColor = circle.colors,
    weight = grid_price_points$py,
    #py를 area나 floor로바꾸면 마커클러스터링 숫자가 달라짐
    clusterOptions = markerClusterOptions(iconCreateFuntion =
                                            JS(avg.formula))
  ) |>
  leafem::addFeatures(
    st_sf(grid),
    layerId =  ~ seq_len(length(grid)),
    color = 'grey'
  )


ui <- fluidPage(#---# 상단 화면: 지도 + 입력 슬라이더
  fluidRow(
    column(9, selectModUI("selectmap"), div(style = "height:45px")),
    column(
      3,
      sliderInput(
        "range_area",
        "전용면적",
        sep = "",
        min = 0,
        max = 210,
        value = c(0, 150)
      ),
      sliderInput(
        "range_time",
        "건축 연도",
        sep = "",
        min = 1980,
        max = 2024,
        value = c(1980, 2000)
      ),
      
    ),),
  
    tabsetPanel(
      tabPanel('Chart',
               column(4, h5('Price Range', align='center'),
                      plotOutput('density',height=300),),
               column(4, h5('Price Trends',align='center'),
                      plotOutput('regression',height=300)),
               column(4, h5("PCA", align='center'),
                      plotOutput('pca',height=300)),),
      
      tabPanel("Table",DT::dataTableOutput("table"))
    ))

#---# [2단계: 반응식 설정: 슬라이더 입력 필터링]

server <- function(input, output, session) {
  #---# 반응식
  all = reactive({all = filter(grid_price_points, between(con_year, input$range_time[1],input$range_time[2]) &
                                 between(area, input$range_area[1], input$range_area[2]))
  return(all)})
  
  #---# [3단계: 지도 입출력 모듈 설정: 그리드 선택 저장]
  g_sel <- callModule(
    selectMod,
    "selectmap",p)
  
  #---# [4단계: 선택에 따른 반응 결과 저장]
  
  #---# 반응 초깃값 설정(NULL)
  rv <- reactiveValues(intersect = NULL, selectgrid = NULL)
  #---# 반응 결과(rv: reactive value) 저장
  observe({
    gs <- g_sel()
    rv$selectgrid <- st_sf(grid[as.numeric(gs[which(gs$selected == TRUE), "id"])])
    if (length(rv$selectgrid) > 0) {
      rv$intersect <- st_intersects(rv$selectgrid, all())
      rv$sel       <- st_drop_geometry(grid_price_points[grid_price_points[unlist(rv$intersect[1:10]), ], ])
    } else {
      rv$intersect <- NULL
    }
  })
  
  ##--##확률 밀도함수그려보자
  output$density <- renderPlot({
    if(nrow(rv$intersect)==0)
      return(NULL)
    
    avg_all <- mean(all()$py, na.rm=T)
    avg_sel <- mean(rv$sel$py, na.rm=T)
    max_all  <- density(all()$py)  ; max_all <- max(max_all$y)
    max_sel  <- density(rv$sel$py) ; max_sel <- max(max_sel$y)
    plot_high  <- max(max_all, max_sel)
    ggplot() +
      geom_density(data=all(), aes(x=py),color='blue',alpha=0.4) +
      geom_density(data=rv$sel,aes(x=py),color='red',alpha=0.4) +
      geom_vline(xintercept = avg_all,color='blue',lty='dashed') +
      geom_vline(xintercept = avg_sel, color='red',lty='dashed') +
      geom_text(aes(x = avg_all + avg_all * 0.13, y = plot_high * 0.3, label = sprintf("전체평균: %.0f", avg_all)), color = "blue", hjust = 0, vjust = 0, size = 4) +
      geom_text(aes(x = avg_sel + avg_sel * 0.13, y = plot_high * 0.5, label = sprintf("관심지역평균: %.0f", avg_sel)), 
                color = "red", hjust = 0, vjust = 0, size = 4) +
      labs(title='시흥시 전체 및 관심 지역의 밀도함수',
           x= '평당가격',
           y= '밀도') +
      theme_minimal()+
      theme(legend.title = element_blank())
  })
  
  #--#회귀분석
  output$regression <- renderPlot({
    if (nrow(rv$intersect)==0)
      return(NULL)
    all <- aggregate(all()$py, by=list(all()$ym),mean,na.rm=T)
    sel <- aggregate(rv$sel$py, by=list(rv$sel$ym),mean,na.rm=T)
    fit_all <- lm(x ~ Group.1,data=all)
    fit_sel <- lm(x ~ Group.1,data=sel)
    
    coef_all <- round(summary(fit_all)$coefficient[2],1)*365
    coef_sel <- round(summary(fit_sel)$coefficient[2],1)*365
    
    ggplot() + 
      geom_line(data=sel,aes(x=Group.1, y=x),color='red',size=1.5) +
      geom_smooth(data=sel, aes(x=Group.1,y=x),method='lm',color='darkgrey',lty='dashed') +
      geom_line(data=all, aes(x=Group.1,y=x),color='blue',size=1.5) +
      geom_text(aes(x=min(sel$Group.1),y=max(sel$x *1.05)), label = paste0("관심지역: ",coef_sel, "만원(평당)"), color='red',size=5,fontface='bold',hjust=0) +
      geom_text(aes(x=min(sel$Group.1), y=max(sel$x)-50),label=paste0('시흥시 전체:',coef_all,'만원(평당)'),color='blue',size=3, fontface='bold',
                hjust=0) +
      labs(x='월',y='평당가격',title = '월별 평당 가격 변화 및 회귀선') +
      theme_bw()
    
  })
  
  output$pca <- renderPlot({
    if (nrow(rv$intersect)==0)
      return(NULL)
    pca_01 <- aggregate(list(rv$sel$con_year,rv$sel$floor,
                             rv$sel$py, rv$sel$area),by=list(rv$sel$apt_nm),mean,na.rm=T)
    colnames(pca_01) <- c('apt_nm','new','floor','price','area')
    m <- prcomp(~ new + floor + price + area, data=pca_01, scale=T)
    autoplot(m, size=NA, loadings.label=T, loadings.label.size=4) + 
      geom_label_repel(aes(label=pca_01$apt_nm),size=3,alpha=0.7, family="Nanum Gothic")
  })
  
  
  
  
  
  #---# [5단계: 반응 결과 렌더링]
  
  output$table <- DT::renderDataTable({
    dplyr::select(rv$sel, ymd,dong_nm, apt_nm,con_year, area, floor, price, py) %>%
      arrange(desc(py))
  }, extensions = 'Buttons', options = list(
    dom = 'Bfrtip',
    scrollY = 300,
    scrollCollapse = T,
    paging = TRUE,
    buttons = c('excel')
  ))
}

#---# [6단계: 애플리케이션 실행] 

shinyApp(ui, server)

