mp <- econDV2::Map()
mp$sf$get_sf_taiwan_simplified() ->
  sf_taiwan_simplified

econDV2::Object(demo)
demo$df <- read.csv("https://github.com/jhchou97/110-1-Economic-Data-Visualization/blob/e5831c90379227d2042fdb835084290fc0f75f6d/2020Nov_district.csv") 
demo$df |>
  mutate(訊號量 = NIGHT_WORK - NIGHT_WEEKEND) %>%
  group_by(TOWN)-> demo$df


Left <- sf_taiwan_simplified$台灣本島$鄉鎮區
Right <- demo$df
  

demo$join <-{
  Left %>%
    right_join(
      Right, by = c("name" = "TOWN")
    )
}


demo$background$台灣本島$鄉鎮區 <- {
  sf_taiwan_simplified$台灣本島$鄉鎮區 |>
    sf::st_cast("MULTIPOLYGON") |>
    mp$sf$make_background_map(
      color="white",
      size=0.1
    )+theme_void()
}

demo$plot <- function(){
  demo$background$台灣本島$鄉鎮區+geom_sf(
      data = demo$join,
      mapping = aes(
        fill = 訊號量
      ),
      color = "white",
      size = 0.01
  )+
    colorspace::scale_fill_continuous_sequential(
      palette="reds", 
      na.value="grey70"
    )+
    labs(title = "週間與週末之夜間訊號量差異",
         subtitle = "週間指週一到週五/ 單位：人", # 補充單位說明
         caption = "資料來源：內政部統計處
       109年11月行政區電信信令人口統計")
  }
