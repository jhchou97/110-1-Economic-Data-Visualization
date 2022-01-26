devtools::install_github("tpemartin/econDV2", force=T) #更新econDV2套件
library(readr)
library(tidyverse)

demo <- read_csv("2020Nov_district.csv",
                 locale = locale(encoding = "BIG5"))

#############################################
#  主題：平日多於假日...可能是因為工作需求  #
#  01  夜間停留人數                         #
#############################################

subset1 <- demo %>%
  filter(NIGHT_WORK >= NIGHT_WEEKEND) %>% #View()
  select(COUNTY, TOWN) %>%
  mutate(
    COUNTY = as.factor(COUNTY)
  )

levels(subset1$COUNTY) # 得知欄位性質

subset1 %>%
  group_by(COUNTY) %>%
  summarise(
    Number_of_Towns = n()
  )%>%
  arrange(Number_of_Towns)%>%
  pull(COUNTY) -> newLevels

print(newLevels)

subset1$COUNTY <- factor(
  subset1$COUNTY, levels = as.character(newLevels))

levels(subset1$COUNTY)

plot1 <- ggplot(
  data = subset1,
  aes( y = COUNTY))+
  geom_bar( fill = "cyan3",
            width = 0.6 #input$width
            # position = position_dodge(0.4)
  )+ # position_dodge的效果會發生在一個類別對應到多個長條
  geom_text(stat = "count", 
            # 利用stat請geom_text計算欠缺的美學元素 (x-axis)
            aes(label = (..count..)),
            # ..count..把count取出來
            color = "white",
            vjust = 0.5, #input$vjust
            hjust = 1.5, #input$hjust
            size = 4 #input$size
  )+
  labs(title = "平日夜間停留人數較多的行政區數量",
       subtitle = "依縣市別/ 平日指週一到週五，與週末相對", # 補充單位說明
       caption = "社會經濟資料平台 109年11月行政區電信信令人口統計"
  )+
  scale_x_discrete(
    expand = c (0,0.1)
  )

plot1
