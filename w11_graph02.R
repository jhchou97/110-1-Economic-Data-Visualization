#devtools::install_github("tpemartin/econDV2", force=T)
#更新econDV2套件
library(tidyverse)

demo <- read_csv("2020Nov_district.csv",
                 locale = locale(encoding = "BIG5"))

#############################################
#  02  夜間停留人次差異 (週間大於週末)      #
#############################################

df <- demo %>%
  select(COUNTY, NIGHT_WORK, NIGHT_WEEKEND) %>% # show these cols
  mutate(NIGHT_DIFF = NIGHT_WORK - NIGHT_WEEKEND) %>% # create a new variable
  group_by(COUNTY) %>%
  summarise(sum(NIGHT_DIFF))

names(df)[2] <- "NIGHT_DIFF"

df1 <- df %>%
  filter(NIGHT_DIFF>=0) # extract these rows

levels(df1$COUNTY)

nLevels_1 <- df1 %>%
  arrange(NIGHT_DIFF) %>% # sort in  descending order: arrange(desc(NIGHT_DIFF))
  pull(COUNTY)

# check if it sort in ascending order: print(nLevels_1)

df1$COUNTY <- factor(
  df1$COUNTY, levels = as.character(nLevels_1))

levels(df1$COUNTY)

ggplot()+
  geom_col(data = df1,
    mapping = aes(
      x = COUNTY,
      y = NIGHT_DIFF,
    ),
     width = 0.6,
    fill = "cyan3"
  )+
  coord_flip()+
  scale_y_discrete(expand = c (0,1))+
  geom_text(data = df1[1,],
    mapping = aes(
      x = COUNTY,
      y = NIGHT_DIFF,
      label = NIGHT_DIFF
    ),
    vjust = 0.5, #input$vjust
    hjust = 1.5, #input$hjust
    size = 4 #input$size
  )+
  geom_text(data = df1[2:7,],
            mapping = aes(
              x = COUNTY,
              y = NIGHT_DIFF,
              label = NIGHT_DIFF
            ),
            vjust = 0.5, #input$vjust
            hjust = -0.35, 
            size = 4 #input$size
  )+
  labs(x = NULL, y = NULL,
       title = "週間與週末之夜間停留人次差異",
       subtitle = "僅列出夜間停留人次週間大於週末之縣市 (週間指週一到週五/ 單位：人)", # 補充單位說明
       caption = "資料來源：內政部統計處
       109年11月行政區電信信令人口統計"
  )+
  theme(plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8))
