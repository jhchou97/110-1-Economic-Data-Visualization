library(readr)
df <- read.csv("https://github.com/jhchou97/110-1-Economic-Data-Visualization/blob/main/HD_2001.csv") 

library(tidyverse)

names(df)[1] <- "HD"

ts(data = df,
   start = c(2001,1), end = c(2021,1),
   frequency = 4) -> A

time(A) %>%
  zoo::as.yearqtr() -> t

time <- as.data.frame.Date(t)

year <- zoo::as.yearqtr(time(A)) %>% 
  format.Date("%Y")

Year <- as.data.frame.Date(year)

quarter <- cycle(A)
Quarter <- as.data.frame.Date(quarter)

df <- cbind.data.frame(Year, Quarter, t, df)


####################################
#       Ratio_HDRE_HD              #
####################################
ggplot(data = df,
       aes(x = t))+
  geom_col(
   aes(y = HD_RE/1000000*8),
   width = 0.2,
   fill = "khaki",
   #色票 http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
   alpha = 0.6 
  )+
  geom_line(
    aes(y = HD_RE/HD*100),
    size = 1,
    color = "navy"
  )+
  geom_vline(
    xintercept = c(2010,2012),
    color = "grey20",
    size = 0.5,
    lty = "dashed"
  )+
  scale_y_continuous(
    name = "購置不動產借款餘額\n佔總借款餘額比例 (%)", #\n 表示換行
    expand = c(0,0),
    sec.axis = 
     sec_axis(~./8
            ,name = "購置不動產借款餘額 (兆)"
            )
    # 老師講義 5.3.2
    # https://tpemartin.github.io/economic-data-visualization-2021/scale.html
  )+
  expand_limits(y = 80)+
  scale_x_continuous(
    name = "季別",
    expand = c(0,0),
    breaks = c(2001,
      2005,2010,2012,2015,2020,
               # seq(2005, 2020, by = 5),
      2021),
    labels = c("01\nQ1","2005\nQ1","2010\nQ1","12\nQ1","2015\nQ1","2020\nQ1",
             "21\nQ1")
  # https://github.com/tpemartin/110-1-Economic-Data-Visualization/blob/main/double-y-economist-timeline.Rmd  
  )+
  labs(title = "個人 (或家庭) 購置不動產之借款餘額\n\n",
       caption = "* 個人 (或家庭) 借款餘額係指全體銀行對個人放款餘額，按用途可分為「購置不動產」、「購置動產」、\n「週轉金」與「企業投資」四類。圖中僅呈現購置不動產一項。資料來源：中央銀行金融統計月報。"
  )+
  theme(
    axis.title.x = 
      element_text(
        size = 8,
        hjust = 1.08,
        vjust = 0
        ), 
    axis.line.y.left  = element_blank(),
    axis.line.y.right = element_blank(),
    axis.ticks.y.left = element_blank(),
    axis.ticks.y.right = element_blank(),
    axis.text.y.left = element_text(color = "navy"),
    axis.text.y.right = element_text(color = "khaki3"),
    axis.title.y.left = 
      element_text(
        color = "navy",
        size = 8,
        angle = 0,
        margin = margin(r = -58),
        hjust = 0,
        vjust = 1.2
      ),
    axis.title.y.right = 
      element_text(
        color = "khaki3",
        size = 8,
        angle = 0,
        margin = margin(l = -63),
        vjust = 1.12
      ),
    panel.grid.major.y = element_line(color = "#ececec"),
    plot.title = element_text(
      hjust = 0.5,
      vjust = 0.1
    ),
    plot.caption = element_text(
      size = 8, 
      vjust = 4,
      hjust = 0,
      color = "grey18")
    )
