df <- read.csv("HD_2001.csv") 
# 分享時改寫成 library(readr)
# read_csv("repository_links")


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


qtr <- c("01\nQ1","","","","02","","","","03","","","","04","","","","2005\nQ1","","","",
         "06","","","","07","","","","08","","","","09","","","","2010\nQ1","","","",
         "11","","","","12","","","","13","","","","14","","","","2015\nQ1","","","",
         "16","","","","17","","","","18","","","","19","","","","2020\nQ1","","","",
         "21\nQ1") #\n 表示換行

####################################
#  Ratio_HDRE_HD                   #
####################################
ggplot(data = df,
       aes(x = t))+
  geom_col(
   aes(y = HD_RE/1000000*8),
   width = 0.2,
   fill = "royalblue1",
   #色票 http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
   alpha = 0.35 
  )+
  geom_line(
    aes(y = HD_RE/HD*100),
    size = 1,
    color = "royalblue4")+
  scale_y_continuous(
    name = "佔總借款餘額比例 (%)",
    expand = c(0,0),
    sec.axis = 
     sec_axis(~./8,
              name = "不動產借款餘額 (兆)")
  )+
  expand_limits(y = 80)+
  scale_x_continuous(
    name = "季別",
    expand = c(0,0),
    breaks = seq(2001, 2021, by = 0.25),
    labels = qtr
  )+
  labs(title = "個人 (或家庭) 購置不動產之借款餘額",
       caption = "* 個人 (或家庭) 借款餘額係指全體銀行對個人放款餘額，\n按用途可分為「購置不動產」、「購置動產」、「週轉金」\n與「企業投資」四類。圖中僅呈現購置不動產一項。\n資料來源：中央銀行金融統計月報。"
  )+
  theme(
    axis.title.x = element_text(size = 10), 
    axis.line.y.left = element_line(color = "royalblue4"),
    axis.ticks.y.left = element_line(color = "royalblue4"),
    axis.text.y.left = element_text(color = "royalblue4"),
    #axis.title.y.left = element_text(color = "royalblue4"),
    axis.line.y.right = element_line(color = "royalblue1"),
    axis.ticks.y.right = element_line(color = "royalblue1"),
    axis.text.y.right = element_text(color = "royalblue1"),
    #axis.title.y.right = element_text(color = "royalblue1"),
    plot.title = element_text(
      hjust = 0.5
    ),
    plot.caption = element_text(
      size = 8, 
      vjust = 8,
      hjust = 0.0,
      color = "grey20")
    )



plot1

plot1 |> plotly::ggplotly()

