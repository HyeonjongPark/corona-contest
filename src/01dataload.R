
############ 패키지 로드

library(data.table)
library(tidyverse)
library(lubridate)

############ 경로설정

setwd("E:/data-analysis/contest/corona-contest/")

############ 데이터로드

roaming1 = read_csv("./data/공모전데이터/2. Roaming_data.csv")
news_list =read.csv("./data/공모전데이터/3-1. NewsList.csv")
country_code = read.csv("./data/외부데이터/ISO국가코드.csv") # 외부데이터 활용


contents = readline("./data/")
contents

############ 데이터 탐색1

roaming1
roaming1$iso %>% unique() # 두자리 국가코드 확인

country_code %>% as.data.table()

news_list$파일명


############ 데이터 전처리


country_code$원어 = NULL
country_code$iso.숫자코드 = NULL
country_code$X3자리코드 = NULL
names(country_code) = c("iso", "iso_koeran", "iso_english")
country_code$iso = tolower(country_code$iso)



############ 데이터 타입 설정

roaming1 %>% str()
roaming1$return = as.Date(as.character(roaming1$return), "%Y%m%d")
roaming1$arrival = as.Date(as.character(roaming1$arrival), "%Y%m%d")
roaming1$departure = as.Date(as.character(roaming1$departure), "%Y%m%d")

roaming1

############ 데이터 통합

roaming2 = merge(roaming1, country_code)
colSums(is.na(roaming2)) # 통합 후 결측치 없음 확인
roaming2 = roaming2 %>% arrange(return)



############ 데이터 탐색2

roaming2 %>% data.table()


# 각 국별 유입자수 확인

return_count_data1 = roaming2 %>% 
  group_by(iso_koeran, return) %>% 
  summarise(sum_count = sum(count))

country_name = "중화인민공화국"
return_count_data1 %>% filter(iso_koeran == country_name) %>% 
  ggplot(aes(x = return, y = sum_count)) +
  geom_line(color = "blue", size = 1) +
  ggtitle(country_name)


# 전체 유입자수 확인

return_count_data2 = roaming2 %>% 
  group_by(return) %>% 
  summarise(sum_count = sum(count))

return_count_data2 %>% ggplot(aes(x = return, y = sum_count)) + 
  geom_line(color = "red", size = 1) +
  ggtitle(country_name)






