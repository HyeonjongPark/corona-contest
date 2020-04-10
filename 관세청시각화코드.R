#install.packages("ggthemes")
library(devtools)
#install.packages("backports")
#install.packages("fs")
library(backports)
library(fs)
#devtools::install_github('cttobin/ggthemr')
library(patchwork)
library(ggthemes)
library(ggthemr)

origin_price = data.frame(week = 41:52,
           price = rnorm(12,399,2.5),
           kinds = "원가")
sale_price = data.frame(week = 41:52,
                        price = c(rnorm(7,390,2.5), rnorm(5, 198,1)),
                        kinds = "할인가")
custom_price = data.frame(week = 41:52,
                          price = c(rnorm(9,390,2.5), rnorm(3, 198,1)),
                          kinds = "세관 신고가")

data = rbind(origin_price,sale_price,custom_price)
data
#write.csv(data, "C:/Users/onycom/Desktop/workspace/프로젝트/[관세청]/price_data.csv")

library(glue)
library(ggplot2)
library(dplyr)
p = data %>% ggplot(aes(x = week, y =price, color = kinds)) + geom_line(size = 1) + 
  geom_vline(xintercept = 48, color = "grey", size = 1.5) +
  annotate("text", x=48, y=185, label="블랙프라이데이 시작", size=5, color = "red") + 
  ggtitle("주별 상품 가격") + xlab("시간 단위(주)") + ylab("가격($)") +
  theme_economist()
  #theme_hc(bgcolor = "darkunica") + scale_colour_hc("darkunica")
p




options(scipen = 100)

diff_price_a = data %>% filter(kinds == "원가") %>% select(price) - data %>% filter(kinds == "할인가") %>% select(price)


#diff_price_a$sd = NA
#for(i in 1:nrow(diff_price_a)){
#  diff_price_a$sd[i] = sd(diff_price_a$price[1:i])
#}

#diff_price_a$sd[1] = 0.2
#diff_price_a$week = 41:52


diff_price_a$sd = NA
for(i in 1:11){
  diff_price_a$sd[i+1] = sd(diff_price_a$price[(i):(i+1)])
}

diff_price_a$sd[1] = 0.2
diff_price_a$week = 41:52





p1 = diff_price_a %>% ggplot(aes(x = week, y =sd)) + geom_line(size = 1, color = "orange") +
  geom_hline(yintercept = 10, color = "yellow", size = 1.5) +
  annotate("text", x=43, y=12, label="허용 표준편차", size=3) +
  annotate("text", x=48, y=23, label="특이치 \n알람발생", size=5, color = "red") +
  ggtitle("원가 - 할인가 의 누적 표준편차") + xlab("시간 단위(주)")+
  ylab("표준편차")

  

p+p1







diff_price_b = data %>% filter(kinds == "할인가") %>% select(price) - data %>% filter(kinds == "세관 신고가") %>% select(price)
diff_price_b


#diff_price_b$sd = NA
#for(i in 1:nrow(diff_price_b)){
#  diff_price_b$sd[i] = sd(diff_price_b$price[1:i])
#}

#diff_price_b$sd[1] = -1.8
#diff_price_b$week = 41:52



diff_price_b$sd = NA
for(i in 1:11){
  diff_price_b$sd[i+1] = sd(diff_price_b$price[(i):(i+1)])
}

diff_price_b$sd[1] = 1
diff_price_b$week = 41:52



diff_price_b %>% ggplot(aes(x = week, y =sd)) + geom_line(size = 1, color = "blue") +
  ggtitle("세일가 신고가 가격차이 표준편차")









diff_price_aa = diff_price_a
diff_price_aa$sor = "원가 - 할인가"

diff_price_bb = diff_price_b
diff_price_bb$sor = "할인가 - 세관 신고가"


mer = rbind(diff_price_aa, diff_price_bb)

p2 = mer %>% ggplot(aes(x = week, y = sd, color = sor)) + geom_line(size = 1) +
  annotate("text", x=50, y=100, label="특이치 회복", size=5, color = "blue") +
  annotate("text", x=48, y=100, label="특이치 발생 경고", size=5, color = "red") +
  ggtitle("2주간 가격차이 표준편차") + xlab("시간 단위(주)")+
  ylab("표준편차") +
  theme_economist()
  #theme_hc(bgcolor = "darkunica") + scale_colour_hc("darkunica")
  #ggthemr("earth", type="outer", layout="scientific", spacing=2)

p2

p + p2


