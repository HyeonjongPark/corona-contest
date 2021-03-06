#####################
##### 데일리팜 ######
#####################


rm(list=ls())

# cd c:\selenium
# java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-3.9.1.jar -port 4449


library(rvest)
library(stringr)
library(RSelenium)
library(rJava)
library(XML)

ch=wdman::chrome(port=4449L) #크롬드라이버를 포트 4449번에 배정
remDr=remoteDriver(remoteServerAddr = "localhost", port=4449L, browserName='chrome') #remort설정
remDr$open() #크롬 Open
remDr$navigate("https://www.data.go.kr/search/index.do") # 홈페이지 이동

search = remDr$findElement(using = "xpath" , value = '//*[@id="resultQuery"]')
search$sendKeysToElement(list("광역시도")) # 검색어 입력
# /html/body/div[2]/div[3]/div/form/div/img
btn = remDr$findElement(using = "xpath" , value = '//*[@id="sub-main"]/div[4]/div[1]/div[1]/button/i')
btn$clickElement() # 검색

btn2 = remDr$findElement(using = "xpath" , value = '//*[@id="datagridTab"]/a/span')
btn2$clickElement() # 

#btn3 = remDr$findElement(using = "xpath" , value = '/html/body/div[3]/div/div[2]/div[2]/form/input')
#btn3$clickElement() # 재검색

frontPage = remDr$getPageSource()
#frontPage[[1]]


df = data.frame(NO = rep(NA,10), categ = rep(NA,10), data_name = rep(NA,10) , admin = rep(NA,10), url = rep(NA,10), info = rep(NA,10), 
                shape = rep(NA,10), extenstion = rep(NA,10), real_time = rep(NA,10),
                col_name = rep(NA,10), col_sample = rep(NA,10), col_info = rep(NA,10))

basic_url = "https://www.data.go.kr"





for(i in 1:length(total_contents)) {
  df$NO[i] = i
  
  # category
  categ = read_html(frontPage[[1]]) %>% html_nodes('.category-filter.visible-desktop') %>% html_text()
  categ = gsub("\n","", categ)
  categ = gsub(" ", "", categ)
  
  df$categ = categ
  
  
  # data name
  
  
  total_contents = read_html(frontPage[[1]]) %>% html_nodes('.data-title') %>% html_text()
  total_contents = gsub("\n","",total_contents)
  total_contents = gsub("\t","",total_contents)
  total_contents = gsub("조회수","",total_contents)
  total_contents = gsub("[0-9]","",total_contents)
  total_contents = str_split(total_contents, "                                                                                                                ")
  df$data_name[i] = total_contents[[i]][2]
  df$data_name[i] = gsub(" ","",df$data_name[i])

  adm = read_html(frontPage[[1]]) %>% html_nodes('.data-meta') %>% html_text()
  adm = str_split(adm, "\n")
  df$admin[i] = gsub(" ","",adm[[i]][3])
  
  # url
  url = str_split(read_html(frontPage[[1]]) %>% html_nodes('.data-title') ,"\n")[[i]][2]
  df$url[i] = url = gsub("                                                    ", "", url)
  df$url[i] = url = gsub("<a href=\"", "", url)
  df$url[i] = url = gsub("\">", "", url)
  df$url[i] = paste0(basic_url, df$url[i])
  
  #info
  info = read_html(frontPage[[1]]) %>% html_nodes('.data-desc') %>% html_text()
  info = gsub("\n                                                    ","", info)
  df$info[i] = substr(info[i], 1, str_locate(info[i], "[.] "))

  
  # 제공방식 형태
  shape = read_html(frontPage[[1]]) %>% html_nodes('.data-meta') %>% html_text()
  shape = str_split(shape, "\n")
  df$shape[i] = gsub("                                                        ","",shape[[i]][5])
  
  # 확장자명 
  ext = read_html(frontPage[[1]]) %>% html_nodes('.data-types') %>% html_text()
  ext = str_split(ext,"\n") 
  tmp = ext[[i]]
  tmp = tmp[-c(1,2,3)]
  
  ext2 = tmp[nchar(tmp) > 52]
  df$extenstion[i] = paste(gsub(" ","",ext2), collapse = "/")
  
  
  # # 페이지 이동 세부 정보
  # 
  # btn_to_go = remDr$findElement(using = "xpath" , value = paste0('//*[@id="standard-list-wrapper"]/div[', i ,']/div[1]/a'))
  # btn_to_go$clickElement() # 검색
  # 
  # frontPage2 = remDr$getPageSource()
  # 
  # for(j in 1:100) {
  #   a = read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_fixed_cell_st.sbgrid_datagrid1_fixedcol_col_',j,'_style.sbgrid_ellipsis.sbgrid_common')) %>% html_text()
  #   b = read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_data_cell_st.sbgrid_ellipsis.sbgrid_datagrid1_col_col_',j,'_style.sbgrid_common')) %>% html_text()
  #   if(!is.na(a)) {
  #     new_list[[1]]$coln[j] = a
  #     new_list[[1]]$colv[j] = b
  #   }
  #   else break
  # }
  # 
  # btn_to_back = remDr$findElement(using = "xpath" , value = '//*[@id="sub-main"]/div[4]/div/div[2]/div[6]/a')
  # btn_to_back$clickElement() # 
  # 
  print(i)
}

df


#write.csv(df, "C:/Users/onycom/Desktop/workspace/프로젝트/[공간정보수집]/collect1_10.csv")







btn = remDr$findElement(using = "xpath" , value = paste0('//*[@id="standard-list-wrapper"]/div[',1,']/div[1]/a'))
btn$clickElement()


frontPage2 = remDr$getPageSource()

col_name = read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_fixed_cell_st.sbgrid_datagrid1_fixedcol_col_',i,'_style.sbgrid_ellipsis.sbgrid_common')) %>% html_text()

read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_fixed_cell_st.sbgrid_datagrid1_fixedcol_col_',16,'_style.sbgrid_ellipsis.sbgrid_common')) %>% html_text()

new_list = list(data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)),
                data.frame(coln = rep(NA,100), colv = rep(NA,100)))

for(i in 1:10) {
  for(j in 1:100) {
    a = read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_fixed_cell_st.sbgrid_datagrid1_fixedcol_col_',j,'_style.sbgrid_ellipsis.sbgrid_common')) %>% html_text()
    b = read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_data_cell_st.sbgrid_ellipsis.sbgrid_datagrid1_col_col_',j,'_style.sbgrid_common')) %>% html_text()
    if(!is.na(a)) {
      new_list[[1]]$coln[j] = a
      new_list[[1]]$colv[j] = b
    }
    else break
  }
  
  btn_to_back = remDr$findElement(using = "xpath" , value = '//*[@id="sub-main"]/div[4]/div/div[2]/div[6]/a')
  btn_to_back$clickElement() # 
  
}

new_list[[1]]
read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell sbgrid_cell_border_st sbgrid_cell_st sbgrid_data_cell_st sbgrid_ellipsis sbgrid_datagrid1_col_col_',1,'_style sbgrid_common')) %>% html_text()

read_html(frontPage2[[1]]) %>% html_node(paste0('.sbgrid_cell.sbgrid_cell_border_st.sbgrid_cell_st.sbgrid_data_cell_st.sbgrid_ellipsis.sbgrid_datagrid1_col_col_1_style.sbgrid_common')) %>% html_text()


total_contents = trimws(gsub("건 검색\\)","",total_contents))
total_contents = as.integer(total_contents)


last_page = read_html(frontPage[[1]]) %>% html_nodes('.PageNav') %>%html_text()
last_page = gsub("\t" , "" ,last_page)
last_page = gsub("\n" , "" ,last_page)
last_page
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


last_page = as.integer(substrRight(last_page,1))


df = data.frame(title = rep(NA,total_page) , main = rep(NA,total_page))
for(i in 0:last_page-1) {
  for(j in 1:10) {
    
    btn4 = remDr$findElement(using = "xpath" , value = paste0('/html/body/div[3]/div/div[2]/div[3]/ul/li[',j,']/a'))
    btn4$clickElement() # 페이지 내부 접속
    frontPage = remDr$getPageSource()
    df$main[(i*10 + j)] = read_html(frontPage[[1]]) %>% html_nodes('.newsContents.font1')%>% html_text()
    df$main[(i*10 + j)] = gsub("\n","",df$main[(i*10 + j)])
    df$main[(i*10 + j)] = gsub("\t","",df$main[(i*10 + j)])
    df$main[(i*10 + j)] = trimws(gsub("<U+.*>", "", df$main[(i*10 + j)])) # <U~~> 제거
    df$main[(i*10 + j)] = trimws(gsub("//슬라이드.*리스트보기카드보기","",df$main[(i*10 + j)]))
    
    df$title[(i*10 + j)] = read_html(frontPage[[1]]) %>%  html_nodes('.newsTitle') %>% html_text()
    df$title[(i*10 + j)] = gsub("\t","",df$title[(i*10 + j)])
    df$title[(i*10 + j)] = gsub("\n","",df$title[(i*10 + j)])
    df$title[(i*10 + j)] = trimws(gsub("<U+.*>", "", df$title[(i*10 + j)]))
    
    Sys.sleep(3)
    remDr$goBack() # 뒤로가기
    
  }
  btn5 = remDr$findElement(using = "xpath" , value = paste0('/html/body/div[3]/div/div[2]/div[3]/div[2]/a[',(i+2),']')) # 페이지 이동
  btn5$clickElement()
  Sys.sleep(3)
}

remDr$close() #크롬 Close
df

setwd("C:/Users/guswh/Desktop/잡동")
write.csv(df,"dailypharm.csv")




## 연습
df[1]
kkk = df[1,2]
gsub("//슬라이드.*리스트보기카드보기      ","",kkk)
trimws(gsub(".*.리스트보기카드보기","",kkk))

trimws(gsub("^\\s*<U\\+\\w+>|-", "", df$title[(i*10 + j)]))

abc = "abcd<U+00A0>asdasdasd"
gsub("<U+.*>", "",abc)
trimws(gsub("^\\s*<U\\+\\w+>|-", " ", abc))

read_html(frontPage[[1]]) %>%  html_nodes('.newsTitle') %>% html_text()

btn4 = remDr$findElement(using = "xpath" , value = '/html/body/div[3]/div/div[2]/div[3]/ul/li[1]/a')
btn4$clickElement()

frontPage = remDr$getPageSource() #페이지 전체 소스 가져오기

abc = read_html(frontPage[[1]]) %>% html_nodes('.newsContents.font1')%>% html_text()
gsub("\\<-\\>","", abc)


btn5 = remDr$findElement(using = "xpath" , value = paste0('/html/body/div[3]/div/div[2]/div[3]/div[2]/a[',(2),']'))
btn5$clickElement()

remDr$close() #크롬 Close

###