## 주간 경제레포트

 
## 주간 경제레포트 일반 설정

Sys.setlocale(category = "LC_ALL", locale = "KOR")

library(lubridate)
library(RODBC)
library(reshape2)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(zoo)
library(quantmod)
library(plyr)

ch <- odbcConnect("funddb", uid="******", pwd="******")
ch1 <- odbcConnect("realdb", uid="******", pwd="******")

options(scipen = 999)
### Fonts
windowsFonts(RixMGOB = windowsFont('RixMGO B'))
windowsFonts(RixMGOL = windowsFont('RixMGo L'))

path1 <- "C:\\Users\\fnguide\\Desktop\\프로젝트\\경제리포트\\시황\\주간레포트"
d <- as.character(today()); d <- gsub(pattern = '-', replacement = '', d)
path <-paste(path1, sep = '\\', d)
dir.create(path = path)
setwd(path) 
### Themes

# It creates Date Query 
dateCreater <- function(PrvDate, Nextdate = today()) {
  # PrvDate format 'YYYYMMDD'
  # Otherwise NextDate Format should be 'yyyy-mm-dd'
  s <- 'between'
  a <- 'and'
  if(class(Nextdate) == "Date") {
    NtDay <- format(Nextdate, '%Y%m%d')  
  } else {
    NtDay <- format(as.Date(Nextdate, '%Y%m%d'), '%Y%m%d') 
  }
  
  p1 <- paste(s, PrvDate)
  p1 <- paste(p1, a)
  p1 <- paste(p1, NtDay)
  p1
}

# Merging Query 
QueryCreater <- function(Query, Date){
  # Date from DateCreator 
  # Or the Date format should be YYYYMMDD
  Q <- paste0(Query, Date)  
  Q  
}

# Calculating Date

DateRange <- function(Setdate, m = 3, w = 0){
  a <- Setdate - period(month = m, week = w)
  b <- Setdate
  while(is.na(a) != FALSE){
    b <- b - 1
    a <- b - period(month = m, week = w)
  }
  format(a, "%Y%m%d")
}

# Function to get the data from SQL

GetSQL <- function(Query, Date){
  d <- QueryCreater(Query, Date)
  d <-  sqlQuery(ch, d)
  d
}

"""
주간 경제레포트 Date 설정
"""

## 만약 Value 가 NA 값이 나오면
## m 값을 12 이상으로 변경해주세요.

d_y <- as_date("2017-09-30")
# Date
dty <- dateCreater(PrvDate = DateRange(d_y, m = 12),  Nextdate = d_y)
econ_dty <- dateCreater(PrvDate = DateRange(d_y, m = 24),  Nextdate = d_y)
bd_dty <- dateCreater(PrvDate = DateRange(d_y, m = 12),  Nextdate = d_y)

# 순매수용
KOS_dty <- dateCreater(PrvDate = DateRange(d_y, m = 6), Nextdate = d_y)


# Extract Date Function(d_y, 1d, 1w, 1m, 3m, 6m, YTD, 1Y)

DateExtract <- function(Data, Dt = date){
  Td <- Dt; Oned <- Dt - 1; Onew <- ymd(DateRange(Dt, m = 0, w = 1))
  Onem <- ymd(DateRange(Dt, m = 1, w = 0)); Thm <- ymd(DateRange(Dt, m = 3, w = 0))
  Sixm <- ymd(DateRange(Dt, m = 6, w = 0)); Oney <- ymd(DateRange(Dt, m = 12, w = 0))
  YTD <- ymd(DateRange(as_date(paste0(year(today()),"-12-31")), m = 12))
  Dt <- Data[Data[,1] %in% c(Td, Oned, Onew, Onem, Thm,
                               Sixm, YTD, Oney),]  
  if(month(d_y) < 5) {

    Dt
  }
  else{
    Dt[c(1,3,2,4,5,6,7,8),]
  }
}

Table_Rate <- function(Data, date = d_y)
{
  Dummy <- DateExtract(Data, date); Dt <- Dummy$TRD_DT;
  Dummy1 <- Dummy[Dummy[,1] == date,][,2:ncol(Dummy)]; 
  Dummy <- Dummy[,2:ncol(Dummy)]; DM <-  (Dummy1[rep(seq_len(nrow(Dummy1)), 
                                                     each=length(Dt)),] - Dummy) / Dummy * 100
  rownames(DM) <- Dt; DM <- as.data.frame(t(DM)); DM <- round(DM[c(ncol(DM):1)], 2)
  DM[,1] <- t(Dummy1); DM
}
d_y
Table_Rate_Bond <- function(Data, date = d_y)
{
  Dummy <- DateExtract(Data, date); Dt <- Dummy$TRD_DT;
  Dummy1 <- Dummy[Dummy[,1] == date,][,2:ncol(Dummy)]; 
  Dummy <- Dummy[,2:ncol(Dummy)]; DM <-  (Dummy1[rep(seq_len(nrow(Dummy1)), 
                                                     each=length(Dt)),] - Dummy) * 100
  rownames(DM) <- Dt; DM <- as.data.frame(t(DM)); DM <- round(DM[c(ncol(DM):1)], 2)
  DM[,1] <- t(Dummy1); DM
}

CmTheme <- theme(plot.title = element_text(family = 'RixMGOB', size = 12),
                 axis.title.x = element_text(family = 'RixMGOB', size = 10),
                 axis.title.y = element_text(family = 'RixMGOB', size = 10),
                 axis.text.x = element_text(family = 'RixMGOL', size = 8,
                                            hjust = 0.5, vjust = 0.5, color = 'black', angle = 30),
                 axis.text.y = element_text(family = 'RixMGOL', size = 10, color = 'black')) +
                 theme(panel.background = element_blank(),
                       panel.grid.major.x = element_blank(),
                       panel.border = element_blank()) +
                 theme(axis.line.x = element_line(color = 'black',size = 0.8),
                       axis.line.y = element_line(color = 'black',size = 0.8)) +
                 theme(legend.text = element_text(family = 'RixMGOL', size = 10, color = 'black'),
                       legend.title = element_blank(), legend.key = element_blank(),
                       legend.position = 'top')   


"""
주간경제레포트 주식
"""

## 주가 지수
SQL_STOCK_IDX <- "
-- 코스피종합/ 코스피200 / 코스닥 / MKF500 / MKF가치 / MKF성장 / 대형주 / 소형주 / 중형주
select sdf.trd_dt, aa.cls_prc 코스피, bb.cls_prc 코스피200,
cc.cls_prc 코스닥, dd.cls_prc MK500, ee.cls_prc MK가치,
ff.cls_prc MK성장, gg.cls_prc 대형주, hh.cls_prc 중형주,
ii.cls_prc 소형주
from 
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20150512' and '20170512'
) sdf
  left join 
  (
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'I.001' and 
  a.trd_dt between '20150512' and '20170512'
  ) aa
  on sdf.trd_dt = aa.trd_dt
  left join 
  (
  -- 코스피 200
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'I.101' and 
  a.trd_dt between '20150512' and '20170512'
  ) bb
  on sdf.trd_dt = bb.trd_dt
  left join
  (
  -- 코스닥
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'I.201' and 
  a.trd_dt between '20150512' and '20170512'
  ) cc
  on sdf.trd_dt = cc.trd_dt
  left join
  (
  -- MK500
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'FI00' and 
  a.trd_dt between '20150512' and '20170512'            
  ) dd 
  on sdf.trd_dt = dd.trd_dt
  left join 
  (
  -- MK 가치
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'FI00.VAL' and 
  a.trd_dt between '20150512' and '20170512'
  ) ee
  on sdf.trd_dt = ee.trd_dt
  left join 
  (
  -- MK 성장
  select a.trd_dt, a.cls_prc
  from FNS_UD a 
  where u_cd = 'FI00.GRO' and 
  a.trd_dt between '20150512' and '20170512'
  ) ff
  on sdf.trd_dt = ff.trd_dt
  left join
  (
  -- 대형주
  select a.trd_dt, a.cls_prc 
  from FNS_UD a 
  where u_cd = 'FI00.LAR' and 
  a.trd_dt between '20150512' and '20170512'
  ) gg
  on sdf.trd_dt = gg.trd_dt
  left join
  (
  -- 중형주
  select a.trd_dt, a.cls_prc 
  from FNS_UD a 
  where u_cd = 'FI00.MID' and 
  a.trd_dt between '20150512' and '20170512'
  ) hh
  on sdf.trd_dt = hh.trd_dt
  left join
  (
  -- 소형주
  select a.trd_dt, a.cls_prc 
  from FNS_UD a 
  where u_cd = 'FI00.SMA' and 
  a.trd_dt between '20150512' and '20170512'
  ) ii
  on sdf.trd_dt = ii.trd_dt
  order by sdf.trd_dt
  "
SQL_STOCK_IDX <- gsub(pattern = "between '20150512' and '20170512'", replacement = dty, x = SQL_STOCK_IDX)
DT_STOCK_IDX <- sqlQuery(ch1, SQL_STOCK_IDX)
DT_STOCK_IDX$TRD_DT <- ymd(DT_STOCK_IDX$TRD_DT)
DT_STOCK_IDX <- na.locf(DT_STOCK_IDX)
DT_STOCK_IDX$TRD_DT <- as_date(DT_STOCK_IDX$TRD_DT)
DT_STOCK_IDX[,2:ncol(DT_STOCK_IDX)] <- colwise(as.numeric)(DT_STOCK_IDX[2:ncol(DT_STOCK_IDX)])
write.csv(x = DT_STOCK_IDX, file = 'DT_STOCK_IDX.csv')  


DT_STOCK_IDX_Dum <- colwise(as.numeric)(DT_STOCK_IDX[2:ncol(DT_STOCK_IDX)])
DT_STOCK_IDX_Dum <- colwise(ROC)(DT_STOCK_IDX_Dum) + 1 
DT_STOCK_IDX_Dum[1,] <- rep(1,ncol(DT_STOCK_IDX_Dum))
DT_STOCK_IDX_Dum <- colwise(cumprod)(DT_STOCK_IDX_Dum) * 100
DT_STOCK_IDX_Dum$TRD_DT <- as_date(DT_STOCK_IDX$TRD_DT)

write.csv(x = DT_STOCK_IDX_Dum, file = 'DT_STOCK_IDX_Dum.csv')  

## 주간 경제레포트 그래프
# 코스피
KPKD_1 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = 코스피, color = '코스피'), lwd = 1) + 
  scale_color_manual(values = c('코스피' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1],
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('코스피 지수') + CmTheme

ggsave(filename = 'KPKD_1.png',plot = KPKD_1, width = 5, height = 3, units = "in")

# 코스닥
KPKD_2 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = 코스닥, color = '코스닥'), lwd = 1) + 
  scale_color_manual(values = c('코스닥' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1],
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('코스닥 지수') + CmTheme

ggsave(filename = 'KPKD_2.png',plot = KPKD_2, width = 5, height = 3, units = "in")

# 코스피200

KPKD_3 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
          geom_line(aes(y = 코스피200, color = '코스피200'), lwd = 1) + 
          scale_color_manual(values = c('코스피200' = 'brown3')) +
          scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
                       limits= c(DT_STOCK_IDX$TRD_DT[1],
                       DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
                       expand = c(0,1)) +
          xlab('일자') + ylab('지수') + ggtitle('코스피200 지수') + CmTheme

ggsave(filename = 'KPKD_3.png',plot = KPKD_3, width = 5, height = 3, units = "in")

# MK500
KPKD_4 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = MK500, color = 'MKF500'), lwd = 1) + 
  scale_color_manual(values = c('MKF500' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('MKF500 지수') + CmTheme

ggsave(filename = 'KPKD_4.png',plot = KPKD_4, width = 5, height = 3, units = "in")

# MK가치
KPKD_5 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = MK가치, color = 'MKF가치'), lwd = 1) + 
  scale_color_manual(values = c('MKF가치' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('MKF가치 지수') + CmTheme

ggsave(filename = 'KPKD_5.png',plot = KPKD_5, width = 5, height = 3, units = "in")

# MK성장
KPKD_6 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = MK성장, color = 'MKF성장'), lwd = 1) + 
  scale_color_manual(values = c('MKF성장' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('MKF성장 지수') + CmTheme

ggsave(filename = 'KPKD_6.png',plot = KPKD_6, width = 5, height = 3, units = "in")

# 대형주
KPKD_7 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = 대형주, color = '대형주'), lwd = 1) + 
  scale_color_manual(values = c('대형주' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('대형주 지수') + CmTheme

ggsave(filename = 'KPKD_7.png',plot = KPKD_7, width = 5, height = 3, units = "in")

# 중형주
KPKD_8 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = 중형주, color = '중형주'), lwd = 1) + 
  scale_color_manual(values = c('중형주' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('중형주 지수') + CmTheme

ggsave(filename = 'KPKD_8.png',plot = KPKD_8, width = 5, height = 3, units = "in")

# 소형주
KPKD_9 <- ggplot(DT_STOCK_IDX, aes(x = TRD_DT)) +
  geom_line(aes(y = 소형주, color = '소형주'), lwd = 1) + 
  scale_color_manual(values = c('소형주' = 'brown3')) +
  scale_x_date(date_breaks = '2 months',date_labels = '%Y년 %B',
               limits= c(DT_STOCK_IDX$TRD_DT[1], 
                         DT_STOCK_IDX$TRD_DT[length(DT_STOCK_IDX$TRD_DT)]),
               expand = c(0,1)) +
  xlab('일자') + ylab('지수') + ggtitle('소형주 지수') + CmTheme

ggsave(filename = 'KPKD_9.png',plot = KPKD_9, width = 5, height = 3, units = "in")

## 국내주식 테이블 - 주요증시 Value / SML Factor

KOR_ST_Table_1 <- Table_Rate(DT_STOCK_IDX,date = d_y)
write.csv(x = KOR_ST_Table_1, file = 'KOR_ST_Table_1.csv')

SQL_STCK_SECTOR <- "
-- FGSC
select aaaa.trd_dt, aa.에너지, bb.소재, cc.산업재, dd.경기소비재, ee.필수소비재, ff.의료, gg.금융,
hh.IT, ii.통신서비스, jj.유틸리티
from 
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20150512' and '20170512'
) aaaa
  left join
  (
  -- 에너지
  select a.trd_dt, a.cls_prc 에너지
  from FNS_UD a 
  where u_cd = 'FGSC.10' and 
  a.trd_dt between '20150512' and '20170512'
  ) aa 
  on aaaa.trd_dt = aa.trd_dt
  left join
  (
  -- 소재
  select a.trd_dt, a.cls_prc 소재
  from FNS_UD a 
  where u_cd = 'FGSC.15' and 
  a.trd_dt between '20150512' and '20170512'
  ) bb
  on aaaa.trd_dt = bb.trd_dt
  left join
  (
  -- 산업재
  select a.trd_dt, a.cls_prc 산업재
  from FNS_UD a 
  where u_cd = 'FGSC.20' and 
  a.trd_dt between '20150512' and '20170512'
  ) cc
  on aaaa.trd_dt = cc.trd_dt          
  left join
  (
  -- 경기소비재
  select a.trd_dt, a.cls_prc 경기소비재
  from FNS_UD a 
  where u_cd = 'FGSC.25' and 
  a.trd_dt between '20150512' and '20170512'
  ) dd
  on aaaa.trd_dt = dd.trd_dt
  left join
  (
  --필수소비재
  select a.trd_dt, a.cls_prc 필수소비재
  from FNS_UD a 
  where u_cd = 'FGSC.30' and 
  a.trd_dt between '20150512' and '20170512'
  ) ee
  on aaaa.trd_dt = ee.trd_dt
  left join
  (          
  --의료
  select a.trd_dt, a.cls_prc 의료
  from FNS_UD a 
  where u_cd = 'FGSC.35' and 
  a.trd_dt between '20150512' and '20170512'
  ) ff
  on aaaa.trd_dt = ff.trd_dt          
  --금융
  left join
  (
  select a.trd_dt, a.cls_prc 금융
  from FNS_UD a 
  where u_cd = 'FGSC.40' and 
  a.trd_dt between '20150512' and '20170512'
  ) gg
  on aaaa.trd_dt = gg.trd_dt
  left join
  (                    
  --IT
  select a.trd_dt, a.cls_prc IT
  from FNS_UD a 
  where u_cd = 'FGSC.45' and 
  a.trd_dt between '20150512' and '20170512'
  ) hh
  on aaaa.trd_dt = hh.trd_dt
  left join
  (          
  --통신서비스
  select a.trd_dt, a.cls_prc 통신서비스 
  from FNS_UD a 
  where u_cd = 'FGSC.50' and 
  a.trd_dt between '20150512' and '20170512'
  ) ii
  on aaaa.trd_dt = ii.trd_dt
  left join
  (          
  --유틸리티
  select a.trd_dt, a.cls_prc 유틸리티 
  from FNS_UD a 
  where u_cd = 'FGSC.55' and 
  a.trd_dt between '20150512' and '20170512'
  ) jj
  on aaaa.trd_dt = jj.trd_dt
  order by aaaa.trd_dt;
  "
  
SQL_STCK_SECTOR <- gsub(pattern = "between '20150512' and '20170512'", replacement = dty, x = SQL_STCK_SECTOR)
DT_STOCK_SECTOR <- sqlQuery(ch1, SQL_STCK_SECTOR)
DT_STOCK_SECTOR$TRD_DT <- ymd(DT_STOCK_SECTOR$TRD_DT)

DT_STOCK_SECTOR <- na.locf(DT_STOCK_SECTOR)
DT_STOCK_SECTOR[2:ncol(DT_STOCK_SECTOR)] <- colwise(as.numeric)(DT_STOCK_SECTOR[2:ncol(DT_STOCK_SECTOR)])
DT_STOCK_SECTOR$TRD_DT <- ymd(DT_STOCK_SECTOR$TRD_DT)

write.csv(x = DT_STOCK_SECTOR, file = 'DT_STOCK_SECTOR.csv')   

DT_STOCK_SECTOR_Dum <- colwise(as.numeric)(DT_STOCK_SECTOR[2:ncol(DT_STOCK_SECTOR)])
DT_STOCK_SECTOR_Dum <- colwise(ROC)(DT_STOCK_SECTOR_Dum) + 1 
DT_STOCK_SECTOR_Dum[1,] <- rep(1,ncol(DT_STOCK_SECTOR_Dum))
DT_STOCK_SECTOR_Dum <- colwise(cumprod)(DT_STOCK_SECTOR_Dum) * 100
DT_STOCK_SECTOR_Dum$TRD_DT <- as_date(DT_STOCK_SECTOR$TRD_DT)

write.csv(x = DT_STOCK_SECTOR_Dum, file = 'DT_STOCK_SECTOR_Dum.csv')

## 국내주식 테이블 - Industrial Factor

KOR_ST_Table_2 <- Table_Rate(DT_STOCK_SECTOR, date = d_y)
write.csv(x = KOR_ST_Table_2, file = 'KOR_ST_Table_2.csv')

"""
채권테이블
"""

## 쿼리

SQL_BOND_TABLE <- "
select aa.trd_dt, aa.AVG_YTM 콜금리, a.AVG_YTM CD금리, b.AVG_YTM CP금리, c.yield 통안_3개월, d.yield 통안_1년, e.yield 통안_2년, f.yield 산금채, g.yield 금융채AAA,
h.yield 금융채AAP, i.yield 국고1년, j.yield 국고3년, k.yield 국고5년, l.yield 국고10년,
m.yield 국고20년 
from 
(
  -- 콜금리
  select trd_dt, AVG_YTM
  from FF_SKT020
  where index_cd in 
  (
  select orig_cd
  from FF_SIM010
  where substr(FULL_NM,1,4) = 'Call'
  ) and trd_dt between '20160529' and '20170529'
) aa 
  left join 
  (
  -- 콜금리
  select trd_dt, AVG_YTM
  from FF_SKT020
  where index_cd in 
  (
  select orig_cd
  from FF_SIM010
  where substr(FULL_NM,1,2) = 'CD'
  ) and trd_dt between '20160529' and '20170529'
  ) a 
  on aa.trd_dt = a.trd_dt 
  left join
  (
  -- CP
  select trd_dt, AVG_YTM
  from FF_SKT020
  where index_cd in 
  (
  select INDEX_CD
  from FF_SKM010
  where substr(INDEX_NM, 1, 5) = 'CP A1' and
  substr(INDEX_NM, 10, 14) = '(13주)'
  ) and trd_dt between '20160529' and '20170529'
  ) b
  on aa.trd_dt = b.trd_dt
  left join
  (
  --통안증권:3개월
  select trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:' 
  ) and a1.INST_CD = 'C' and a1.exp_month = '0003' 
  and a1.trd_dt between '20160529' and '20170529'
  ) c
  on aa.trd_dt = c.trd_dt
  left join
  (
  --통안증권:1년
  select trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:' 
  ) and a1.INST_CD = 'C' and a1.exp_month = '0012' 
  and a1.trd_dt between '20160529' and '20170529'
  ) d
  on aa.trd_dt = d.trd_dt
  left join
  (
  --통안증권:2년
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:' 
  ) and a1.INST_CD = 'C' and a1.exp_month = '0024' 
  and a1.trd_dt between '20160529' and '20170529'
  ) e
  on aa.trd_dt = e.trd_dt
  left join
  (
  --산금채AAA
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,9) = 'AAA(산금채):' 
  ) and a1.INST_CD = 'C' and a1.exp_month = '0012' 
  and a1.trd_dt between '20160529' and '20170529'
  ) f
  on aa.trd_dt = f.trd_dt
  left join
  (
  --금융채AAA
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,8) = '금융채I-AAA' and
  substr(orig_cd, 8,11) = '0012'  
  ) and a1.INST_CD = 'C' and a1.exp_month = '0012' 
  and a1.trd_dt between '20160529' and '20170529'
  ) g
  on aa.trd_dt = g.trd_dt
  left join
  (
  --금융채AA+
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,8) = '금융채I-AA+' and
  substr(orig_cd, 8,11) = '0012'  
  ) and a1.INST_CD = 'C' and a1.exp_month = '0012' 
  and a1.trd_dt between '20160529' and '20170529'
  ) h
  on aa.trd_dt = h.trd_dt
  left join
  (
  --국고1년 
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '국고채권:' and
  substr(orig_cd, 8,11) = '0012'
  ) and a1.INST_CD = 'C' and a1.exp_month = '0012' 
  and a1.trd_dt between '20160529' and '20170529'
  ) i
  on aa.trd_dt = i.trd_dt
  left join
  (
  --국고3년 
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '국고채권:' and
  substr(orig_cd, 8,11) = '0036'
  ) and a1.INST_CD = 'C' and a1.exp_month = '0036' 
  and a1.trd_dt between '20160529' and '20170529'
  ) j
  on aa.trd_dt = j.trd_dt
  left join
  (
  --국고5년 
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '국고채권:' and
  substr(orig_cd, 8,11) = '0060'
  ) and a1.INST_CD = 'C' and a1.exp_month = '0060' 
  and a1.trd_dt between '20160529' and '20170529'
  ) k
  on aa.trd_dt = k.trd_dt
  left join
  (
  --국고10년 
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '국고채권:' and
  substr(orig_cd, 8,11) = '0120'
  ) and a1.INST_CD = 'C' and a1.exp_month = '0120' 
  and a1.trd_dt between '20160529' and '20170529'
  ) l
  on aa.trd_dt = l.trd_dt
  left join
  (
  --국고20년 
  select a1.trd_dt, a1.yield
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
  where a1.bond_cd in 
  (
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '국고채권:' and
  substr(orig_cd, 8,11) = '0240'
  ) and a1.INST_CD = 'C' and a1.exp_month = '0240' 
  and a1.trd_dt between '20160529' and '20170529'
  ) m
  on aa.trd_dt = m.trd_dt
  order by aa.trd_dt;
  "
  
SQL_BOND_TABLE <- gsub(pattern = "between '20160529' and '20170529'", replacement = dty, x = SQL_BOND_TABLE)
DT_BOND_TABLE <- sqlQuery(ch, SQL_BOND_TABLE)
DT_BOND_TABLE$TRD_DT <- ymd(DT_BOND_TABLE$TRD_DT)
  
DT_BOND_TABLE <- na.locf(DT_BOND_TABLE)
DT_BOND_TABLE[2:ncol(DT_BOND_TABLE)] <- colwise(as.numeric)(DT_BOND_TABLE[2:ncol(DT_BOND_TABLE)])
DT_BOND_TABLE$TRD_DT <- ymd(DT_BOND_TABLE$TRD_DT)
  
write.csv(x = DT_BOND_TABLE, file = 'DT_BOND_TABLE.csv')   
d_y
DT_BOND_TABLE_1 <- Table_Rate_Bond(DT_BOND_TABLE, date = d_y)
write.csv(x = DT_BOND_TABLE_1, file = 'DT_BOND_TABLE_1.csv')  

## 국고채 및 통안채 수익률 6개월

sql_ktb <- "
select a1.trd_dt, a1.exp_month, a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where a1.bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010
where substr(FULL_NM,1,5) = '국고채권:'
) and INST_CD = 'C' and a1.trd_dt "

dt_6m_ktb <- GetSQL(sql_ktb, bd_dty)
dt_6m_ktb$TRD_DT <- ymd(dt_6m_ktb$TRD_DT)
write.csv(x = dt_6m_ktb, file = 'dt_6m_ktb.csv')

## 국고채 금리 변동 그래프
change_ktb <- dcast(dt_6m_ktb, TRD_DT ~ EXP_MONTH)
c <- colnames(change_ktb)[2:length(colnames(change_ktb))]
c_ktb <- paste0('KTB', sep = "_", c)
c_ktb <- paste0(c_ktb,'m')
c_ktb <- c('Date',c_ktb)
colnames(change_ktb) <- c_ktb

#그래프 - 3년, 5년, 10년, 20년
sub_change_ktb <-  
  change_ktb %>%
  select(Date ,KTB_36m, KTB_60m, KTB_120m, KTB_240m) %>%
  melt(id.var = 'Date')

sub_change_ktb$variable <- as.factor(sub_change_ktb$variable)
sub_change_ktb_xlsx <- dcast(sub_change_ktb, Date ~ variable, value.var = "value")

write.csv(sub_change_ktb_xlsx,file = "sub_change_ktb_xlsx.csv")

## 국고채 그래프

ktb_1 <- ggplot(sub_change_ktb) +
  geom_line(aes(x = Date, y = value, color = variable, group = variable), lwd = 1) + 
  xlab('일자') + ylab('수익률') + ggtitle('국고채 수익률 변화') +  
  scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
               limits= c(sub_change_ktb$Date[1], 
                         sub_change_ktb$Date[nrow(sub_change_ktb)]),
               expand = c(0,0)) +
  scale_color_discrete(breaks = c("KTB_36m", "KTB_60m", "KTB_120m", "KTB_240m"),
                       labels = c("KTB 3년", "KTB 5년", "KTB 10년", "KTB 20년")) + CmTheme

write.csv(x = sub_change_ktb, file = "sub_change_ktb.csv")
ggsave(filename = 'ktb_1.png',plot = ktb_1, width = 5, height = 3, units = "in")

## 국고채 장단기 스프래드 

sprd_ktb <- sub_change_ktb %>%
              dcast(Date ~ variable)  %>%
              mutate(TFSpd = KTB_60m - KTB_36m) %>%
              mutate(FtenSpd = KTB_120m - KTB_60m) %>%
              select(Date, TFSpd, FtenSpd)%>%
              melt(id.var = 'Date')


sprd_ktb$variable <- as.factor(sprd_ktb$variable)

sprd_ktb_1 <- ggplot(sprd_ktb) +
  geom_line(aes(x = Date, y = value * 100, color = variable, group = variable), lwd = 1) + 
  xlab('일자') + ylab('bp') + ggtitle('국고채 장단기 스프래드 변화') +  
  scale_x_date(date_breaks = '2 months',date_labels = '%y년 %B',
               limits= c(sprd_ktb$Date[1], sprd_ktb$Date[nrow(sprd_ktb)+5]),
               expand = c(0,5)) +
  scale_color_discrete(breaks = c("TFSpd", "FtenSpd"),
                       labels = c("3-5년", "5-10년")) + CmTheme 

write.csv(x = sprd_ktb, file = 'sprd_ktb.csv')
ggsave(filename = 'sprd_ktb_1.png',plot = sprd_ktb_1, width = 5, height = 3, units = "in")

## 국고채 수익률 커브 그래프

KR_CURVE <- change_ktb %>%
              select(Date, KTB_3m, KTB_6m, KTB_12m, KTB_24m, KTB_36m,
                     KTB_60m, KTB_120m, KTB_240m, KTB_360m, KTB_600m)
## 1개월','3개월', '6개월', '1년', 
## '            2년', '3년', '5년', '10년','30년'

KR_CURVE <- melt(KR_CURVE, id.vars = 'Date')

KR_CURVE_xlsx <- dcast(KR_CURVE, variable ~ Date, value.var = 'value')
write.csv(KR_CURVE_xlsx, file = 'KR_CURVE_xlsx.csv')

KR_yield_dte <- c(d_y,
                  as_date(DateRange(d_y, m = 0, w = 1), '%Y%m%d'),
                  as_date(DateRange(d_y, m = 1, w = 0), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 3), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 6), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 12), '%Y%m%d'))

KR_CURVE <- KR_CURVE %>%
  filter(Date %in% KR_yield_dte)
KR_CURVE$Date <- as.factor(KR_CURVE$Date)

KR_CURVE_1 <- ggplot(data = KR_CURVE, 
                     aes(x = variable, y = value, group = Date, color = Date)) +
  geom_line(lwd = 1) + scale_x_discrete(breaks = c(
    'KTB_3m', 'KTB_6m', 'KTB_12m', 'KTB_24m', 'KTB_36m',
    'KTB_60m', 'KTB_120m', 'KTB_240m', 'KTB_360m', 'KTB_600m'),
                                        labels = c('3개월','6개월', '1년', '2년', 
                                                   '3년', '5년', '10년', '20년','30년',
                                                   '50년')) + 
  scale_color_discrete(labels = c('전년','6개월전','3개월전', '1개월 전', '1주 전',
                                  '당일')) + 
  ggtitle("국고채 기간별 수익률 커브") + xlab("잔존년수") + ylab("수익률") + CmTheme

ggsave(filename = 'KR_CURVE_1.png', plot 
       = KR_CURVE_1, width = 5, height = 3, units = "in")




## 통안채 수익률 쿼리

sql_msb <- "
select a1.trd_dt, a1.exp_month, a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where a1.bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010 
where substr(FULL_NM,1,5) = '통안증권:'
) and a1.INST_CD = 'C' and a1.trd_dt "

dt_6m_msb <- GetSQL(sql_msb, bd_dty)
dt_6m_msb$TRD_DT <- ymd(dt_6m_msb$TRD_DT)
write.csv(x = dt_6m_msb, file = 'dt_6m_msb.csv')

## 통안채 그래프
change_msb <- dcast(dt_6m_msb, TRD_DT ~ EXP_MONTH)
c <- colnames(change_msb)[2:length(colnames(change_msb))]
c_msb <- paste0('MSB', sep = "_", c)
c_msb <- paste0(c_msb,'m')
c_msb <- c('Date',c_msb)
colnames(change_msb) <- c_msb

#1Y, 2Y
sub_change_msb <- 
  change_msb %>%
  select(Date ,MSB_12m, MSB_24m) %>%
  melt(id.var = 'Date')

sub_change_msb$variable <- as.factor(sub_change_msb$variable)

sub_change_msb$variable <- as.factor(sub_change_msb$variable)
sub_change_msb_xlsx <- dcast(sub_change_msb, Date ~ variable, value.var = "value")

write.csv(sub_change_msb_xlsx,file = "sub_change_msb_xlsx.csv")


msb_1 <- ggplot(sub_change_msb) +
  geom_line(aes(x = Date, y = value, color = variable, group = variable), lwd = 1) + 
  xlab('일자') + ylab('수익률') + ggtitle('통안채 수익률 변화') +  
  scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
               limits= c(sub_change_msb$Date[1], 
                         sub_change_msb$Date[nrow(sub_change_msb)]),
               expand = c(0,0)) +
  scale_color_discrete(breaks = c("MSB_12m", "MSB_24m"),
                       labels = c("MSB 1년", "MSB 2년")) + CmTheme

ggsave(filename = 'msb_1.png',plot = msb_1, width = 5, height = 3, units = "in")
write.csv(x = sub_change_msb, file = 'sub_change_msb.csv')



## BEI 스프레드
# BEI 쿼리
sql_BEI <- "
          select c.trd_dt, c.itemnm, c.yield 
          from FF_SKT030 c 
          where c.itemcd in 
          (
              select ITEMCD
              from 
              (
              select ITEMNM, ITEMCD, ISSUE_DT
              from FF_SKM030
              where substr(itemnm, 1, 2) = '물가'
              order by ISSUE_DT DESC
              )    
              where rownum = 1
          union all
              select ITEMCD
              from
              (
              select ITEMNM, ITEMCD, ISSUE_DT
              from FF_SKM030 
              where substr(itemnm, 1, 2) = '국고' and
              TO_NUMBER(substr(EXP_DT, 1, 4)) - 
              TO_NUMBER(substr(ISSUE_DT, 1, 4)) = 10
              and substr(ISSUE_DT,1,4) = 
              (
              select Yr
              from 
              (
              select substr(ISSUE_DT, 1, 4) Yr 
              from FF_SKM030 A
              where substr(A.itemnm, 1, 2) = '물가'
              order by A.ISSUE_DT DESC        
              )
              where rownum = 1
              )
              order by ISSUE_DT DESC
              )
              where rownum = 1
          ) and c.trd_dt "

dte_BEI  <- dateCreater(PrvDate = DateRange(as_date('2016-09-19'), 
                                            m = 0, w = 0), 
                        Nextdate = today())
dt_BEI <- GetSQL(sql_BEI, dte_BEI)
dt_BEI$TRD_DT <- ymd(dt_BEI$TRD_DT)
write.csv(x = dt_BEI, file = 'dt_BEI.csv')  

# BEI 그래프

## BEI 변동지수
# Name of The Security

nmTr <- paste0(substr(as.character(unique(dt_BEI$ITEMNM)[1]), 1, 2), 
               substr(as.character(unique(dt_BEI$ITEMNM)[1]), 14, 17))
nnTips <- paste0(substr(as.character(unique(dt_BEI$ITEMNM)[2]), 1, 2), 
                 substr(as.character(unique(dt_BEI$ITEMNM)[2]), 14, 17))

dtest <- dt_BEI
dtest$ITEMNM <- as.character(dtest$ITEMNM)
dtest$ITEMNM[grepl('국고', dtest$ITEMNM ) == TRUE] <- 'KTB'
dtest$ITEMNM[grepl('물가', dtest$ITEMNM ) == TRUE ] <- 'TIPS'
dtest <- arrange(dtest, ITEMNM, TRD_DT)

BEI <- dtest$YIELD[grepl('KTB', dtest$ITEMNM) == TRUE] - 
       dtest$YIELD[grepl('TIPS', dtest$ITEMNM ) == TRUE]  

dtest <- dtest %>%
         dcast(TRD_DT~ITEMNM)

dtest$BEI <- BEI

write.csv(x = dt_BEI_xlsx, file = 'dt_BEI_xlsx.csv')

BEI_1 <- ggplot(dtest, aes(x = TRD_DT)) +
          geom_line(aes(y = KTB, color = 'KTB'), lwd = 1) + 
          geom_line(aes(y = TIPS, color = 'TIPS'), lwd = 1) +
          geom_area(aes(y = BEI * 2, fill = 'BEI'), alpha = 0.5) +
          scale_color_manual(values = c('KTB' = 'brown3',
                                        'TIPS' = 'cornflowerblue'),
                             labels = c(nmTr, nnTips)) +
          scale_fill_manual(values = c('BEI' = 'gray69'),
                            labels = 'BEI지수') +
          scale_y_continuous(sec.axis = sec_axis(trans = ~. / 2 * 100, name = 'BEI(bp)')) +
          scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                       limits= c(dtest$TRD_DT[1], 
                                 dtest$TRD_DT[length(dtest$TRD_DT)]),
                       expand = c(0,5)) +
          xlab('일자') + ylab('수익률') + ggtitle('BEI 스프래드') + 
          CmTheme 

write.csv(x = dtest, file = 'dtest.csv')  
ggsave(filename = 'BEI_1.png', plot 
       = BEI_1, width = 5, height = 3, units = "in")

###  은행채 AAA 수익률

sql_Fnb3A <- "select a1.trd_dt, a1.exp_month, a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where a1.bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010 
where substr(FULL_NM,1,8) = '금융채I-AAA'

) and  a1.INST_CD = 'C' and a1.trd_dt "
  
dt_6m_Fnb3A <- GetSQL(sql_Fnb3A, bd_dty)
dt_6m_Fnb3A$TRD_DT <- ymd(dt_6m_Fnb3A$TRD_DT)
write.csv(x = dt_6m_Fnb3A, file = 'dt_6m_Fnb3A.csv')

dt_6m_Fnb3A_xlsx <- dcast(dt_6m_Fnb3A, TRD_DT ~ EXP_MONTH, value.var = "YIELD")
write.csv(dt_6m_Fnb3A_xlsx, file = 'dt_6m_Fnb3A_xlsx.csv')

###  은행채 AA+ 수익률

sql_Fnb2A1P <- "select a1.trd_dt, a1.exp_month, a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where a1.bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010 
where substr(FULL_NM,1,8) = '금융채I-AA+'

) and a1.INST_CD = 'C' and a1.trd_dt "

dt_6m_Fnb2A1P <- GetSQL(sql_Fnb2A1P, bd_dty)
dt_6m_Fnb2A1P$TRD_DT <- ymd(dt_6m_Fnb2A1P$TRD_DT)
write.csv(x = dt_6m_Fnb2A1P, file = 'dt_6m_Fnb2A1P.csv')

change_Fnb3A <- dcast(dt_6m_Fnb3A, TRD_DT ~ EXP_MONTH)
c <- colnames(change_Fnb3A)[2:length(colnames(change_Fnb3A))]
c_Fnb3A <- paste0('Fnb3A', sep = "_", c)
c_Fnb3A <- paste0(c_Fnb3A,'m')
c_Fnb3A <- c('Date',c_Fnb3A)
colnames(change_Fnb3A) <- c_Fnb3A

## 은행채 그래프

# AAA Bank Bond Spread(은행채-통안채(1년), 은행채-국고채(3년))

sub_3y_ktb <- sub_change_ktb %>%
  dcast(Date~variable) %>%
  select(Date, KTB_36m)

sub_1y_msb <- sub_change_msb %>%
  dcast(Date~variable) %>%
  select(Date, MSB_12m)

sprd_Fnb3A <- 
  change_Fnb3A %>%
  select(Date, Fnb3A_12m, Fnb3A_36m) %>%
  mutate(AAA1yrMSB1yr = Fnb3A_12m - sub_1y_msb$MSB_12m) %>%
  mutate(AAA1yrMSB3yr = Fnb3A_36m - sub_3y_ktb$KTB_36m) %>%
  select(Date, AAA1yrMSB1yr, AAA1yrMSB3yr) %>%
  melt(id.vars = 'Date')

sprd_Fnb3A_xlsx <- dcast(sprd_Fnb3A, Date ~ variable, value.var = "value")
write.csv(sprd_Fnb3A_xlsx, file = 'sprd_Fnb3A_xlsx.csv')

sprd_Fnb3A_1 <- ggplot(sprd_Fnb3A) +
  geom_line(aes(x = Date, y = value * 100, color = variable, group = variable), lwd = 1) + 
  xlab('일자') + ylab('bp') + ggtitle('은행AAA-국고채 스프래드 변화') +  
  scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
               limits= c(sprd_Fnb3A$Date[1]+1, 
                         sprd_Fnb3A$Date[nrow(sprd_Fnb3A)]),
               expand = c(0,0)) +
  scale_color_discrete(breaks = c("AAA1yrMSB1yr", "AAA1yrMSB3yr"),
                       labels = c("은행AAA-통안채(1년)", "은행AAA-국고채(3년)")) + CmTheme

ggsave(filename = 'sprd_Fnb3A_1.png',plot = sprd_Fnb3A_1, width = 5, height = 3, units = "in")

# AA+ Bank Bond Spread(은행채-통안채(1년), 은행채-국고채(3년))

change_Fnb2A1P <- dcast(dt_6m_Fnb2A1P, TRD_DT ~ EXP_MONTH)
c <- colnames(change_Fnb2A1P)[2:length(colnames(change_Fnb2A1P))]
c_Fnb2A1P <- paste0('Fnb2A1P', sep = "_", c)
c_Fnb2A1P <- paste0(c_Fnb2A1P,'m')
c_Fnb2A1P <- c('Date',c_Fnb2A1P)
colnames(change_Fnb2A1P) <- c_Fnb2A1P

sprd_Fnb2A1P <- 
  change_Fnb2A1P %>%
  select(Date, Fnb2A1P_12m, Fnb2A1P_36m) %>%
  mutate(AAP1yrMSB1yr = Fnb2A1P_12m - sub_1y_msb$MSB_12m) %>%
  mutate(AAP1yrKTB3yr = Fnb2A1P_36m - sub_3y_ktb$KTB_36m) %>%
  select(Date, AAP1yrMSB1yr, AAP1yrKTB3yr) %>%
  melt(id.vars = 'Date')

sprd_Fnb2A1P_xlsx <- dcast(sprd_Fnb2A1P, Date ~ variable, value.var = "value")
write.csv(sprd_Fnb2A1P_xlsx, file = 'sprd_Fnb2A1P_xlsx.csv')

sprd_Fnb2A1P_1 <- ggplot(sprd_Fnb2A1P) +
  geom_line(aes(x = Date, y = value * 100, color = variable, group = variable), lwd = 1) + 
  xlab('일자') + ylab('bp') + ggtitle('은행AA+-국고채 스프래드 변화') +  
  scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
               limits= c(sprd_Fnb2A1P$Date[1]+1, 
                         sprd_Fnb2A1P$Date[nrow(sprd_Fnb2A1P)]),
               expand = c(0,0)) +
  scale_color_discrete(breaks = c("AAP1yrMSB1yr", "AAP1yrKTB3yr"),
                       labels = c("은행AA+-통안채(1년)", "은행AA+-국고채(3년)")) + CmTheme

write.csv(x = sprd_Fnb2A1P, file = 'sprd_Fnb2A1P.csv')
ggsave(filename = 'sprd_Fnb2A1P_1.png',plot = sprd_Fnb2A1P_1, width = 5, height = 3, units = "in")

### 회사채 AAA 수익률
sql_Corp <- "select a1.trd_dt, a1.exp_month, a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where a1.bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010
where substr(full_nm, 1, 10) = '공모/무보증-AAA'    
) and a1.INST_CD = 'C' and a1.trd_dt "


dt_6m_Corp <- GetSQL(sql_Corp, bd_dty)
dt_6m_Corp$TRD_DT <- ymd(dt_6m_Corp$TRD_DT)
write.csv(x = dt_6m_Corp, file = 'dt_6m_Corp.csv')

## Corp Bond Spread(AAA)
# 3y Spread

change_Corp <- dcast(dt_6m_Corp, TRD_DT ~ EXP_MONTH)
c <- colnames(change_Corp)[2:length(colnames(change_Corp))]
c_Corp <- paste0('Corp', sep = "_", c)
c_Corp <- paste0(c_Corp,'m')
c_Corp <- c('Date',c_Corp)
colnames(change_Corp) <- c_Corp


sprd_Corp <- 
  change_Corp %>%
  select(Date, Corp_36m)

Corp_sub_3y_ktb <- sub_3y_ktb %>%
                      select(KTB_36m)

sprd_Corp$KTB_36m <- Corp_sub_3y_ktb$KTB_36m

sprd_Corp <- sprd_Corp %>%
  mutate(Corp3yrKTB3yr = Corp_36m - KTB_36m) %>%
  select(Date, Corp_36m, KTB_36m,Corp3yrKTB3yr)

sprd_Corp_1 <- ggplot(data = sprd_Corp, aes(x = Date)) +
               geom_line(aes(y = Corp_36m, color = "Corp_36m"), lwd = 1) + 
               geom_line(aes(y = KTB_36m, color = "KTB_36m"), lwd = 1) + 
               geom_area(aes(y = Corp3yrKTB3yr * 5, fill = "Corp3yrKTB3yr"), alpha = 0.5) + 
               scale_y_continuous(sec.axis = sec_axis(trans = ~. / 5 * 100,
                                                      name = 'bp')) +
               scale_color_discrete(breaks = c("Corp_36m", "KTB_36m"),
                                    labels = c("회사AAA(3년)", "국고(3년)")) +
               scale_fill_manual(values = c("Corp3yrKTB3yr" = "grey11"),
                                 labels = "회사채AAA-국고채(3년)") +
               xlab('일자') + ylab('수익률') + ggtitle('회사AAA-국고채 스프래드 변화') +  
               scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                            limits= c(sprd_Corp$Date[1], 
                                      sprd_Corp$Date[nrow(sprd_Corp)+5]),
                            expand = c(0,5)) + CmTheme

write.csv(x = sprd_Corp, file = 'sprd_Corp.csv')  
ggsave(filename = 'sprd_Corp_1.png', plot = sprd_Corp_1, 
       width = 5, height = 3, units = "in")

## 특수채
### 특수채 AAA 수익률
sql_GSE <- "
select  a1.trd_dt,  a1.exp_month,  a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where bond_cd in 
(
  select distinct substr(orig_cd, 1, 7)
  from FF_SIM010
  where substr(FULL_NM,1,7) = '특수채AAA:'
) and a1.INST_CD = 'C' and a1.trd_dt "

dt_6m_GSE <- GetSQL(sql_GSE, bd_dty)
dt_6m_GSE$TRD_DT <- ymd(dt_6m_GSE$TRD_DT)
write.csv(x = dt_10y_GSE, file = 'dt_10y_GSE.csv')
  
### 산금채 AAA 수익률
sql_KDB <- "
select  a1.trd_dt,  a1.exp_month,  a1.yield
from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA a1
where bond_cd in 
(
select distinct substr(orig_cd, 1, 7)
from FF_SIM010
where substr(FULL_NM,1,9) = 'AAA(산금채):'
) and a1.INST_CD = 'C' and a1.trd_dt "

  
dt_6m_KDB <- GetSQL(sql_KDB, bd_dty)
dt_6m_KDB$TRD_DT <- ymd(dt_6m_KDB$TRD_DT)
write.csv(x = dt_10y_KDB, file = 'dt_10y_KDB.csv')


# 특수채 데이터
GSE_KDB_change_ktb <- change_ktb %>%
                        select(Date, KTB_6m, KTB_18m, KTB_30m, KTB_36m)

nrow(GSE_KDB_change_ktb)
nrow(change_GSE)
change_GSE <- dcast(dt_6m_GSE, TRD_DT ~ EXP_MONTH, value.var = 'YIELD')
c <- colnames(change_GSE)[2:length(colnames(change_GSE))]
c_GSE <- paste0('GSE', sep = "_", c)
c_GSE <- paste0(c_GSE,'m')
c_GSE <- c('Date',c_GSE)
colnames(change_GSE) <- c_GSE

sub_change_GSE <- change_GSE %>%
  select(Date, GSE_6m, GSE_18m, GSE_30m, GSE_36m) %>%
  mutate(sprd_GSE_6m = GSE_6m - GSE_KDB_change_ktb$KTB_6m) %>%
  mutate(sprd_GSE_18m = GSE_18m - GSE_KDB_change_ktb$KTB_18m) %>%
  mutate(sprd_GSE_30m = GSE_30m - GSE_KDB_change_ktb$KTB_30m) %>%
  mutate(sprd_GSE_36m = GSE_30m - GSE_KDB_change_ktb$KTB_30m)

# 산금채 데이터

change_KDB <- dcast(dt_6m_KDB, TRD_DT ~ EXP_MONTH, value.var = 'YIELD')
c <- colnames(change_KDB)[2:length(colnames(change_KDB))]
c_KDB <- paste0('KDB', sep = "_", c)
c_KDB <- paste0(c_KDB,'m')
c_KDB <- c('Date',c_KDB)
colnames(change_KDB) <- c_KDB

sub_change_KDB <- change_KDB %>%
  select(Date, KDB_6m, KDB_18m, KDB_30m, KDB_36m) %>%
  mutate(sprd_KDB_6m = KDB_6m - GSE_KDB_change_ktb$KTB_6m) %>%
  mutate(sprd_KDB_18m = KDB_18m - GSE_KDB_change_ktb$KTB_18m) %>%
  mutate(sprd_KDB_30m = KDB_30m - GSE_KDB_change_ktb$KTB_30m) %>%
  mutate(sprd_KDB_36m = KDB_30m - GSE_KDB_change_ktb$KTB_30m)


write.csv(sub_change_GSE, file = 'sub_change_GSE.csv')
write.csv(sub_change_KDB, file = 'sub_change_KDB.csv')

# GSE Plot

sub_GSE_6M <- ggplot(data = sub_change_GSE, aes(x = Date)) + 
              geom_line(aes(y = GSE_6m, color = 'GSE_6m'), lwd = 1) + 
              geom_line(aes(y = sprd_GSE_6m * 10, color = 'sprd_GSE_6m'), lwd = 1) +
              scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                             breaks = c(5, 10, 15, 20, 25),
                                             labels = c(5, 10, 15, 20, 25),
                                             name = 'bp')) + 
              xlab('일자') + ylab('수익률') + ggtitle('특수채AAA-국고채(6M) 변화') +  
              scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                           limits= c(sub_change_GSE$Date[1], 
                                     sub_change_GSE$Date[nrow(sub_change_GSE)+5]),
                           expand = c(0,5)) +
              scale_color_manual(values = c('GSE_6m' = 'brown3',
                                            'sprd_GSE_6m' = 'cornflowerblue'),
                                 labels = c('특수채 AAA 6M(좌)', '특수채 AAA 6M \nSpread(우)')) + 
              CmTheme 

ggsave(filename = 'sub_GSE_6M.png', plot = sub_GSE_6M, 
       width = 5, height = 3, units = "in")

sub_GSE_18M <- ggplot(data = sub_change_GSE, aes(x = Date)) + 
                geom_line(aes(y = GSE_18m, color = 'GSE_18m'), lwd = 1) + 
                geom_line(aes(y = sprd_GSE_18m * 10, color = 'sprd_GSE_18m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('특수채AAA-국고채 변화(1.5Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_GSE$Date[1], 
                                       sub_change_GSE$Date[nrow(sub_change_GSE)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('GSE_18m' = 'brown3',
                                              'sprd_GSE_18m' = 'cornflowerblue'),
                                   labels = c('특수채 AAA 1.5Y(좌)', '특수채 AAA 1.5Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_GSE_18M.png', plot = sub_GSE_18M, 
       width = 5, height = 3, units = "in")

sub_GSE_30M <- ggplot(data = sub_change_GSE, aes(x = Date)) + 
                geom_line(aes(y = GSE_30m, color = 'GSE_30m'), lwd = 1) + 
                geom_line(aes(y = sprd_GSE_30m * 10, color = 'sprd_GSE_30m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('특수채AAA-국고채 변화(2.5Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_GSE$Date[1], 
                                       sub_change_GSE$Date[nrow(sub_change_GSE)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('GSE_30m' = 'brown3',
                                              'sprd_GSE_30m' = 'cornflowerblue'),
                                   labels = c('특수채 AAA 2.5Y(좌)', '특수채 AAA 2.5Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_GSE_30M.png', plot = sub_GSE_30M, 
       width = 5, height = 3, units = "in")

sub_GSE_36M <- ggplot(data = sub_change_GSE, aes(x = Date)) + 
                geom_line(aes(y = GSE_36m, color = 'GSE_36m'), lwd = 1) + 
                geom_line(aes(y = sprd_GSE_36m * 10, color = 'sprd_GSE_36m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('특수채AAA-국고채 변화(3Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_GSE$Date[1], 
                                       sub_change_GSE$Date[nrow(sub_change_GSE)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('GSE_36m' = 'brown3',
                                              'sprd_GSE_36m' = 'cornflowerblue'),
                                   labels = c('산금채 AAA 3Y(좌)', '산금채 AAA 3Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_GSE_36M.png', plot = sub_GSE_36M, 
       width = 5, height = 3, units = "in")

# KDB Plot

sub_KDB_6M <- ggplot(data = sub_change_KDB, aes(x = Date)) + 
              geom_line(aes(y = KDB_6m, color = 'KDB_6m'), lwd = 1) + 
              geom_line(aes(y = sprd_KDB_6m * 10, color = 'sprd_KDB_6m'), lwd = 1) +
              scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                     breaks = c(5, 10, 15, 20, 25),
                                                     labels = c(5, 10, 15, 20, 25),
                                                     name = 'bp')) + 
              xlab('일자') + ylab('수익률') + ggtitle('산금채AAA-국고채(6M) 변화') +  
              scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                           limits= c(sub_change_KDB$Date[1], 
                                     sub_change_KDB$Date[nrow(sub_change_KDB)+5]),
                           expand = c(0,5)) +
              scale_color_manual(values = c('KDB_6m' = 'brown3',
                                            'sprd_KDB_6m' = 'cornflowerblue'),
                                 labels = c('산금채 AAA 6M(좌)', '산금채 AAA 6M \nSpread(우)')) + 
              CmTheme 

ggsave(filename = 'sub_KDB_6M.png', plot = sub_KDB_6M, 
       width = 5, height = 3, units = "in")

sub_KDB_18M <- ggplot(data = sub_change_KDB, aes(x = Date)) + 
                geom_line(aes(y = KDB_18m, color = 'KDB_18m'), lwd = 1) + 
                geom_line(aes(y = sprd_KDB_18m * 10, color = 'sprd_KDB_18m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('산금채AAA-국고채 변화(1.5Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_KDB$Date[1], 
                                       sub_change_KDB$Date[nrow(sub_change_KDB)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('KDB_18m' = 'brown3',
                                              'sprd_KDB_18m' = 'cornflowerblue'),
                                   labels = c('산금채 AAA 1.5Y(좌)', '산금채 AAA 1.5Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_KDB_18M.png', plot = sub_KDB_18M, 
       width = 5, height = 3, units = "in")

sub_KDB_30M <- ggplot(data = sub_change_KDB, aes(x = Date)) + 
                geom_line(aes(y = KDB_30m, color = 'KDB_30m'), lwd = 1) + 
                geom_line(aes(y = sprd_KDB_30m * 10, color = 'sprd_KDB_30m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('산금채AAA-국고채 변화(2.5Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_KDB$Date[1], 
                                       sub_change_KDB$Date[nrow(sub_change_KDB)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('KDB_30m' = 'brown3',
                                              'sprd_KDB_30m' = 'cornflowerblue'),
                                   labels = c('산금채 AAA 2.5Y(좌)', '산금채 AAA 2.5Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_KDB_30M.png', plot = sub_KDB_30M, 
       width = 5, height = 3, units = "in")

sub_KDB_36M <- ggplot(data = sub_change_KDB, aes(x = Date)) + 
                geom_line(aes(y = KDB_36m, color = 'KDB_36m'), lwd = 1) + 
                geom_line(aes(y = sprd_KDB_36m * 10, color = 'sprd_KDB_36m'), lwd = 1) +
                scale_y_continuous(sec.axis = sec_axis(trans = ~.* 10,
                                                       breaks = c(5, 10, 15, 20, 25),
                                                       labels = c(5, 10, 15, 20, 25),
                                                       name = 'bp')) + 
                xlab('일자') + ylab('수익률') + ggtitle('산금채AAA-국고채 변화(3Y)') +  
                scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                             limits= c(sub_change_KDB$Date[1], 
                                       sub_change_KDB$Date[nrow(sub_change_KDB)+5]),
                             expand = c(0,5)) +
                scale_color_manual(values = c('KDB_36m' = 'brown3',
                                              'sprd_KDB_36m' = 'cornflowerblue'),
                                   labels = c('산금채 AAA 3Y(좌)', '산금채 AAA 3Y \nSpread(우)')) + 
                CmTheme 

ggsave(filename = 'sub_KDB_36M.png', plot = sub_KDB_36M, 
       width = 5, height = 3, units = "in")

## 단기
SQL_SHORT_Rt <- "
                select a.trd_dt, c.avg_ytm CallRt, a.avg_ytm CD, b.avg_ytm CP 
from 
(
  select trd_dt, AVG_YTM
  from FF_SKT020
  where index_cd in 
  (
  select orig_cd
  from FF_SIM010
  where substr(FULL_NM,1,2) = 'CD'
  ) and trd_dt between 20150531 and 20170531
) a
  inner join
  (
  select TRD_DT, AVG_YTM
  from FF_SKT020
  where index_cd = 
  (
  select INDEX_CD
  from FF_SKM010
  where substr(INDEX_NM, 1, 5) = 'CP A1' and
  substr(INDEX_NM, 10, 14) = '(13주)'
  ) and trd_dt between 20150531 and 20170531
  ) b
  on a.trd_dt = b.trd_dt
  inner join
  (
  select trd_dt, AVG_YTM
  from FF_SKT020
  where index_cd in 
  (
  select orig_cd
  from FF_SIM010
  where substr(FULL_NM,1,4) = 'Call'
  ) and trd_dt between 20150531 and 20170531
  ) c
  on a.trd_dt = c.trd_dt
  order by a.trd_dt
  "
  
SQL_SHORT_Rt <- gsub(pattern = "between 20150531 and 20170531", 
                     replacement = econ_dty, 
                     x = SQL_SHORT_Rt)
DT_SHORT_Rt <- sqlQuery(ch, SQL_SHORT_Rt)
DT_SHORT_Rt <- na.locf(DT_SHORT_Rt)
DT_SHORT_Rt$TRD_DT <- ymd(DT_SHORT_Rt$TRD_DT)

write.csv(DT_SHORT_Rt, file = 'DT_SHORT_Rt.csv')

SHORT_Rt_1 <- ggplot(data = DT_SHORT_Rt, aes(x = TRD_DT)) +
                  geom_line(aes(y = CALLRT, color = "콜"), lwd = 1) + 
                  geom_line(aes(y = CD, color = "CD 3개월"), lwd = 1) + 
                  geom_line(aes(y = CP, color = "CP 3개월"), lwd = 1) + 
                  xlab('일자') + ylab('수익률') + ggtitle('단기 금리 변화') +  
                  scale_x_date(date_breaks = '2 months',date_labels = '%y년 %B',
                               limits= c(DT_SHORT_Rt$Date[1], 
                                         DT_SHORT_Rt$Date[nrow(DT_SHORT_Rt)+5]),
                               expand = c(0,5)) + CmTheme

ggsave(filename = 'SHORT_Rt_1.png', plot = SHORT_Rt_1, width = 5, height = 3, units = "in")
  
## 미국 채권

SQL_UST_BOND <- "
select aa.trd_dt, a.OneM, b.ThreeM, c.SixM, 
d.OneY, e.TwoY, f.ThreeY, g.FiveY,
h.TenY, i.ThirtyY
from
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20150331' and '20170331'
) aa 
  left join
  (
  -- 1개월
  select trd_dt, amount OneM
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.008' and TERM = 'D'
  ) a
  on aa.trd_dt = a.trd_dt
  left join
  (
  -- 3개월
  select trd_dt, amount ThreeM
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.001' and TERM = 'D'
  ) b
  on aa.trd_dt = b.trd_dt
  left join
  (
  -- 6개월
  select trd_dt, amount SixM
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.002' and TERM = 'D'
  ) c
  on aa.trd_dt = c.trd_dt
  left join 
  (
  -- 1년
  select trd_dt, amount OneY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.003' and TERM = 'D'
  ) d
  on aa.trd_dt = d.trd_dt
  left join 
  (
  -- 2년
  select trd_dt, amount TwoY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.005' and TERM = 'D'
  ) e
  on aa.trd_dt = e.trd_dt
  left join 
  (
  -- 3년
  select trd_dt, amount ThreeY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.012' and TERM = 'D'
  ) f
  on aa.trd_dt = f.trd_dt
  left join 
  (
  -- 5년
  select trd_dt, amount FiveY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.007' and TERM = 'D'
  ) g
  on aa.trd_dt = g.trd_dt
  left join 
  (
  -- 10년
  select trd_dt, amount TenY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.004' and TERM = 'D'
  ) h
  on aa.trd_dt = h.trd_dt
  left join 
  (
  -- 30년
  select trd_dt, amount ThirtyY
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.007.006' and TERM = 'D'
  ) i
  on aa.trd_dt = i.trd_dt
  order by aa.trd_dt;
"

SQL_UST_BOND <- gsub(pattern = "between '20150331' and '20170331'", replacement = bd_dty, 
                     x = SQL_UST_BOND)
DT_UST_BOND <- sqlQuery(ch1, SQL_UST_BOND)

DT_UST_BOND <- na.locf(DT_UST_BOND)
DT_UST_BOND[2:ncol(DT_UST_BOND)] <- colwise(as.numeric)(DT_UST_BOND[2:ncol(DT_UST_BOND)])
DT_UST_BOND$TRD_DT <- ymd(DT_UST_BOND$TRD_DT)

write.csv(x = DT_UST_BOND, file = 'DT_UST_BOND.csv')



US_CURVE <- DT_UST_BOND
US_CURVE <- melt(US_CURVE, id.vars = 'TRD_DT')

US_yield_dte <- c(d_y,
                  as_date(DateRange(d_y, m = 0, w = 1), '%Y%m%d'),
                  as_date(DateRange(d_y, m = 1, w = 0), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 3), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 6), '%Y%m%d'),
                  as_date(DateRange(as_date(d_y), m = 12), '%Y%m%d'))

US_CURVE <- US_CURVE %>%
  filter(TRD_DT %in% US_yield_dte)
US_CURVE$TRD_DT <- as.factor(US_CURVE$TRD_DT)

US_CURVE_xlsx <- dcast(US_CURVE, variable ~ TRD_DT, value.var = 'value')
write.csv(US_CURVE_xlsx, file = 'US_CURVE_xlsx.csv')

US_CURVE_1 <- ggplot(data = US_CURVE, 
                     aes(x = variable, y = value, group = TRD_DT, color = TRD_DT)) +
              geom_line(lwd = 1) + scale_x_discrete(breaks = c('ONEM', 'THREEM', 'SIXM',
                                                   'ONEY','TWOY',
                                                   'THREEY', 'FIVEY',
                                                   'TENY', 'THIRTYY'),
                                                    labels = c('1개월','3개월', '6개월', '1년', 
                                                   '            2년', '3년', '5년', '10년','30년')) + 
              scale_color_discrete(labels = c('전년','6개월전','3개월전', '1개월 전', '1주 전',
                                              '당일')) + 
              ggtitle("미 국채 기간별 수익률 커브") + xlab("잔존년수") + ylab("수익률") + CmTheme

ggsave(filename = 'US_CURVE_1.png', plot 
       = US_CURVE_1, width = 5, height = 3, units = "in")


## 미국채 주요금리 변화  3년 5년 10년 30년 
M_DT_UST_BOND <- melt(DT_UST_BOND[c('TRD_DT','THREEM', 
                                    'ONEY', 'THREEY',
                                    'FIVEY', 'TENY',
                                    'THIRTYY')], id.vars = 'TRD_DT')

M_DT_UST_BOND_1 <-ggplot(data = M_DT_UST_BOND, aes(x=TRD_DT, color = variable)) +
                  geom_line(aes(y = value), lwd = 1) +  
                  scale_x_date(date_breaks = '2 months',date_labels = '%y년 %B',
                  limits= c(M_DT_UST_BOND$Date[1], 
                            M_DT_UST_BOND$Date[nrow(M_DT_UST_BOND)+5]),
                            expand = c(0,5)) +
                  scale_color_discrete(breaks = c("THREEM", "ONEY", "THREEY",
                                                  "FIVEY",  "TENY", "THIRTYY"),
                                       labels = c("3개월", "1년", "3년",
                                                  "5년", "10년", "30년")) + 
                  xlab('일자') + ylab('수익률') + 
                  ggtitle('미국채 수익률 변화') + CmTheme

ggsave(filename = 'M_DT_UST_BOND_1.png', plot 
       = M_DT_UST_BOND_1, width = 5, height = 3, units = "in")


## Ted Spread / 미국 장단기 금리차(10년 - 2년) 
SQL_UST_TED <- 
"
select aa.trd_dt, a.amount ThreeM, b.amount Libor, b.amount - a.amount Ted
from
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20161230' and '20170331'
) aa 
  left join
  (
  select trd_dt, amount  
  from FNE_ECO_DATA
  where trd_dt between '20161230' and '20170331' and
  eco_cd in ('14.07.001.007.001')  and TERM = 'D'
  ) a
  on aa.trd_dt = a.trd_dt
  left join 
  (
  select trd_dt, amount 
  from FNE_ECO_DATA
  where trd_dt between '20161230' and '20170331' and
  eco_cd in ('14.07.002.002.029')  and TERM = 'D'
  ) b
  on aa.trd_dt = b.trd_dt
  order by aa.trd_dt
"

SQL_UST_TED <- gsub(pattern = "between '20161230' and '20170331'", replacement = bd_dty, 
                     x = SQL_UST_TED)
DT_UST_TED <- sqlQuery(ch1, SQL_UST_TED)

DT_UST_TED <- na.locf(DT_UST_TED)
DT_UST_TED[2:ncol(DT_UST_TED)] <- colwise(as.numeric)(DT_UST_TED[2:ncol(DT_UST_TED)])
DT_UST_TED$TRD_DT <- ymd(DT_UST_TED$TRD_DT)
write.csv(x = DT_UST_TED, file = 'DT_UST_TED.csv')

UST_TED_1 <- ggplot(data = DT_UST_TED, aes(x = TRD_DT)) +
              geom_line(aes(y = THREEM, color = "미국채_3개월"), lwd = 1) + 
              geom_line(aes(y = LIBOR, color = "리보"), lwd = 1) + 
              geom_area(aes(y = TED, fill = "TED"), alpha = 0.5) + 
              scale_y_continuous(sec.axis = sec_axis(trans = ~. * 100,
                                                     name = 'bp')) +
              scale_fill_manual(values = c("TED" = "grey11"),
                                labels = "TED") +
              xlab('일자') + ylab('수익률') + ggtitle('TED 스프래드 변화') +  
              scale_x_date(date_breaks = '2 months',date_labels = '%y년 %B',
                           limits= c(DT_UST_TED$Date[1], 
                                     DT_UST_TED$Date[nrow(DT_UST_TED)+5]),
                           expand = c(0,5)) + CmTheme

ggsave(filename = 'UST_TED_1.png', plot 
       = UST_TED_1, width = 5, height = 3, units = "in")


SQL_WORLD_BOND <- "
select aa.trd_dt, a.amount JPY_1Y, b.amount JPY_3Y,
c.amount JPY_5Y, d.amount JPY_10Y, e.amount JPY_30Y,
f.amount GER_1Y, g.amount GER_3Y, h.amount GER_5Y,
i.amount GER_10Y, j.amount GER_30Y 
from
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20150331' and '20170331'
) aa 
  left join
  (
  -- 일본 1년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.006.003' and TERM = 'D'
  ) a
  on aa.trd_dt = a.trd_dt
  left join
  (
  -- 일본 3년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.006.008' and TERM = 'D'
  ) b
  on aa.trd_dt = b.trd_dt
  left join
  (
  -- 일본 5년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.006.011' and TERM = 'D'
  ) c
  on aa.trd_dt = c.trd_dt
  left join
  (
  -- 일본 10년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.006.004' and TERM = 'D'
  ) d
  on aa.trd_dt = d.trd_dt
  left join
  (
  -- 일본 30년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.006.009' and TERM = 'D'
  ) e
  on aa.trd_dt = e.trd_dt
  left join
  (
  -- 독일 1년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.002.012' and TERM = 'D'
  ) f
  on aa.trd_dt = f.trd_dt
  left join
  (
  -- 독일 3년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.002.004' and TERM = 'D'
  ) g
  on aa.trd_dt = g.trd_dt
  left join
  (
  -- 독일 5년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.002.007' and TERM = 'D'
  ) h
  on aa.trd_dt = h.trd_dt
  left join
  (
  -- 독일 10년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.002.001' and TERM = 'D'
  ) i
  on aa.trd_dt = i.trd_dt
  left join
  (
  -- 독일 30년
  select trd_dt, amount
  from FNE_ECO_DATA
  WHERE trd_dt between '20150331' and '20170331' and
  eco_cd = '14.07.001.002.005' and TERM = 'D'
  ) j
  on aa.trd_dt = j.trd_dt
  order by aa.trd_dt
"

SQL_WORLD_BOND <- gsub(pattern = "between '20150331' and '20170331'", replacement = bd_dty, 
                    x = SQL_WORLD_BOND)
DT_WORLD_BOND <- sqlQuery(ch1, SQL_WORLD_BOND)

DT_WORLD_BOND <- na.locf(DT_WORLD_BOND)
DT_WORLD_BOND[2:ncol(DT_WORLD_BOND)] <- colwise(as.numeric)(DT_WORLD_BOND[2:ncol(DT_WORLD_BOND)])
DT_WORLD_BOND$TRD_DT <- ymd(DT_WORLD_BOND$TRD_DT)
write.csv(x = DT_WORLD_BOND, file = 'DT_WORLD_BOND.csv')


WRD_BOND_TABLE_1 <- rbind(Table_Rate_Bond(DT_UST_BOND, 
                                          date = d_y), Table_Rate_Bond(DT_WORLD_BOND,
                                                                                               date = d_y))

write.csv(x = WRD_BOND_TABLE_1, file = 'WRD_BOND_TABLE.csv')


"""
임시 그래프
ㅇㅇ
"""

## 미국주식

 SQL_USD_STOCK <- "
                select a.trd_dt, a.cls_prc SP500, b.cls_prc NASDAQ
                from
                (
                    --SP500
                    select trd_dt, cls_prc
                    from fgs_ovs_index_d
                    where trd_dt between '20151231' and '20170331' and
                    ITEM_CD = 'I.GSPC'
                ) a 
                left join
                (
                    --나스닥
                    select trd_dt, cls_prc
                    from fgs_ovs_index_d
                    where trd_dt between '20151231' and '20170331' and
                    ITEM_CD = 'I.IXIC'
                ) b
                on a.trd_dt = b.trd_dt
                "

SQL_USD_STOCK <- gsub(pattern = "between '20151231' and '20170331'", replacement = dty, 
                      x = SQL_USD_STOCK)
DT_USD_STOCK <- sqlQuery(ch1, SQL_USD_STOCK)
DT_USD_STOCK$TRD_DT <- ymd(DT_USD_STOCK$TRD_DT)
write.csv(x = DT_USD_STOCK, file = 'DT_USD_STOCK.csv')

US_STOCK <- DT_USD_STOCK

US_STOCK_1 <- ggplot(US_STOCK, aes(x = TRD_DT)) +
              geom_line(aes(y = SP500 + 3000, color = 'S&P500'), lwd = 1) + 
              geom_line(aes(y = NASDAQ , color = '나스닥'), lwd = 1) +
              scale_color_manual(values = c('S&P500' = 'brown3',
                                '나스닥' = 'cornflowerblue')) + 
              scale_y_continuous(limits = c(min(US_STOCK$NASDAQ) - 100 , max(US_STOCK$NASDAQ) + 100), 
                                 sec.axis = sec_axis(trans = ~. - 3000, name = '지수')) +
              scale_x_date(date_breaks = '3 months', date_labels = '%y년 %B',
                           limits= c(US_STOCK$TRD_DT[1], US_STOCK$TRD_DT[nrow(US_STOCK)]),
                           expand = c(0,0)) +
              xlab('일자') + ylab('지수') + ggtitle('S&P / 나스닥 지수') + CmTheme 

ggsave(filename = 'US_STOCK_1.png', plot 
       = US_STOCK_1, width = 5, height = 3, units = "in")


## 세계증시
# When the first low is empty 
# Then adjust your query by hand.
# Or figure out the easist way as far as you can!

SQL_WORLD_IDX <- "
    select b.trd_dt, c.cls_prc MSCI_ACWI, i.cls_prc MSCI_EAFE, 
           d.cls_prc SP500, e.cls_prc 심천종합, f.cls_prc 상해종합, 
           g.cls_prc 토픽스, h.cls_prc 항셍, j.cls_prc STOXX600,
           k.cls_prc 나스닥
    from
    (
          select trd_dt
          from fnc_calendar
          where trd_dt between '20161230' and '20170331'
    ) b
      left join
      (
          -- MSCI ACWI
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.ISACWI' and 
          a.trd_dt between '20161230' and '20170331'
      ) c
      on b.trd_dt = c.trd_dt
      left join
      (
          -- SP500
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.GSPC' and 
          a.trd_dt between '20161230' and '20170331'
      ) d
      on b.trd_dt = d.trd_dt
      left join
      (
          -- 심천종합
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.SZ399106' and 
          a.trd_dt between '20161230' and '20170331'
      ) e
      on b.trd_dt = e.trd_dt
      left join
      (
          -- 상해종합
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.SSEC' and 
          a.trd_dt between '20161230' and '20170331'
      ) f
      on b.trd_dt = f.trd_dt
      left join
      (
          -- 토픽스
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.TOPX' and 
          a.trd_dt between '20161230' and '20170331'
      ) g
      on b.trd_dt = g.trd_dt
      left join
      (
          -- 항셍
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.HSI' and 
          a.trd_dt between '20161230' and '20170331'
      ) h
      on b.trd_dt = h.trd_dt
      left join
      (
          -- MSCI EAFE
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.ISEFA' and 
          a.trd_dt between '20161230' and '20170331'
      ) i
      on b.trd_dt = i.trd_dt
      left join
      (
          -- STOXX
          select a.trd_dt, a.cls_prc                         
          from fgs_ovs_index_d a  
          where item_cd = 'I.STOXX600' and 
          a.trd_dt between '20161230' and '20170331'
      ) j
      on b.trd_dt = j.trd_dt
      left join
      (
          --나스닥
          select a.trd_dt, a.cls_prc
          from fgs_ovs_index_d a
          where a.trd_dt between '20161230' and '20170331' and
          a.ITEM_CD = 'I.IXIC'
      ) k
      on b.trd_dt = k.trd_dt
      order by b.trd_dt
  "

SQL_WORLD_IDX <- gsub(pattern = "between '20161230' and '20170331'", 
                      replacement = dty, 
                      x = SQL_WORLD_IDX)

DT_WORLD_IDX <- sqlQuery(ch1, SQL_WORLD_IDX)
DT_WORLD_IDX <- na.locf(DT_WORLD_IDX)
DT_WORLD_IDX$TRD_DT <- ymd(DT_WORLD_IDX$TRD_DT)

tail(DT_WORLD_IDX)
WRD_ST_Table_1 <- Table_Rate(DT_WORLD_IDX, date = d_y)
write.csv(x = WRD_ST_Table_1, file = 'WRD_ST_Table_1.csv')

DT_WORLD_IDX_Dum <- colwise(as.numeric)(DT_WORLD_IDX[2:ncol(DT_WORLD_IDX)])
DT_WORLD_IDX_Dum <- colwise(ROC)(DT_WORLD_IDX_Dum) + 1 
DT_WORLD_IDX_Dum[1,] <- rep(1,ncol(DT_WORLD_IDX_Dum))
DT_WORLD_IDX_Dum <- colwise(cumprod)(DT_WORLD_IDX_Dum) * 100
DT_WORLD_IDX_Dum$TRD_DT <- as_date(DT_WORLD_IDX$TRD_DT)

write.csv(DT_WORLD_IDX_Dum, file = 'DT_WORLD_IDX_Dum.csv')

## 수익률 지수 그래프
WORLD_IDX_1 <- ggplot(data = DT_WORLD_IDX_Dum, aes(x = TRD_DT)) + 
                  geom_line(aes(y = MSCI_ACWI, color = 'MSCI ACWI'), lwd = 2) + 
                  geom_line(aes(y = MSCI_EAFE, color = 'MSCI EAFE'), lwd = 1.5) +
                  geom_line(aes(y = SP500, color = 'SP500'), lwd = 1.5) +
                  geom_line(aes(y = 심천종합, color = '심천종합'), lwd = 1.5) +
                  geom_line(aes(y = 상해종합, color = '상해종합'), lwd = 1.5) +
                  geom_line(aes(y = 토픽스, color = '토픽스'), lwd = 1.5) +
                  geom_line(aes(y = 항셍, color = '항셍'), lwd = 1.5) +
                  geom_line(aes(y = STOXX600, color = 'STOXX600'), lwd = 1.5) +
                  geom_hline(yintercept = 100, lty = 2, color = 'red') + 
                  xlab('일자') + ylab('전년대비(전년:100)') + ggtitle('세계 증시 누적 수익률') +  
                  scale_x_date(date_breaks = '3 months',date_labels = '%y년 %B %d일',
                               limits= c(DT_WORLD_IDX_Dum$TRD_DT[1], 
                                         DT_WORLD_IDX_Dum$TRD_DT[nrow(DT_WORLD_IDX_Dum)+5]),
                               expand = c(0,0)) + CmTheme 

ggsave(filename = 'WORLD_IDX_1.png', plot 
       = WORLD_IDX_1, width = 5, height = 3, units = "in")


## 경제 지표

SQL_Manufac_R <- "select aa.trd_dt, aa.Manufac_R, bb. Mining_R, 
                  cc.Service_R, dd.Const_mn_R, ee.Retail_R, ff.Cap_Inv_R
from
(
  -- 전산업생산지수(원지수)
  select a.trd_dt, round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) Manufac_R
  from FNE_ECO_DATA a 
  where ECO_CD = '04.47.001.001' and term = 'M'
  and a.trd_dt between '20150531' and '20170531'  
  order by a.trd_dt desc
) aa
  left join
  (
  -- 광공업 생산지수
  select b.trd_dt, round(((b.amount - 
  lag(b.amount, 12, b.amount) 
  over (order by b.trd_dt)) /
  lag(b.amount, 12, b.amount) 
  over (order by b.trd_dt)) * 100, 2) Mining_R 
  from FNE_ECO_DATA b 
  where b.ECO_CD = '04.05.004.001' and
  b.trd_dt between '20150531' and '20170531'
  order by b.trd_dt desc
  ) bb
  on aa.trd_dt = bb.trd_dt
  left join
  (
  -- 서비스업 생산지수
  select c.trd_dt,  round(((c.amount - 
  lag(c.amount, 12, c.amount) 
  over (order by c.trd_dt)) /
  lag(c.amount, 12, c.amount) 
  over (order by c.trd_dt)) * 100, 2) Service_R
  from FNE_ECO_DATA c 
  where c.ECO_CD = '04.05.004.002' and
  c.trd_dt between '20150531' and '20170531'
  order by c.trd_dt desc
  ) cc
  on aa.trd_dt = cc.trd_dt
  left join
  (
  -- 건설기성액 
  select d.trd_dt, round(((d.amount - 
  lag(d.amount, 12, d.amount) 
  over (order by d.trd_dt)) /
  lag(d.amount, 12, d.amount) 
  over (order by d.trd_dt)) * 100, 2) Const_mn_R
  from FNE_ECO_DATA d 
  where d.ECO_CD = '04.05.004.003' and
  d.trd_dt between '20150531' and '20170531'
  order by d.trd_dt desc
  ) dd
  on aa.trd_dt = dd.trd_dt
  left join 
  (
  -- 소매판매
  select e.trd_dt, round(((e.amount - 
  lag(e.amount, 12, e.amount) 
  over (order by e.trd_dt)) /
  lag(e.amount, 12, e.amount) 
  over (order by e.trd_dt)) * 100, 2) Retail_R
  from FNE_ECO_DATA e 
  where e.ECO_CD = '04.05.004.004' and
  e.trd_dt between '20150531' and '20170531'
  order by e.trd_dt desc
  ) ee
  on aa.trd_dt = ee.trd_dt
  left join
  (
  -- 설비투자
  select f.trd_dt, round(((f.amount - 
  lag(f.amount, 12, f.amount) 
  over (order by f.trd_dt)) /
  lag(f.amount, 12, f.amount) 
  over (order by f.trd_dt)) * 100, 2) Cap_Inv_R
  from FNE_ECO_DATA f 
  where f.ECO_CD = '04.58.001.001' and f.term = 'M'
  AND f.trd_dt between '20150531' and '20170531'
  order by f.trd_dt desc
  ) ff
  on aa.trd_dt = ff.trd_dt
  order by aa.trd_dt desc"
  
SQL_Manufac_R <- gsub(pattern = "between '20150531' and '20170531'", replacement = econ_dty, x = SQL_Manufac_R)
Dt_Manufac_R <- sqlQuery(ch1, SQL_Manufac_R)
Dt_Manufac_R$TRD_DT <- ymd(Dt_Manufac_R$TRD_DT)
write.csv(x = Dt_Manufac_R, file = 'Dt_Manufac_R.csv')

## 무역
SQL_Trading_1 <- "
          select aa.trd_dt, aa.amount 대미수출, bb.amount 대일수출, 
          cc.amount 대중수출, dd.amount - (aa.amount + bb.amount + cc.amount) 그외
          from
          (
            -- 미국
            select a.trd_dt, a.amount
            from FNE_ECO_DATA a 
            where a.ECO_CD = '13.01.005.002.020' and a.term = 'M'
            AND a.trd_dt between '20150331' and '20170331'
          ) aa
            inner join
            (
            -- 일본
            select b.trd_dt, b.amount
            from FNE_ECO_DATA b 
            where b.ECO_CD = '13.01.005.002.021' and b.term = 'M'
            AND b.trd_dt between '20150331' and '20170331'
            ) bb
            on aa.trd_dt = bb.trd_dt
            inner join 
            (
            -- 중국    
            select c.trd_dt, c.amount
            from FNE_ECO_DATA c 
            where c.ECO_CD = '13.01.005.002.002' and c.term = 'M'
            AND c.trd_dt between '20150331' and '20170331'
            ) cc
            on aa.trd_dt = cc.trd_dt
            inner join 
            (
            -- 합계
            select d.trd_dt, d.amount
            from FNE_ECO_DATA d 
            where d.ECO_CD = '13.01.005.001.001' and d.term = 'M'
            AND d.trd_dt between '20150331' and '20170331'
            
            ) dd 
            on aa.trd_dt = dd.trd_dt;
  "
SQL_Trading_1 <- gsub(pattern = "between '20150331' and '20170331'", replacement = econ_dty, x = SQL_Trading_1)
Dt_Trading <- sqlQuery(ch1, SQL_Trading_1)
Dt_Trading$TRD_DT <- ymd(Dt_Trading$TRD_DT)
write.csv(x = Dt_Trading, file = 'Dt_Trading.csv')  

SQL_Trading_2 <- "
select aa.trd_dt 날짜, aa.amount 수출물량지수, b.amount 수출금액지수, c.amount 수입물량지수, d.amount 수입금액지수 
from
(
  select a.trd_dt, a.amount
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.001.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531'
) aa 
  left join 
  (
  select a.trd_dt, a.amount
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.002.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531'
  ) b
  on aa.trd_dt = b.trd_dt
  left join 
  (
  select a.trd_dt, a.amount
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.004.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531'
  ) c
  on aa.trd_dt = c.trd_dt
  left join 
  (
  select a.trd_dt, a.amount
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.003.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531'
  ) d
  on aa.trd_dt = d.trd_dt
  order by aa.trd_dt desc
  "
SQL_Trading_2 <- gsub(pattern = "between '20150531' and '20170531'", replacement = econ_dty, x = SQL_Trading_2)
Dt_Trading_2 <- sqlQuery(ch1, SQL_Trading_2)
Dt_Trading_2$날짜 <- ymd(Dt_Trading_2$날짜)
write.csv(x = Dt_Trading_2, file = 'Dt_Trading_2.csv')  
  
SQL_Trading_1_R <- "
select aa.trd_dt, aa.To_Am_R 대미수출, bb.To_Jp_R 대일수출, 
cc.To_Ch_R 대중수출, 
round((dd.amount - (aa.amount + bb.amount + cc.amount) -
lag(dd.amount - (aa.amount + bb.amount + cc.amount), 12, 
dd.amount - (aa.amount + bb.amount + cc.amount))
over(order by dd.trd_dt)) /  
lag(dd.amount - (aa.amount + bb.amount + cc.amount), 12, 
dd.amount - (aa.amount + bb.amount + cc.amount)) 
over(order by dd.trd_dt) * 100, 2)
from
(
  -- 미국
  select a.trd_dt, a.amount, round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) To_Am_R
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.005.002.020' and a.term = 'M'
  AND a.trd_dt between '20150331' and '20170331'
) aa
  inner join
  (
  -- 일본
  select b.trd_dt, b.amount, round(((b.amount - 
  lag(b.amount, 12, b.amount) 
  over (order by b.trd_dt)) /
  lag(b.amount, 12, b.amount) 
  over (order by b.trd_dt)) * 100, 2) To_Jp_R
  from FNE_ECO_DATA b 
  where b.ECO_CD = '13.01.005.002.021' and b.term = 'M'
  AND b.trd_dt between '20150331' and '20170331'
  ) bb
  on aa.trd_dt = bb.trd_dt
  inner join 
  (
  -- 중국    
  select c.trd_dt, c.amount, round(((c.amount - 
  lag(c.amount, 12, c.amount) 
  over (order by c.trd_dt)) /
  lag(c.amount, 12, c.amount) 
  over (order by c.trd_dt)) * 100, 2) To_Ch_R
  from FNE_ECO_DATA c 
  where c.ECO_CD = '13.01.005.002.002' and c.term = 'M'
  AND c.trd_dt between '20150331' and '20170331'
  ) cc
  on aa.trd_dt = cc.trd_dt
  inner join 
  (
  -- 합계
  select d.trd_dt, d.amount
  from FNE_ECO_DATA d 
  where d.ECO_CD = '13.01.005.001.001' and d.term = 'M'
  AND d.trd_dt between '20150331' and '20170331'
  ) dd 
  on aa.trd_dt = dd.trd_dt;
  "
SQL_Trading_1_R <- gsub(pattern = "between '20150331' and '20170331'", replacement = econ_dty, x = SQL_Trading_1_R)
Dt_Trading_1_R <- sqlQuery(ch1, SQL_Trading_1_R)
Dt_Trading_1_R$TRD_DT <- ymd(Dt_Trading_1_R$TRD_DT)
write.csv(x = Dt_Trading_1_R , file = 'Dt_Trading_1_R .csv')  

SQL_Trading_2_R <- "
select aa.trd_dt 날짜,  aa.수출물량지수, 
       b.수출금액지수, c.수입물량지수, 
d.수입금액지수 
from
(
  select a.trd_dt, round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 수출물량지수
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.001.001' and a.term = 'M'
  AND a.trd_dt between '20150331' and '20170331'
) aa 
  left join 
  (
  select a.trd_dt,   round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 수출금액지수
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.002.001' and a.term = 'M'
  AND a.trd_dt between '20150331' and '20170331'
  ) b
  on aa.trd_dt = b.trd_dt
  left join 
  (
  select a.trd_dt,   round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 수입물량지수
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.004.001' and a.term = 'M'
  AND a.trd_dt between '20150331' and '20170331'
  ) c
  on aa.trd_dt = c.trd_dt
  left join 
  (
  select a.trd_dt,   round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 수입금액지수
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.013.003.001' and a.term = 'M'
  AND a.trd_dt between '20150331' and '20170331'
  ) d
  on aa.trd_dt = d.trd_dt
  order by aa.trd_dt desc;
  "
SQL_Trading_2_R <- gsub(pattern = "between '20150331' and '20170331'", replacement = econ_dty, x = SQL_Trading_2_R)
Dt_Trading_2_R <- sqlQuery(ch1, SQL_Trading_2_R)
Dt_Trading_2_R$날짜 <- ymd(Dt_Trading_2_R$날짜)
write.csv(x = Dt_Trading_2_R , file = 'Dt_Trading_2_R .csv')

SQL_Trading_3 <- "
select a.trd_dt, a.국가별수출액, a.국가별수출액_전년동기, b.국가별수입액, b.국가별수입액_전년동기, 
a.국가별수출액 - b.국가별수입액 무역수지,  round((((a.국가별수출액 - b.국가별수입액) -    
lag(a.국가별수출액 - b.국가별수입액, 12, a.국가별수출액 - b.국가별수입액) 
over (order by a.trd_dt)) /
lag(a.국가별수출액 - b.국가별수입액, 12, a.국가별수출액 - b.국가별수입액) 
over (order by a.trd_dt)) * 100, 2) 무역수지_전년동기
from
(
  select a.trd_dt, a.amount 국가별수출액, round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 국가별수출액_전년동기
  
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.005.001.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531'
) a , (select a.trd_dt, a.amount 국가별수입액,
  round(((a.amount - 
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) /
  lag(a.amount, 12, a.amount) 
  over (order by a.trd_dt)) * 100, 2) 국가별수입액_전년동기
  from FNE_ECO_DATA a 
  where a.ECO_CD = '13.01.006.001.001' and a.term = 'M'
  AND a.trd_dt between '20150531' and '20170531') b 
  where a.trd_dt = b.trd_dt
  order by a.trd_dt desc;
  "
  SQL_Trading_3 <- gsub(pattern = "between '20150531' and '20170531'", replacement = econ_dty, x = SQL_Trading_3)
  Dt_Trading_3 <- sqlQuery(ch1, SQL_Trading_3)
  Dt_Trading_3$TRD_DT <- ymd(Dt_Trading_3$TRD_DT)
  write.csv(x = Dt_Trading_3 , file = 'Dt_Trading_3 .csv')  



## 시장별 순매수
# dcast 필
# 코스피

"""

-- 0:기관계
-- 8:개인
-- 11:외국인계

"""
SQL_KP_NBUY <- "
select cc.Yrm, cc.CD_NM, sum(cc.순매수) 순매수 
from
(
  SELECT A.trd_dt, sum(A.buy_amt - A.sell_amt) 순매수, B.CD_NM, concat(substr(A.trd_dt,1,4), substr(A.trd_dt,5,2)) Yrm
  FROM FNS_J_INVEST A, (SELECT * FROM FNC_GRP_CD_DETAIL
  WHERE GRP_CD = 'TY01' and 
  cd in (0,8,11)) B, (
  SELECT GICODE FROM FNS_J_MAST
  WHERE MKT_GB = 1) D
  WHERE A.INVEST_GB = B.CD AND A.GICODE IN D.GICODE AND  
  A.TRD_DT between '20150331' and '20170331'
  group by A.trd_dt, B.CD_NM 
) cc
  group by cc.CD_NM, Yrm
  order by Yrm
  "
SQL_KP_NBUY <- gsub(pattern = "between '20150331' and '20170331'", replacement = KOS_dty, x = SQL_KP_NBUY)
DT_KP_NBUY <- sqlQuery(ch1, SQL_KP_NBUY)
DT_KP_NBUY$YRM <- as.character(DT_KP_NBUY$YRM)
DT_KP_NBUY$YRM <- as.yearmon(DT_KP_NBUY$YRM,'%Y%m')
write.csv(x = DT_KP_NBUY, file = 'DT_KP_NBUY.csv')  

DT_KP_NBUY_XLS <- dcast(DT_KP_NBUY, YRM ~ CD_NM, value.var = "순매수")
write.csv(x = DT_KP_NBUY_XLS, file = 'DT_KP_NBUY_XLS.csv')  

  
# 코스닥
SQL_KD_NBUY <- "
              select cc.Yrm, cc.CD_NM, sum(cc.순매수) 순매수 
              from
              (
              SELECT A.trd_dt, sum(A.buy_amt - A.sell_amt) 순매수, B.CD_NM, concat(substr(A.trd_dt,1,4), substr(A.trd_dt,5,2)) Yrm
              FROM FNS_J_INVEST A, (SELECT * FROM FNC_GRP_CD_DETAIL
              WHERE GRP_CD = 'TY01' and 
              cd in (0,8,11)) B, (
              SELECT GICODE FROM FNS_J_MAST
              WHERE MKT_GB = 2) D
              WHERE A.INVEST_GB = B.CD AND A.GICODE IN D.GICODE AND  
              A.TRD_DT between '20150331' and '20170331'
              group by A.trd_dt, B.CD_NM 
              ) cc
              group by cc.CD_NM, Yrm
              order by Yrm
"

SQL_KD_NBUY <- gsub(pattern = "between '20150331' and '20170331'", replacement = KOS_dty, x = SQL_KD_NBUY)
DT_KD_NBUY <- sqlQuery(ch1, SQL_KD_NBUY)
DT_KD_NBUY$YRM <- as.character(DT_KD_NBUY$YRM)
DT_KD_NBUY$YRM <- as.yearmon(DT_KD_NBUY$YRM,'%Y%m')
write.csv(x = DT_KD_NBUY, file = 'DT_KD_NBUY.csv')

DT_KD_NBUY_XLS <- dcast(DT_KD_NBUY, YRM ~ CD_NM, value.var = "순매수")
write.csv(x = DT_KD_NBUY_XLS, file = 'DT_KD_NBUY_XLS.csv')  

DT_KP_NBUY$Cat <- "KOSPI"
DT_KD_NBUY$Cat <- "KOSDAQ"
DT_KO_NBUY <- rbind(DT_KP_NBUY,DT_KD_NBUY)
DT_KO_NBUY$Cat <- as.factor(DT_KO_NBUY$Cat)

DT_KO_M <- DT_KO_NBUY %>%
           ddply(.(YRM,Cat), summarize,
                 sum = sum(순매수)) %>%
           dcast(YRM~Cat, value.var = "sum") %>%
           arrange(desc(YRM))

write.csv(x = DT_KO_M, file = 'DT_KO_M.csv')
DT_KO_B <-DT_KO_NBUY %>%
            ddply(.(YRM,CD_NM), summarize,
                  sum = sum(순매수)) %>%
            dcast(YRM~CD_NM, value.var = "sum") %>%
            arrange(desc(YRM))
write.csv(x = DT_KO_B, file = 'DT_KO_B.csv')

KP_NBY <- DT_KP_NBUY
KP_NBY$CD_NM <- as.factor(KP_NBY$CD_NM)

KP_NBY_1 <- ggplot(KP_NBY, aes(x = as.factor(YRM), fill = CD_NM)) + 
  geom_bar(aes(y = 순매수/10^7), stat = 'identity', position = 'dodge') + 
  scale_y_continuous(limits = c(min(KP_NBY$순매수/10^7) - 5 , 
                                max(KP_NBY$순매수/10^7)+ 5)) +
  xlab('일자') + ylab('순매수') + ggtitle('코스피 기관별 순매수(단위: 조)') + 
  theme(plot.title = element_text(family = 'RixMGOB', size = 12),
        axis.title.x = element_text(family = 'RixMGOB', size = 10),
        axis.title.y = element_text(family = 'RixMGOB', size = 10),
        axis.text.x = element_text(family = 'RixMGOL', size = 8,
                                   hjust = 0.5, vjust = 0.5, color = 'black', angle = 30),
        axis.text.y = element_text(family = 'RixMGOL', size = 10, color = 'black')) + 
  CmTheme 

write.csv(x = KP_NBY, file = 'KP_NBY.csv')  
ggsave(filename = 'KP_NBY_1.png', plot 
       = KP_NBY_1, width = 5, height = 3, units = "in")


## 코스닥

KD_NBY <- DT_KD_NBUY
KD_NBY$CD_NM <- as.factor(KD_NBY$CD_NM)


KD_NBY_1 <- ggplot(KD_NBY, aes(x = as.factor(YRM), fill = CD_NM)) + 
  geom_bar(aes(y = 순매수/10^7), stat = 'identity', position = 'dodge') + 
  scale_y_continuous(limits = c(min(KD_NBY$순매수/10^7) - 5 , 
                                max(KD_NBY$순매수/10^7)+ 5)) +
  xlab('일자') + ylab('순매수') + ggtitle('코스닥 기관별 순매수(단위: 조)') + 
  CmTheme 

write.csv(x = KD_NBY, file = 'KD_NBY.csv')  
ggsave(filename = 'KD_NBY_1.png', plot 
       = KD_NBY_1, width = 5, height = 3, units = "in")

## 소비자 심리지수
SQL_Consuming <- "select a.trd_dt 날짜, a.amount 소비자심리지수
from FNE_ECO_DATA a 
where a.ECO_CD = '04.13.018' and a.term = 'M' and 
a.trd_dt between '20150331' and '20170331'
" 
SQL_Consuming <- gsub(pattern = "between '20150331' and '20170331'", replacement = dty, x = SQL_Consuming)
Dt_Consuming <- sqlQuery(ch1, SQL_Consuming)
Dt_Consuming$날짜 <- ymd(Dt_Consuming$날짜)
write.csv(x = Dt_Consuming, file = 'Dt_Consuming.csv')  

SQL_Consuming_Diff <- "
select a.trd_dt 날짜, a.amount - lag(a.amount, 1, a.amount) 
                                over (order by a.trd_dt) 전월대비변화
from FNE_ECO_DATA a 
where a.ECO_CD = '04.13.018' and a.term = 'M' and 
a.trd_dt between '20150331' and '20170331'
"
SQL_Consuming_Diff <- gsub(pattern = "between '20150331' and '20170331'", replacement = dty, x = SQL_Consuming_Diff)
Dt_Consuming_Diff <- sqlQuery(ch1, SQL_Consuming_Diff)
Dt_Consuming_Diff$날짜 <- ymd(Dt_Consuming_Diff$날짜)
write.csv(x = Dt_Consuming_Diff, file = 'Dt_Consuming.csv')  

Cons_pref_diff_1 <- ggplot(data = Dt_Consuming_Diff, aes(x = 날짜)) +
                          geom_bar(aes(y = 전월대비변화), stat = "identity", fill = 'red') + 
                          scale_x_date(date_breaks = '1 months',date_labels = '%y년 %B',
                                       limits = c(Dt_Consuming_Diff$날짜[1], Dt_Consuming_Diff$날짜[nrow(Dt_Consuming_Diff)]),
                                       expand = c(0,5)) + 
                          xlab('일자') + CmTheme 
  
ggsave(filename = 'Cons_pref_diff_1.png', plot 
       = Cons_pref_diff_1, width = 5, height = 3, units = "in")

Cons_pref <- Dt_Consuming

Cons_pref_1 <- ggplot(Cons_pref, aes(x = 날짜)) +
                    geom_line(aes(y = 소비자심리지수), color = 'cornflowerblue', lwd = 2) + 
                    scale_x_date(date_breaks = '3 months',date_labels = '%y년 %B',
                    limits= c(Cons_pref$날짜[1], Cons_pref$날짜[nrow(Cons_pref)]),
                    expand = c(0,5)) +
                    xlab('일자') + ylab('지수') +  
                    CmTheme 

write.csv(x = Cons_pref, file = 'Cons_pref.csv')  
ggsave(filename = 'Cons_pref_1.png', plot 
       = Cons_pref_1, width = 5, height = 3, units = "in")
  
## 환율

SQL_CRCY <- "
-- 달러
select aab.trd_dt, aa.amount 달러인덱스, bb.amount 원달러
from
(
  select trd_dt
  from fnc_calendar
  where trd_dt between '20150331' and '20170331'
) aab
  left join
  (
  -- 달러인덱스
  select d.trd_dt, d.amount
  from FNE_ECO_DATA d 
  where d.ECO_CD = '14.09.006.001' and TERM = 'D'
  AND d.trd_dt between '20150331' and '20170331'
  ) aa
  on aab.trd_dt = aa.trd_dt
  left join
  (
  --원달러(외국환종가)
  select d.trd_dt, d.amount
  from FNE_ECO_DATA d 
  where d.ECO_CD = '13.03.007.001.005' and TERM = 'D'
  AND d.trd_dt between '20150331' and '20170331'
  ) bb
  on aab.trd_dt = bb.trd_dt
  order by aab.trd_dt;
"

SQL_CRCY <- gsub(pattern = "between '20150331' and '20170331'", replacement = dty, 
                  x = SQL_CRCY)
  
DT_CRCY <- sqlQuery(ch1, SQL_CRCY)
DT_CRCY <- na.locf(DT_CRCY)
DT_CRCY$TRD_DT <- ymd(DT_CRCY$TRD_DT)


write.csv(x = DT_CRCY, file = 'DT_CRCY.csv')
CRCY_TABLE_1 <- Table_Rate(DT_CRCY, date = d_y)
write.csv(x = CRCY_TABLE_1, file = 'CRCY_TABLE_1.csv')

KRWD_1 <- ggplot(DT_CRCY, aes(x = TRD_DT)) +
          geom_line(aes(y = 원달러, color = "원달러"), lwd = 1) + 
          geom_line(aes(y = 달러인덱스*10, color = "달러인덱스"), lwd = 1) +
          scale_color_manual(values = c("원달러" = 'brown3',
                                        "달러인덱스" = 'cornflowerblue')) +
          scale_y_continuous(sec.axis = sec_axis(trans = ~. / 10, name = '지수')) +
          scale_x_date(date_breaks = '2 months',date_labels = '%y년 %B',
                       limits= c(DT_CRCY$TRD_DT[1], 
                                 DT_CRCY$TRD_DT[length(DT_CRCY$TRD_DT)]),
                       expand = c(0,5)) +
          xlab('일자') + ylab('원/달러 ') + 
          CmTheme 

ggsave(filename = 'KRWD_1.png', plot 
       = KRWD_1, width = 5, height = 3, units = "in")

## 펀드

Fund_Data_Mani <- function(Query){
  Data <- sqlQuery(ch, Query)
  Data <- dcast(Data, TRD_DT ~ PEER_NM)
  Data <- na.locf(Data)
  Data$TRD_DT <- ymd(Data$TRD_DT)
  Data <- Data[c(1:(ncol(Data)-1))]
  Data  
}

SQL_Fund_Dome <- "
select a.trd_dt, b.peer_nm, b.set_amt
from
(
  select distinct trd_dt
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA
  where trd_dt between '20170312' and  '20170512' and
  bond_cd in (select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:') and 
  INST_CD = 'C' 
  order by trd_dt
) a 
  left join
  (
  select a.trd_dt, c.peer_nm, a.set_amt 
  from F2_CDT021 a, 
  (
  select PEER_CD, PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('국내주식형', '국내혼합형', '국내채권형')
  ) c
  where a.peer_cd in 
  (
  select PEER_CD
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('국내주식형', '국내혼합형', '국내채권형')
  )
  and a.co_cd = '000' and 
  a.peer_cd = c.peer_cd and a.trd_dt between '20170312' and  '20170512'
  ) b
  on a.trd_dt = b.trd_dt
  order by a.trd_dt
  "
  
SQL_Fund_Dome <- gsub(pattern = "between '20170312' and  '20170512'", replacement = dty, x = SQL_Fund_Dome)

DT_Fund_Dome <- Fund_Data_Mani(SQL_Fund_Dome)
write.csv(x = DT_Fund_Dome, file = 'DT_Fund_Dome.csv')
Table_Fund_Dome <- Table_Rate(DT_Fund_Dome, date = as_date('2017-07-31'))
Table_Fund_Dome[,1] <- Table_Fund_Dome[,1] / 10^8
write.csv(x = Table_Fund_Dome, file = 'Table_Fund_Dome.csv')
  
# 부동산 대외 투자형

SQL_Fund_Alter <- "
select a.trd_dt, b.peer_nm, b.set_amt
from
(
  select distinct trd_dt
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA
  where trd_dt between '20170312' and  '20170512' and
  bond_cd in (select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:') and 
  INST_CD = 'C' 
  order by trd_dt
) a 
  left join
  (
  select a.trd_dt, c.peer_nm, a.set_amt 
  from F2_CDT021 a, 
  (
  select m.PEER_CD, m.PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('국내부동산', '해외부동산', '대안투자형', '기타형')
  ) c
  where a.peer_cd in 
  (
  select PEER_CD
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('국내부동산', '해외부동산', '대안투자형', '기타형')
  )
  and a.co_cd = '000' and 
  a.peer_cd = c.peer_cd and a.trd_dt between '20170312' and  '20170512'
  ) b
  on a.trd_dt = b.trd_dt
  order by a.trd_dt;
"
SQL_Fund_Alter <- gsub(pattern = "between '20170312' and  '20170512'", replacement = dty, x = SQL_Fund_Alter)

DT_Fund_Alter <- Fund_Data_Mani(SQL_Fund_Alter)
write.csv(x = DT_Fund_Alter, file = 'DT_Fund_Alter.csv')
Table_Fund_Alter <- Table_Rate(DT_Fund_Alter, date = as_date('2017-07-31'))
Table_Fund_Alter[,1] <- Table_Fund_Alter[,1] / 10^8
write.csv(x = Table_Fund_Alter, file = 'Table_Fund_Alter.csv')

# 일반 해외형
SQL_Fund_Fore <- "
select a.trd_dt, b.peer_nm, b.set_amt
from
(
  select distinct trd_dt
  from FNB_BOND_MATRIX@D_FNDB2_UFNGDBA
  where trd_dt between '20170312' and '20170512' and
  bond_cd in (select distinct substr(orig_cd, 1, 7)
  from FF_SIM010 
  where substr(FULL_NM,1,5) = '통안증권:') and 
  INST_CD = 'C' 
  order by trd_dt
) a 
  left join
  (
  select a.trd_dt, c.peer_nm, a.set_amt 
  from F2_CDT021 a, 
  (
  select m.PEER_CD, m.PEER_NM 
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('해외주식형', '해외혼합형', '해외채권형')
  ) c
  where a.peer_cd in 
  (
  select PEER_CD
  from F2_SGM010 m
  where m.peer_gb = 'G' 
  and m.offer_gb = 'A'
  and m.class_gb  = 'L'
  and m.peer_nm in ('해외주식형', '해외혼합형', '해외채권형')
  )
  and a.co_cd = '000' and 
  a.peer_cd = c.peer_cd and a.trd_dt between '20170312' and '20170512'
  ) b
  on a.trd_dt = b.trd_dt
  order by a.trd_dt;
"

SQL_Fund_Fore <- gsub(pattern = "between '20170312' and '20170512'", replacement = dty, x = SQL_Fund_Fore)
DT_Fund_Fore <- Fund_Data_Mani(SQL_Fund_Fore)
write.csv(x = DT_Fund_Fore, file = 'DT_Fund_Fore.csv')
Table_Fund_Fore <- Table_Rate(DT_Fund_Fore, date = as_date('2017-07-31'))
Table_Fund_Fore[,1] <- Table_Fund_Fore[,1] / 10^8
write.csv(x = Table_Fund_Fore, file = 'Table_Fund_Fore.csv')

## MMF 그래프

SQL_MMF_DT <- "select b.trd_dt Dt, b.set_amt 국내채권형, d.set_amt MMF  
              from 
              (
                select a.trd_dt, a.set_amt
                from F2_CDT021 a
                where a.peer_cd in 
                (
                select m.peer_cd
                from F2_SGM010 m
                where m.peer_gb = 'G' 
                and m.offer_gb = 'A'
                and m.class_gb  = 'L'
                and m.peer_nm in ('국내채권형')
                ) 
                and a.co_cd = '000' and 
                a.trd_dt between 20160531 and 20170531
              ) b
                inner join
                (
                select c.trd_dt, c.set_amt
                from F2_CDT021 c
                where c.peer_cd in 
                (
                select m.peer_cd
                from F2_SGM010 m
                where m.peer_gb = 'G' 
                and m.offer_gb = 'A'
                and m.class_gb  = 'L'
                and m.peer_nm in ('MMF')
                ) 
                and c.co_cd = '000' and c.trd_dt between 20160531 and 20170531
                ) d
                on b.trd_dt = d.trd_dt"

SQL_MMF_DT <- gsub(pattern = "between 20160531 and 20170531", replacement = dty, x = SQL_MMF_DT)
DT_Fund_MMF <- sqlQuery(ch, SQL_MMF_DT)
DT_Fund_MMF$DT <- ymd(DT_Fund_MMF$DT)

DT_Fund_MMF_Date <- DT_Fund_MMF$DT[2:nrow(DT_Fund_MMF)] 
DT_Fund_MMF_Bond <- diff(DT_Fund_MMF$국내채권형)
DT_Fund_MMF_MMF <- diff(DT_Fund_MMF$MMF)

DT_Fund_MMF_N_BOND <- data.frame(DT_Fund_MMF_Date,
                                 DT_Fund_MMF_Bond / 10^8,
                                 DT_Fund_MMF_MMF / 10^8)

colnames(DT_Fund_MMF_N_BOND) <- c("DT", "Bond", "MMF")

New_change_MMF_BOND <- melt(DT_Fund_MMF_N_BOND, id.vars = 'DT')
New_change_MMF_BOND$variable <- as.factor(New_change_MMF_BOND$variable)

New_change_MMF_BOND_xlsx <- dcast(New_change_MMF_BOND,DT~variable, value.var = 'value')
write.csv(New_change_MMF_BOND_xlsx, file = 'New_change_MMF_BOND_xlsx.csv')

New_change_MMF_BOND_1 <- ggplot(data = New_change_MMF_BOND,
                                aes(x = DT, y = value, fill = variable, group = variable)) +
  geom_area(stat = 'identity', position = 'stack') +
  geom_hline(yintercept = 0, color = 'black') + 
  scale_fill_manual(values = c("Bond" = "chartreuse1",
                               "MMF" = "chocolate1"),
                    labels = c("국내채권형", "MMF")) +
  xlab('일자') + ylab("수탁고(단위 억)") + 
  ggtitle('국내채권형, MMF 수탁고  순변화') +  
  scale_x_date(date_breaks = '2 month',date_labels = '%y년 %b월',
               limits= c(DT_Fund_MMF_N_BOND$DT[1], 
                         DT_Fund_MMF_N_BOND$DT[nrow(DT_Fund_MMF_N_BOND)])) + 
  CmTheme 

write.csv(x = New_change_MMF_BOND, file = 'New_change_MMF_BOND_1D.csv')  
ggsave(filename = 'DT_Fund_MMF_N_BOND_1.png', plot = New_change_MMF_BOND_1,
       width = 5, height = 3, units = "in")

Sys.setlocale(category = "LC_ALL", locale = "us")
