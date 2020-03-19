install.packages("dplyr")
library(readxl)
library(dplyr)

############# 유치원
#3-5거주인구
pop345 <- read_excel('data/kinder0. age345live_dong.xlsx')
pop345 <- pop345 %>% select('자치구','행정동',total) %>% rename(gu='자치구',dong='행정동')
write.csv(pop345,'data/kinder0. age345live_dong.csv')

#1. 수용률별 거주인구
y1 <- read_excel('data/kinder1. capa.xlsx')
y1 <- as.data.frame(y1)
y1
y1 <- y1 %>% select(gu, '설립', capa) %>% group_by(gu) %>% summarise(capacity=sum(capa))
gupop <- pop345 %>% group_by(gu) %>% summarise(gupop=sum(total)) #구별 3-5세 거주인구
capa <- left_join(y1, gupop, by='gu') %>% mutate(perc=capacity/gupop) %>% arrange(perc)
capa <- capa %>% mutate(perc_inv=1/perc)
capa_r <- left_join(pop345,capa) %>% select(gu,dong,total,perc_inv) %>% mutate(result=total*perc_inv) %>% arrange(desc(result))
write.csv(capa_r,'data/kinder1. capa_result.csv')

#2. 설립의무 미실행 지역
y2 <- read.csv('data/kinder2. dutynot.csv')
y2 <- as.data.frame(y2)
y2['e']<-c(1)
y2 <- y2 %>% select(dong,e)
y2 <- left_join(pop345,y2,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
y2$ee[is.na(y2$ee)] <- c(0)
write.csv(y2,'data/kinder2. dutynot_result.csv')

#3. 접근권역도 소외지역
y3 <- read.csv('data/kinder3. no400m.csv')
y3 <- as.data.frame(y3)
y3['e']<-c(1)
y3 <- y3 %>% select(dong,e)
y3 <- left_join(pop345,y3,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
y3$ee[is.na(y3$ee)] <- c(0)
write.csv(y3,'data/kinder3. no400m_result.csv')

#4. 접근권역도 소외지역
y4 <- read.csv('data/kinder4. completeno.csv')
y4 <- as.data.frame(y4)
y4['e']<-c(1)
y4 <- y4 %>% select(dong,e)
y4 <- left_join(pop345,y4,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
y4$ee[is.na(y4$ee)] <- c(0)
write.csv(y4,'data/kinder4. completeno.csv')


############## 어린이집
#0-5거주인구
pop05 <- read.csv('data/pre0. age05live_dong.csv')
str(pop05)
pop05 <- pop05 %>% select(gu,dong,total)
#1. 수용률별 거주인구
p1 <- read.csv('data/pre1. capa.csv')
p1 <- as.data.frame(p1)
p1
gupop05 <- pop05 %>% group_by(gu) %>% summarise(gupop=sum(total)) #구별 0-5세 거주인구
capa2 <- left_join(p1, gupop, by='gu') %>% mutate(perc=capacity/gupop) %>% arrange(perc)
capa2 <- capa2 %>% mutate(perc_inv=1/perc)
head(capa2)
capa_r2 <- left_join(pop05,capa2) %>% select(gu,dong,total,perc_inv) %>% mutate(result=total*perc_inv) %>% arrange(desc(result))
head(capa_r2)
write.csv(capa_r2,'data/pre1. capa_result.csv')

#2. 설립의무 미실행 지역
p2 <- read_excel('data/pre2. dutynot.xlsx')
p2 <- as.data.frame(p2)
p2['e']<-c(1)
p2 <- p2 %>% select(dong,e)
p2 <- left_join(pop05,p2,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
p2$ee[is.na(p2$ee)] <- c(0)
p2 %>% View()
write.csv(p2,'data/pre2. dutynot_result.csv')

#3. 접근권역도 소외지역
p3 <- read_excel('data/pre3. no400m.xlsx')
p3 <- as.data.frame(p3)
p3['e']<-c(1)
p3 <- p3 %>% select(dong,e)
p3 <- left_join(pop05,p3,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
p3$ee[is.na(p3$ee)] <- c(0)
head(p3)
write.csv(p3,'data/pre3. no400m_result.csv')

#4. 접근권역도 소외지역
p4 <- read_excel('data/pre4. completeno.xlsx')
p4 <- as.data.frame(p4)
p4['e']<-c(1)
p4 <- p4 %>% select(dong,e)
p4 <- left_join(pop05,p4,by=c('dong')) %>% mutate(ee = total*e) %>% select(gu,dong,ee) %>% arrange(desc(ee))
p4$ee[is.na(p4$ee)] <- c(0)
head(p4)
write.csv(p4,'data/pre4. completeno.csv')
