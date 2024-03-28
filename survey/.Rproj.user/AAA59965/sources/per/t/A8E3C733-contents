load("daejin_survey.RData")

library(readxl)
ori_survey <- read_excel("survey.xlsx")
View(ori_survey)

str(ori_survey)



#---학습자---
#sex,grade,major : 프로필
#q5 : 원격수업환경 
#q7  : 학습자 태도
#---강의---
#q9 - q12 : 교육자
#q13 - 15 : 교육컨텐츠
#q16 - 18 / q22_1 : 이론수업
#q19 - 21 / q22_2: 실습수업
#q23 - q25 : 과제
#q26 - q30 : 질문&피드백
#q31 - q34 : 시스템

#---만족도---
#q35 - q37 : 학습성취도
#q38 - q41 : 전반적만족도
#q42 - q44 : 장점및 개선점


#기본 범주값 : 1. 매우 그렇지 않다 2. 그렇지 않다 3.보통이다 4. 그렇다 5.매우 그렇다
#q7 : 1.매우 낮다 2.낮다 3.보통이다 4.높다 5.매우 높다
#q21 : 1.매우 불만족 2.불만족 3.보통 4.만족 5.매우 만족

survey <- ori_survey

library(dplyr)


survey$q5 <- ifelse(survey$q5 == "매우 그렇지  않다",1,
                      ifelse(survey$q5 == "그렇지  않다",2,
                             ifelse(survey$q5 == "보통이다",3,
                                    ifelse(survey$q5 == "그렇다",4,5))))
table(survey$q5)
table(ori_survey$q5)

table(survey$q7)
survey$q7 <- ifelse(survey$q7 == "매우 낮다",1,
                      ifelse(survey$q7 == "낮다",2,
                             ifelse(survey$q7 == "보통이다",3,
                                    ifelse(survey$q7 == "높다",4,5))))
table(survey$q7)


table(survey$q8)

survey$q8 <- ifelse(survey$q8 == "10회 이상",4,
                      ifelse(survey$q8 == "5-10회",3,
                             ifelse(survey$q8 == "2-5회",2,1)))
table(survey$q8)

str(survey$q8)


table(survey$q9_1)
survey$q9_1 <- ifelse(survey$q9_1 == "매우 그렇지 않다",1,
                      ifelse(survey$q9_1 == "그렇지 않다",2,
                             ifelse(survey$q9_1 == "보통이다",3,
                                    ifelse(survey$q9_1 == "그렇다",4,5))))
table(survey$q9_1)

table(survey$q9_2)
survey$q9_2 <- ifelse(survey$q9_2 == "매우 그렇지 않다",1,
                      ifelse(survey$q9_2 == "그렇지 않다",2,
                             ifelse(survey$q9_2 == "보통이다",3,
                                    ifelse(survey$q9_2 == "그렇다",4,5))))
table(survey$q9_2)

table(survey$q10_1)
survey$q10_1 <- ifelse(survey$q10_1 == "매우 그렇지 않다",1,
                      ifelse(survey$q10_1 == "그렇지 않다",2,
                             ifelse(survey$q10_1 == "보통이다",3,
                                    ifelse(survey$q10_1 == "그렇다",4,5))))
table(survey$q10_1)

table(survey$q10_2)
survey$q10_2 <- ifelse(survey$q10_2 == "매우 그렇지 않다",1,
                      ifelse(survey$q10_2 == "그렇지 않다",2,
                             ifelse(survey$q10_2 == "보통이다",3,
                                    ifelse(survey$q10_2 == "그렇다",4,5))))
table(survey$q10_2)

table(survey$q11_1)
survey$q11_1 <- ifelse(survey$q11_1 == "매우 그렇지 않다",1,
                      ifelse(survey$q11_1 == "그렇지 않다",2,
                             ifelse(survey$q11_1 == "보통이다",3,
                                    ifelse(survey$q11_1 == "그렇다",4,5))))
table(survey$q11_1)

table(survey$q11_2)
survey$q11_2 <- ifelse(survey$q11_2 == "매우 그렇지 않다",1,
                      ifelse(survey$q11_2 == "그렇지 않다",2,
                             ifelse(survey$q11_2 == "보통이다",3,
                                    ifelse(survey$q11_2 == "그렇다",4,5))))
table(survey$q11_2)

table(survey$q12_1)
survey$q12_1 <- ifelse(survey$q12_1 == "매우 그렇지 않다",1,
                      ifelse(survey$q12_1 == "그렇지 않다",2,
                             ifelse(survey$q12_1 == "보통이다",3,
                                    ifelse(survey$q12_1 == "그렇다",4,5))))
table(survey$q12_1)

table(survey$q12_2)
survey$q12_2 <- ifelse(survey$q12_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q12_2 == "그렇지 않다",2,
                              ifelse(survey$q12_2 == "보통이다",3,
                                     ifelse(survey$q12_2 == "그렇다",4,5))))
table(survey$q12_2)

table(survey$q13_1)
survey$q13_1 <- ifelse(survey$q13_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q13_1 == "그렇지 않다",2,
                              ifelse(survey$q13_1 == "보통이다",3,
                                     ifelse(survey$q13_1 == "그렇다",4,5))))
table(survey$q13_1)

table(survey$q13_2)
survey$q13_2 <- ifelse(survey$q13_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q13_2 == "그렇지 않다",2,
                              ifelse(survey$q13_2 == "보통이다",3,
                                     ifelse(survey$q13_2 == "그렇다",4,5))))
table(survey$q13_2)

table(survey$q14_1)
survey$q14_1 <- ifelse(survey$q14_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q14_1 == "그렇지 않다",2,
                              ifelse(survey$q14_1 == "보통이다",3,
                                     ifelse(survey$q14_1 == "그렇다",4,5))))
table(survey$q14_1)

table(survey$q14_2)
survey$q14_2 <- ifelse(survey$q14_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q14_2 == "그렇지 않다",2,
                              ifelse(survey$q14_2 == "보통이다",3,
                                     ifelse(survey$q14_2 == "그렇다",4,5))))
table(survey$q14_2)

table(survey$q15_1)
survey$q15_1 <- ifelse(survey$q15_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q15_1 == "그렇지 않다",2,
                              ifelse(survey$q15_1 == "보통이다",3,
                                     ifelse(survey$q15_1 == "그렇다",4,5))))
table(survey$q15_1)

survey$q15_1 <- survey$q15_1
mean(survey$q15_1)

table(survey$q15_2)
survey$q15_2 <- ifelse(survey$q15_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q15_2 == "그렇지 않다",2,
                              ifelse(survey$q15_2 == "보통이다",3,
                                     ifelse(survey$q15_2 == "그렇다",4,5))))
table(survey$q15_2)

table(survey$q16_1)
survey$q16_1 <- ifelse(survey$q16_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q16_1 == "그렇지 않다",2,
                              ifelse(survey$q16_1 == "보통이다",3,
                                     ifelse(survey$q16_1 == "그렇다",4,5))))
table(survey$q16_1)

table(survey$q16_2)
survey$q16_2 <- ifelse(survey$q16_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q16_2 == "그렇지 않다",2,
                              ifelse(survey$q16_2 == "보통이다",3,
                                     ifelse(survey$q16_2 == "그렇다",4,5))))
table(survey$q16_2)

table(survey$q17_1)
survey$q17_1 <- ifelse(survey$q17_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q17_1 == "그렇지 않다",2,
                              ifelse(survey$q17_1 == "보통이다",3,
                                     ifelse(survey$q17_1 == "그렇다",4,5))))
table(survey$q17_1)

table(survey$q17_2)
survey$q17_2 <- ifelse(survey$q17_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q17_2 == "그렇지 않다",2,
                              ifelse(survey$q17_2 == "보통이다",3,
                                     ifelse(survey$q17_2 == "그렇다",4,5))))
table(survey$q17_2)

table(survey$q18_1)
survey$q18_1 <- ifelse(survey$q18_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q18_1 == "그렇지 않다",2,
                              ifelse(survey$q18_1 == "보통이다",3,
                                     ifelse(survey$q18_1 == "그렇다",4,5))))
table(survey$q18_1)

table(survey$q18_2)
survey$q18_2 <- ifelse(survey$q18_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q18_2 == "그렇지 않다",2,
                              ifelse(survey$q18_2 == "보통이다",3,
                                     ifelse(survey$q18_2 == "그렇다",4,5))))
table(survey$q18_2)


table(survey$q19)
survey$q19 <- ifelse(survey$q19 == "매우 그렇지 않다",1,
                       ifelse(survey$q19 == "그렇지 않다",2,
                              ifelse(survey$q19 == "보통이다",3,
                                     ifelse(survey$q19 == "그렇다",4,5))))
table(survey$q19)

table(survey$q20)
survey$q20 <- ifelse(survey$q20 == "매우 그렇지 않다",1,
                     ifelse(survey$q20 == "그렇지 않다",2,
                            ifelse(survey$q20 == "보통이다",3,
                                   ifelse(survey$q20 == "그렇다",4,5))))
table(survey$q20)

table(survey$q21)
survey$q21 <- ifelse(survey$q21 == "매우 불만족",1,
                     ifelse(survey$q21 == "불만족",2,
                            ifelse(survey$q21 == "보통",3,
                                   ifelse(survey$q21 == "만족",4,5))))
table(survey$q21)

table(survey$q22_1)
survey$q22_1 <- ifelse(survey$q22_1 == "매우 그렇지 않다",1,
                       ifelse(survey$q22_1 == "그렇지 않다",2,
                              ifelse(survey$q22_1 == "보통이다",3,
                                     ifelse(survey$q22_1 == "그렇다",4,5))))
table(survey$q22_1)

table(survey$q22_2)
survey$q22_2 <- ifelse(survey$q22_2 == "매우 그렇지 않다",1,
                       ifelse(survey$q22_2 == "그렇지 않다",2,
                              ifelse(survey$q22_2 == "보통이다",3,
                                     ifelse(survey$q22_2 == "그렇다",4,5))))
table(survey$q22_2)

table(survey$q23)
survey$q23 <- ifelse(survey$q23 == "매우많다",1,
                      ifelse(survey$q23 == "많다",2,
                             ifelse(survey$q23 == "보통이다",3,
                                    ifelse(survey$q23 == "적다",4,5))))
table(survey$q23)

table(survey$q24)
survey$q24 <- ifelse(survey$q24 == "전혀 도움이 되지 않는다",1,
                     ifelse(survey$q24 == "도움이 안된다",2,
                            ifelse(survey$q24 == "보통이다",3,
                                   ifelse(survey$q24 == "도움이 된다",4,5))))
table(survey$q24)


table(survey$q25)
survey$q25 <- ifelse(survey$q25 == "주 1회",1,
                     ifelse(survey$q25 == "2주 1회",2,3))
table(survey$q25)



table(survey$q26)
survey$q26 <- ifelse(survey$q26 == "매우 적다",1,
                     ifelse(survey$q26 == "적은 편이다",2,
                            ifelse(survey$q26 == "보통이다",3,
                                   ifelse(survey$q26 == "많은 편이다",4,5))))
table(survey$q26)

table(survey$q27)
survey$q27<- ifelse(survey$q27 == "매우 그렇지 않다",1,
                       ifelse(survey$q27 == "그렇지 않다",2,
                              ifelse(survey$q27 == "보통이다",3,
                                     ifelse(survey$q27 == "그렇다",4,5))))
table(survey$q27)

table(survey$q28)
survey$q28 <- ifelse(survey$q28 == "매우 적다",1,
                     ifelse(survey$q28 == "적은 편이다",2,
                            ifelse(survey$q28 == "보통이다",3,
                                   ifelse(survey$q28 == "많은 편이다",4,5))))
table(survey$q28)


table(survey$q29)
survey$q29 <- ifelse(survey$q29 == "전혀 피드백이 없다",1,
                     ifelse(survey$q29 == "피드백이 적다",2,
                            ifelse(survey$q29 == "보통이다",3,
                                   ifelse(survey$q29 == "대부분 피드백이 있다",4,5))))
table(survey$q29)

table(survey$q30)
survey$q30<- ifelse(ori_survey$q30 == "매우 그렇지 않다",5,
                    ifelse(ori_survey$q30 == "그렇지 않다",4,
                           ifelse(ori_survey$q30 == "보통이다",3,
                                  ifelse(ori_survey$q30 == "그렇다",2,1))))
table(survey$q30)
table(ori_survey$q30)

table(survey$q31)
survey$q31<- ifelse(survey$q31 == "매우 불편하다",1,
                    ifelse(survey$q31 == "불편하다",2,
                           ifelse(survey$q31 == "잘 모르겠다",3,
                                  ifelse(survey$q31 == "편하다",4,5))))
table(survey$q31)


table(survey$q32)
survey$q32<- ifelse(survey$q32 == "매우 그렇지 않다",1,
                    ifelse(survey$q32 == "그렇지 않다",2,
                           ifelse(survey$q32 == "보통이다",3,
                                  ifelse(survey$q32 == "그렇다",4,5))))
table(survey$q32)

survey$q33 <- ori_survey$q33
table(survey$q33)
survey$q33<- ifelse(survey$q33 == "매우 적다",5,
                    ifelse(survey$q33 == "적은 편이다",4,
                           ifelse(survey$q33 == "보통이다",3,
                                  ifelse(survey$q33 == "많은 편이다",2,1))))
table(survey$q33)
table(survey$q34)
survey$q34<- ifelse(survey$q34 == "매우 그렇지 않다",1,
                    ifelse(survey$q34 == "그렇지 않다",2,
                           ifelse(survey$q34 == "보통이다",3,
                                  ifelse(survey$q34 == "그렇다",4,5))))
table(survey$q34)

table(survey$q35)
survey$q35<- ifelse(survey$q35 == "매우 그렇지 않다",1,
                    ifelse(survey$q35 == "그렇지 않다",2,
                           ifelse(survey$q35 == "보통이다",3,
                                  ifelse(survey$q35 == "그렇다",4,5))))
table(survey$q35)

table(survey$q36)
survey$q36<- ifelse(survey$q36 == "매우 그렇지 않다",1,
                    ifelse(survey$q36 == "그렇지 않다",2,
                           ifelse(survey$q36 == "보통이다",3,
                                  ifelse(survey$q36 == "그렇다",4,5))))
table(survey$q36)

table(survey$q37)
survey$q37<- ifelse(survey$q37 == "매우 그렇지 않다",1,
                    ifelse(survey$q37 == "그렇지 않다",2,
                           ifelse(survey$q37 == "보통이다",3,
                                  ifelse(survey$q37 == "그렇다",4,5))))
table(survey$q37)

table(survey$q38)
survey$q38<- ifelse(survey$q38 == "매우 낮다",1,
                    ifelse(survey$q38 == "낮다",2,
                           ifelse(survey$q38 == "보통이다",3,
                                  ifelse(survey$q38 == "높다",4,5))))
table(survey$q38)


table(survey$q39)
survey$q39<- ifelse(survey$q39 == "매우 그렇지 않다",1,
                    ifelse(survey$q39 == "그렇지 않다",2,
                           ifelse(survey$q39 == "보통이다",3,
                                  ifelse(survey$q39 == "그렇다",4,5))))
table(survey$q39)


table(survey$q40)
survey$q40<- ifelse(survey$q40 == "매우 낮다",1,
                    ifelse(survey$q40 == "낮다",2,
                           ifelse(survey$q40 == "보통이다",3,
                                  ifelse(survey$q40 == "높다",4,5))))
table(survey$q40)

str(survey)
#char 속성을 factor로 변환
survey[sapply(survey,is.character)] <- lapply(survey[sapply(survey,is.character)],as.factor)

#name은 다시 character로 변환
survey$name <- as.character(survey$name)
str(survey)

##########문항 빈도##############

install.packages("descr")
library(descr)
library(gmodels)
attach(survey)

View(freq(sex))
View(freq(grade))
View(freq(major))

round(prop.table(table(sex))*100,2)
round(prop.table(table(grade))*100,2)
round(prop.table(table(major))*100,2)
CrossTable(sex,grade,prop.c = T,prop.chisq = F,prop.t = T,format = "SPSS")
###4번 문항 빈도

#성별 교차분석
CrossTable(sex,q4,prop.c = F,chisq=T)   #유의

#학년별
CrossTable(grade,q4,prop.c=F,chisq = T)
fisher.test(grade,q4,simulate.p.value=T)

#전공별
CrossTable(major,q4,prop.c=F,chisq=T)
fisher.test(grade,q4,simulate.p.value = T)


###q5 문항
CrossTable(sex,q5,prop.c = F,chisq=T)
fisher.test(sex,q5,simulate.p.value = T)

CrossTable(grade,q5,prop.c=F,chisq = T)
fisher.test(grade,q5,simulate.p.value=T)

CrossTable(major,q5,prop.c=F,chisq=T)
fisher.test(grade,q5,simulate.p.value = T)

###q4 문항
table(q4)
round(prop.table(table(q4))*100,1)

###q4와 q5 교차분석  #유의
CrossTable(q4,q5,prop.c = F,prop.t = F,prop.chisq = F,chisq = T,format = "SPSS")

fisher.test(q4,q5,simulate.p.value = T)


###q6 문항
CrossTable(sex,q6,prop.c = F,chisq=T)

CrossTable(grade,q6,prop.c=F,chisq = T)

CrossTable(major,q6,prop.c=F,chisq=T)
fisher.test(grade,q6,simulate.p.value = T)

#q4와q6 교차분석
CrossTable(survey$q4,survey$q6,prop.c = F,prop.t = F,prop.chisq = F)
survey %>% group_by(q4,q6) %>% 
  summarise(sum = n())

4/7*100

###q7 문항
mean(q7)

CrossTable(sex,q7,prop.c = F,chisq=T)


CrossTable(grade,q7,prop.c=F,chisq = T)


CrossTable(major,q7,prop.c=F,chisq=T)
fisher.test(grade,q7,simulate.p.value = T)


###q8 문항
CrossTable(sex,q8,prop.c=F,chisq=T)
fisher.test(sex,q8,simulate.p.value = T)

CrossTable(grade,q8,prop.c=F,chisq=T)
fisher.test(grade,q8,simulate.p.value = T)

CrossTable(major,q8,prop.c=F,chisq=T)
fisher.test(major,q8,simulate.p.value = T)


###q9_1 문항
CrossTable(sex,q9_1,prop.c = F,chisq = T)
fisher.test(sex,q9_1,simulate.p.value = T)

CrossTable(grade,q9_1,prop.c = F,chisq=T)
fisher.test(grade,q9_1,simulate.p.value = T)

CrossTable(major,q9_1,prop.c = F,chisq = T)
fisher.test(major,q9_1,simulate.p.value = T)


###q9_2 문항
CrossTable(sex,q9_2,prop.c=F,chisq = T)
fisher.test(sex,q9_2,simulate.p.value = T)

CrossTable(grade,q9_2,prop.c = F,chisq=T)
fisher.test(grade,q9_2,simulate.p.value = T)

CrossTable(major,q9_2,prop.c=F,chisq = T,prop.t=F,prop.chisq = F)
fisher.test(major,q9_2,simulate.p.value = T) #유의
#q9_2 전공별 평균
survey %>% group_by(major) %>% 
  summarise(mean_sat = mean(q9_2))

library(dplyr)
###q10_1 문항
CrossTable(sex,q10_1,prop.c = F,chisq = T,prop.chisq = F,prop.t = F)
fisher.test(sex,q10_1,simulate.p.value = T) #유의
#q10_1 성별 평균
survey %>% group_by(sex) %>% 
  summarise(mean_sat = mean(q10_1))

CrossTable(grade,q10_1,prop.c=F,chisq=T)
fisher.test(grade,q10_1,simulate.p.value = T)

CrossTable(major,q10_1,prop.c = F,chisq=T)
fisher.test(major,q10_1,simulate.p.value = T) #유의



###q10_2문항

CrossTable(sex,q10_2,prop.c=F,chisq=T,prop.t=F,prop.chisq = F)
4fisher.test(sex,q10_2,simulate.p.value = T) #유의

CrossTable(grade,q10_2,prop.c = F,chisq=T)
fisher.test(grade,q10_2,simulate.p.value = T)

CrossTable(major,q10_2,prop.c = F,chisq = T)
fisher.test(major,q10_2,simulate.p.value = T) #유의

###q11_1문항
CrossTable(sex,q11_1,prop.c=F,chisq=T) #유의

CrossTable(grade,q11_1,prop.c = F,chisq=T)
fisher.test(grade,q11_1,simulate.p.value = T)

CrossTable(major,q11_1,prop.c = F,chisq=T)
fisher.test(major,q11_1,simulate.p.value = T) #유의

###q11_2 문항
CrossTable(sex,q11_2,prop.c = F,chisq=T) #유의

CrossTable(grade,q11_2,prop.c = F,chisq=T)
fisher.test(grade,q11_2,simulate.p.value = T)

CrossTable(major,q11_2,prop.c = F,chisq=T)
fisher.test(major,q11_2,simulate.p.value = T) #유의


###q12_1 문항
CrossTable(sex,q12_1,prop.c = F,chisq=T) 
fisher.test(sex,q12_1,simulate.p.value = T) #유의

CrossTable(grade,q12_1,prop.c = F,chisq=T)
fisher.test(grade,q12_1,simulate.p.value = T)

CrossTable(major,q12_1,prop.c = F,chisq=T)
fisher.test(major,q12_1,simulate.p.value = T)


###q12_2 문항
CrossTable(sex,q12_2,prop.c = F,chisq=T) 
fisher.test(sex,q12_2,simulate.p.value = T)

CrossTable(grade,q12_2,prop.c = F,chisq=T)
fisher.test(grade,q12_2,simulate.p.value = T)

CrossTable(major,q12_2,prop.c = F,chisq=T)
fisher.test(major,q12_2,simulate.p.value = T)


###q13_1 문항
CrossTable(sex,q13_1,prop.c = F,chisq=T) 
fisher.test(sex,q13_1,simulate.p.value = T) #유의

CrossTable(grade,q13_1,prop.c = F,chisq=T)
fisher.test(grade,q13_1,simulate.p.value = T)

CrossTable(major,q13_1,prop.c = F,chisq=T)
fisher.test(major,q13_1,simulate.p.value = T)


###q13_2 문항
CrossTable(sex,q13_2,prop.c = F,chisq=T) 
fisher.test(sex,q13_2,simulate.p.value = T)

CrossTable(grade,q13_2,prop.c = F,chisq=T)
fisher.test(grade,q13_2,simulate.p.value = T)

CrossTable(major,q13_2,prop.c = F,chisq=T)
fisher.test(major,q13_2,simulate.p.value = T)


###q14_1 문항
CrossTable(sex,q14_1,prop.c = F,chisq=T) 
fisher.test(sex,q14_1,simulate.p.value = T)

CrossTable(grade,q14_1,prop.c = F,chisq=T)
fisher.test(grade,q14_1,simulate.p.value = T) #유의

CrossTable(major,q14_1,prop.c = F,chisq=T)
fisher.test(major,q14_1,simulate.p.value = T) 


###q14_2 문항
CrossTable(sex,q14_2,prop.c = F,chisq=T) 
fisher.test(sex,q14_2,simulate.p.value = T) #유의

CrossTable(grade,q14_2,prop.c = F,chisq=T)
fisher.test(grade,q14_2,simulate.p.value = T)

CrossTable(major,q14_2,prop.c = F,chisq=T)
fisher.test(major,q14_2,simulate.p.value = T)


###q15_1 문항
CrossTable(sex,q15_1,prop.c = F,chisq=T) 


CrossTable(grade,q15_1,prop.c = F,chisq=T)
fisher.test(grade,q15_1,simulate.p.value = T)

CrossTable(major,q15_1,prop.c = F,chisq=T)
fisher.test(major,q15_1,simulate.p.value = T) #유의


###q15_2 문항
CrossTable(sex,q15_2,prop.c = F,chisq=T) 

CrossTable(grade,q15_2,prop.c = F,chisq=T)
fisher.test(grade,q15_2,simulate.p.value = T)

CrossTable(major,q15_2,prop.c = F,chisq=T)
fisher.test(major,q15_2,simulate.p.value = T) #유의


###q16_1 문항
CrossTable(sex,q16_1,prop.c = F,chisq=T) 
fisher.test(sex,q16_1,simulate.p.value = T)

CrossTable(grade,q16_1,prop.c = F,chisq=T)
fisher.test(grade,q16_1,simulate.p.value = T)

CrossTable(major,q16_1,prop.c = F,chisq=T)
fisher.test(major,q16_1,simulate.p.value = T)


###q16_2 문항
CrossTable(sex,q16_2,prop.c = F,chisq=T) 
fisher.test(sex,q16_2,simulate.p.value = T)

CrossTable(grade,q16_2,prop.c = F,chisq=T)
fisher.test(grade,q16_2,simulate.p.value = T)

CrossTable(major,q16_2,prop.c = F,chisq=T)
fisher.test(major,q16_2,simulate.p.value = T)


###q17_1 문항
CrossTable(sex,q17_1,prop.c = F,chisq=T) 

CrossTable(grade,q17_1,prop.c = F,chisq=T)
fisher.test(grade,q17_1,simulate.p.value = T)

CrossTable(major,q17_1,prop.c = F,chisq=T)
fisher.test(major,q17_1,simulate.p.value = T)


###q17_2 문항
CrossTable(sex,q17_2,prop.c = F,chisq=T) 

CrossTable(grade,q17_2,prop.c = F,chisq=T)
fisher.test(grade,q17_2,simulate.p.value = T)

CrossTable(major,q17_2,prop.c = F,chisq=T)
fisher.test(major,q17_2,simulate.p.value = T)


###q18_1  문항
CrossTable(sex,q18_1,prop.c = F,chisq=T)  #유의

CrossTable(grade,q18_1,prop.c = F,chisq=T)
fisher.test(grade,q18_1,simulate.p.value = T)

CrossTable(major,q18_1,prop.c = F,chisq=T)
fisher.test(major,q18_1,simulate.p.value = T)


###q18_2 문항
CrossTable(sex,q18_2,prop.c = F,chisq=T) 

CrossTable(grade,q18_2,prop.c = F,chisq=T)

CrossTable(major,q18_2,prop.c = F,chisq=T)
fisher.test(major,q18_2,simulate.p.value = T)


save(list=ls(),file = "survey.RData")

###q19 문항
CrossTable(sex,q19,prop.c = F,chisq=T) 

CrossTable(grade,q19,prop.c = F,chisq=T)

CrossTable(major,q19,prop.c = F,chisq=T) #유의


###q20  문항
CrossTable(sex,q20,prop.c = F,chisq=T) 

CrossTable(grade,q20,prop.c = F,chisq=T)

CrossTable(major,q20,prop.c = F,chisq=T)
fisher.test(major,q20,simulate.p.value = T) #유의


###q21 문항
CrossTable(sex,q21,prop.c = F,chisq=T)  #유의

CrossTable(grade,q21,prop.c = F,chisq=T)

CrossTable(major,q21,prop.c = F,chisq=T)


###q22_1 문항
CrossTable(sex,q22_1,prop.c = F,chisq=T) 

CrossTable(grade,q22_1,prop.c = F,prop.chisq=F,chisq=T)
CrossTable(major,q22_1,prop.c = F,chisq=T)
fisher.test(major,q22_1,simulate.p.value = T)


###q22_2문항
CrossTable(sex,q22_2,prop.c = F,chisq=T) 


CrossTable(grade,q22_2,prop.c = F,prop.chisq=F,chisq=T)

CrossTable(major,q22_2,prop.c = F,prop.chisq=F,chisq=T)


###q23 문항
CrossTable(sex,q23,prop.c = F,chisq=T)
fisher.test(sex,q23,simulate.p.value = T)

CrossTable(grade,q23,prop.c = F,chisq=T,prop.chisq = F)
fisher.test(grade,q23,simulate.p.value = T) #유의

CrossTable(major,q23,prop.c = F,chisq=T)
fisher.test(major,q23,simulate.p.value = T) 


###q24 문항
CrossTable(sex,q24,prop.c = F,chisq=T)

CrossTable(grade,q24,prop.c = F,chisq=T)

CrossTable(major,q24,prop.c = F,chisq=T)
fisher.test(major,q24,simulate.p.value = T)


###q25 문항
CrossTable(sex,q25,prop.c = F,chisq=T)

CrossTable(grade,q25,prop.c = F,chisq=T) #유의

CrossTable(major,q25,prop.c = F,chisq=T)


###q26 문항
CrossTable(sex,q26,prop.c = F,chisq=T)

CrossTable(grade,q26,prop.c = F,chisq=T)

CrossTable(major,q26,prop.c = F,chisq=T)
fisher.test(major,q26,simulate.p.value = T)


###q27 문항
CrossTable(sex,q27,prop.c = F,chisq=T) #유의

CrossTable(grade,q27,prop.c = F,chisq=T)

CrossTable(major,q27,prop.c = F,chisq=T)
fisher.test(major,q27,simulate.p.value = T) #유의


###q28 문항
CrossTable(sex,q28,prop.c = F,chisq=T) 

CrossTable(grade,q28,prop.c = F,chisq=T)
fisher.test(grade,q28,simulate.p.value = T)

CrossTable(major,q28,prop.c = F,chisq=T)
fisher.test(major,q28,simulate.p.value = T)


###q29 문항
CrossTable(sex,q29,prop.c = F,chisq=T) #유의

CrossTable(grade,q29,prop.c = F,chisq=T)
fisher.test(grade,q29,simulate.p.value = T)

CrossTable(major,q29,prop.c = F,chisq=T)
fisher.test(major,q29,simulate.p.value = T)


###q30 문항
CrossTable(sex,q30,prop.c = F,chisq=T)  #유의

CrossTable(grade,q30,prop.c = F,chisq=T)

CrossTable(major,q30,prop.c = F,chisq=T)
fisher.test(major,q30,simulate.p.value = T)


###q31 문항
CrossTable(sex,q31,prop.c = F,chisq=T) 

CrossTable(grade,q31,prop.c = F,chisq=T)
fisher.test(grade,q31,simulate.p.value = T) #유의

CrossTable(major,q31,prop.c = F,chisq=T)
fisher.test(major,q31,simulate.p.value = T)


###q32 문항
CrossTable(sex,q32,prop.c = F,chisq=T) 

CrossTable(grade,q32,prop.c = F,chisq=T)
fisher.test(grade,q32,simulate.p.value = T)

CrossTable(major,q32,prop.c = F,chisq=T)
fisher.test(major,q32,simulate.p.value = T) #유의


###q33 문항
CrossTable(sex,q33,prop.c = F,chisq=T) 

CrossTable(grade,q33,prop.c = F,chisq=T)
fisher.test(grade,q33,simulate.p.value = T) #유의
 
CrossTable(major,q33,prop.c = F,chisq=T)
fisher.test(major,q33,simulate.p.value = T) #유의


###q34 문항
CrossTable(sex,q34,prop.c = F,chisq=T) 

CrossTable(grade,q34,prop.c = F,chisq=T)
fisher.test(grade,q34,simulate.p.value = T)

CrossTable(major,q34,prop.c = F,chisq=T)
fisher.test(major,q34,simulate.p.value = T)


###q35 문항
CrossTable(sex,q35,prop.c = F,chisq=T) 

CrossTable(grade,q35,prop.c = F,chisq=T)
fisher.test(grade,q35,simulate.p.value = T)

CrossTable(major,q35,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q35,simulate.p.value = T) #유의


###q36 문항
CrossTable(sex,q36,prop.c = F,prop.chisq=F,chisq=T) 

CrossTable(grade,q36,prop.c = F,prop.chisq=F,chisq=T) #유의

CrossTable(major,q36,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q36,simulate.p.value = T)


###q37 문항
CrossTable(sex,q37,prop.c = F,prop.chisq=F,chisq=T) 

CrossTable(grade,q37,prop.c = F,prop.chisq=F,chisq=T) #유의

CrossTable(major,q37,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q37,simulate.p.value = T)


###q38 문항
CrossTable(sex,q38,prop.c = F,prop.chisq=F,chisq=T) 

CrossTable(grade,q38,prop.c = F,prop.chisq=F,chisq=T) #유의

CrossTable(major,q38,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q38,simulate.p.value = T)


###q39 문항
CrossTable(sex,q39,prop.c = F,prop.chisq=F,chisq=T) #유의

CrossTable(grade,q39,prop.c = F,prop.chisq=F,chisq=T)

CrossTable(major,q39,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q39,simulate.p.value = T)


###q40 문항
CrossTable(sex,q40,prop.c = F,prop.chisq=F,chisq=T) 

CrossTable(grade,q40,prop.c = F,prop.chisq=F,chisq=T)


CrossTable(major,q40,prop.c = F,prop.chisq=F,chisq=T)


###q41 문항
CrossTable(sex,q41,prop.c = F,prop.chisq=F,chisq=T) 

CrossTable(grade,q41,prop.c = F,prop.chisq=F,chisq=T)
attach(survey)
library(gmodels)
CrossTable(major,q41,prop.c = F,prop.chisq=F,chisq=T)
fisher.test(major,q41,simulate.p.value = T) #유의


###q42 문항
table(q42)
a <- length(grep("장소와 시간의 자유",q42,value=F)) 
a.1 <- a/NROW(survey)

b <- length(grep("반복시청 가능",q42,value=F))
b.1 <- b/NROW(survey)

c <- length(grep("비대면",q42,value=F))
c.1 <- c/NROW(survey)

d <- length(grep("질문",q42,value=F))
d.1 <- d/NROW(survey)

e <- length(grep("비용 절감",q42,value=F))
e.1 <- e/NROW(survey)

a+b+c+d+e #2167

q42_freq <- data.frame(c(a,b,c,d,e),
                       c(a,b,c,d,e)/2167,
                       c(a.1,b.1,c.1,d.1,e.1)
                       )

dimnames(q42_freq) = list(row=c("장소와 시간의 자유","반복시청 가능","비대면을 통한 부담 적음","자유로운 질문(의견) 표출","비용절감(교통비,식비등)"),col=c("freq","per","respon_per"))

View(q42_freq)


###q43 문항
a <- length(grep("질의응답",q43,value=F)) 
a.1 <- a/NROW(survey)

b <- length(grep("수업집중도",q43,value=F))
b.1 <- b/NROW(survey)

c <- length(grep("과제",q43,value=F))
c.1 <- c/NROW(survey)

d <- length(grep("e-learning",q43,value=F))
d.1 <- d/NROW(survey)

e <- length(grep("음질",q43,value=F))
e.1 <- e/NROW(survey)

a+b+c+d+e #1855

q43_freq <- data.frame(c(a,b,c,d,e),
                       c(a,b,c,d,e)/1855,
                       c(a.1,b.1,c.1,d.1,e.1)
)

View(q43_freq)
dimnames(q43_freq) = list(row=c("질의응답 원할하지 않음","오프라인 대비 수업집중도 하락","많은 과제로 인한 공부 방해","e-learning 시스템 개편으로 인한 불편함","저수준의 음질 및 화질"),col=c("freq","per","respon_per"))

View(q43_freq)


###q44 문항
a <- length(grep("질의응답",q44,value=F)) 
a.1 <- a/NROW(survey)

b <- length(grep("과제량",q44,value=F))
b.1 <- b/NROW(survey)

c <- length(grep("음질",q44,value=F))
c.1 <- c/NROW(survey)

d <- length(grep("공지",q44,value=F))
d.1 <- d/NROW(survey)


a+b+c+d #1632

q44_freq <- data.frame(c(a,b,c,d),
                       c(a,b,c,d)/1632,
                       c(a.1,b.1,c.1,d.1)
)

View(q44_freq)
dimnames(q44_freq) = list(row=c("질의응답관련 신속한 피드백","과제량 축소","음질 및 화질 향상","원활한 수업관련 공지"),col=c("freq","per","respon_per"))

View(q44_freq)

rm(b,c,d,e,b.1,c.1,d.1,e.1)
table(ori_survey$q5)
table(survey$q5)
###################신뢰도 분석#####################

#q5 : 원격수업환경 (q4,q6,q8)
#q7  : 학습자 태도
#---강의---
#q9 - q12 : 교육자
#q13 - 15 : 교육컨텐츠
#q16 - 18 / q22_1 : 이론강의
#q19 - 21 / q22_2: 실습관련
#q23 - q25 : 과제
#q26 - q30 : 질문&피드백
#q31 - q34 : 시스템

#---만족도---
#q35 - q37 : 학습성취도
#q38 - q41 : 전반적만족도
#q42 - q44 : 장점및 개선점

library(psych)
install.packages("ltm")
library(ltm)
attach(survey)

#q5 : 원격수업환경
remote_en <- q5
round(mean(remote_en),2)


#q7 : 학습자 태도
#학습자 태도 상위 문항 평균
learn_att <- q7
mean(learn_att)


#q9_1 ~ q12_2 : 교육자
alpha(survey[,c("q9_1","q9_2","q10_1","q10_2","q11_1","q11_2")])
#교육자 평균
educator <- (q9_1 + q9_2 + q10_1 + q10_2 + q11_1 + q11_2) /6
mean(educator)


#q13_1 ~ q15_2 : 교육컨텐츠
alpha(survey[,c("q13_1","q13_2","q14_1","q14_2","q15_1","q15_2")])
contents <- (q13_1 + q13_2 + q14_1 + q14_2 + q15_1 + q15_2)/6
mean(contents)

#q16_1 ~ q18_2 : 이론강의
alpha(survey[,c("q16_1","q16_2","q17_1","q17_2","q18_1","q18_2","q22_1")])

#이론강의 상위문항
theory <- (q16_1+q16_2+q17_1+q17_2+q18_1+q18_2+q22_1)/7

mean(theory)


#q19 ~ q22_2 : 실습강의
alpha(survey[,c("q19","q20","q21","q22_2")])
#실습강의 상위 문항
practice <- (q19+q20+q21+q22_2)/4
mean(practice)




#q23 ~ q25 : 과제 (q25 제외)
alpha(survey[,c("q23","q24")])
#과제 상위문항
task <- (q23+q24)/2
mean(task)


#q26~ q30 :질문& 피드백
alpha(survey[,c("q26","q27","q28","q29","q30")])
#피드백 상위문항
feedback <- (q26+q27+q28+q29+q30) / 5
mean(feedback)
table(q30);round(prop.table(table(q30))*100,2)

#q31 ~ q34 : 시스템 system
alpha(survey[,c("q31","q32","q33","q34")])
#시스템 상위 문항
system <- (q31+q32+q33+q34) / 4
mean(system)

attach(survey)

#q35 ~ q37 : 학습 성취도 achiev
alpha(survey[,c("q35","q36","q37")])
#성취도 상위 문항
achiev <- (q35+q36+q37) / 3
mean(achiev)

#38 ~ q41 : 전반적 만족도 satisfacion

alpha(survey[,c("q38","q39","q40")])

satisfaction <- (q38 + q39 + q40 )/ 3
mean(satisfaction)

############## t-test ########################


#remote_en : 원격수업환경
#learn_att : 학습자 태도
#educator : 교육자
#theory : 이론강의
#practice : 실습강의
#task : 과제
#feedback : 피드백
#system : 시스템
#achiev : 학습성취도
#satisfaction : 만족도


p_question <- data.frame(sex,grade,major,remote_en,learn_att,educator,contents,theory,
                    practice,task,feedback,system,achiev,satisfaction)
head(p_question)
str(p_question)

######성별 t.test##########

#성별 var.test 함수
sex.var_func <- function(obj,sex){
  a <- data.frame(obj,sex)
  var.test(a[a$sex=="남자",1],a[a$sex=="여자",1])
 
}
#성별 t.test 함수
sex.t_func <- function(obj,sex){
  a <- data.frame(obj,sex)
  t.test(a[a$sex=="남자",1],a[a$sex=="여자",1])
}



attach(p_question)


#성별 원격수업환경 t-test
sex.var_func(remote_en,sex)
sex.t_func(remote_en,sex)

##성별 학습자태도 t-test
sex.var_func(learn_att,sex)
sex.t_func(learn_att,sex)


#성별 교육자 t-test
sex.var_func(educator,sex)
sex.t_func(educator,sex)

#성별 교육컨텐츠 t-test
sex.var_func(contents,sex)
sex.t_func(contents,sex)

#성별 이론강의 t-test
sex.var_func(theory,sex)
sex.t_func(theory,sex)

#성별  실습강의t-test  
sex.var_func(practice,sex)
sex.t_func(practice,sex)

#성별  과제 t-test
sex.var_func(task,sex)
sex.t_func(task,sex)

#성별  질문&피드백 t-test
sex.var_func(feedback,sex)
sex.t_func(feedback,sex)

#성별 시스템 t-test
sex.var_func(system,sex)
sex.t_func(system,sex)

#성별 학습성취도 t-test
sex.var_func(achiev,sex)
sex.t_func(achiev,sex)

#성별 만족도 t-tests
sex.var_func(satisfaction,sex)
sex.t_func(satisfaction,sex)




######학년별 평균비교(onw-way Anova)##########


###학년별 원격수업 평균 비교
anova <-aov(remote_en~grade,data=p_question)
summary(anova)


###학년별 학습자 태도 평균 비교
anova <- aov(learn_att~grade,data=p_question)
summary(anova)


###학년별 교육자 평균 비교   
anova <- aov(educator~grade,data=p_question)   
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova)     #유의 : 1학년과 4학년의 평균이 다름
Tukey
plot(Tukey)


###학년별 교육컨텐츠 평균 비교
anova <- aov(contents ~ grade , data=p_question)
summary(anova)


###학년별 이론강의 평균 비교
anova <- aov(theory~grade,data=p_question)
summary(anova)


###학년별 실습강의 평균 비교
anova <- aov(practice~grade,data=p_question)
summary(anova)


###학년별 과제 평균 비교
anova <- aov(task~grade,data=p_question)
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova)  #유의 : 1 -2 학년/ 1 - 4학년 과제 평균 다름
Tukey
plot(Tukey)


###학년별 피드백 평균 비교
anova <- aov(feedback~grade,data=p_question)
summary(anova)


###학년별 시스템 평균 비교
anova <- aov(system~grade,data=p_question)
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova) #유의 : 1-4학년간 시스템 평균이 다름
Tukey
plot(Tukey)


###학년별 학습성취도 평균 비교
anova <- aov(achiev~grade,data=p_question)
summary(anova)


###학년별 만족도 평균 비교
anova <- aov(satisfaction~grade,data=p_question)
summary(anova)

# 학년별 교육자,과제,시스템 평균이 유의함




######전공별 평균비교(onw-way Anova)##########

###전공별 원격수업 평균 비교
anova <-aov(remote_en~major,data=p_question)
summary(anova)


###전공별 학습자 태도 평균 비교
anova <- aov(learn_att~major,data=p_question)
summary(anova)
duncan.test(anova,"major",console = T)
#사후검정
Tukey <- TukeyHSD(anova)  #유의 : a = 전자공학전공
plot(Tukey)


###전공별 교육자 평균 비교   
anova <- aov(educator~major,data=p_question)   
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova)    
Tukey
plot(Tukey)
install.packages("agricolae")
library(agricolae)
duncan.test(anova,"major",console = T)  # 유의 : 그룹 a = 산공,환경에너지공



###전공별 교육컨텐츠 평균 비
anova <- aov(contents ~ major,data=p_question)
summary(anova)


###전공별 이론강의 평균 비교
anova <- aov(theory~major,data=p_question)
summary(anova)


###전공별 실습강의 평균 비교
anova <- aov(practice~major,data=p_question)
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova)              #유의 : 에너지-산공 / 컴공 - 신소재 / 컴공-에너지 / 휴먼 - 컴공 차이남
Tukey
plot(Tukey)
duncan.test(anova,"major",console=T)  #그룹 a = 산공,컴공 


###전공별 과제 평균 비교
anova <- aov(task~major,data=p_question)
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova)  
Tukey
plot(Tukey)
duncan.test(anova,"major",console=T) #유의 : 그룹 a = 산공


###전공별 피드백 평균 비교
anova <- aov(feedback~major,data=p_question)
summary(anova)


###전공별 시스템 평균 비교
anova <- aov(system~major,data=p_question)
summary(anova)
#사후검정
Tukey <- TukeyHSD(anova) 
Tukey
plot(Tukey)
duncan.test(anova,"major",console = T) # 유의 : 그룹 a = 전자공학과  / 환공 - 전자공 차이


###전공별 학습성취도 평균 비교
anova <- aov(achiev~major,data=p_question)
summary(anova)


###전공별 만족도 평균 비교
anova <- aov(satisfaction~major,data=p_question)
summary(anova)

# 전공별 교육자,실습강의,과제,시스템 평균이 유의함

.libPaths()


#########이론 / 이론+실습 비교############
attach(survey)

##하위문항 평균 비교
#교육자 하위문항
t.test(q9_1,q9_2)
t.test(q10_1,q10_2)
t.test(q11_1,q11_2)
t.test(q12_1,q12_2)
th_edu <- (q9_1+q10_1+q11_1+q12_1)/4
th_p_edu <- (q9_2+q10_2+q11_2+q12_2)/4
#교육자 상위개념 비교
t.test(th_edu,th_p_edu)

#교육컨텐츠 하위문항
t.test(q13_1,q13_2)
t.test(q14_1,q14_2)
t.test(q15_1,q15_2)
th_con <- (q13_1+q14_1+q15_1)/3
th_p_con <- (q13_2+q14_2+q15_2)/3
#교육컨텐츠 상위개념 비교
t.test(th_con,th_p_con)

#이론강의 하위문항
t.test(q16_1,q16_2)
t.test(q17_1,q17_2)
t.test(q18_1,q18_2)
th_th <- (q16_1+q17_1+q18_1)/3
th_p_th <- (q16_2+q17_2+q18_2)/3
#이론강의 상위개념 비교
t.test(th_th,th_p_th)

#장점 평균 비교
t.test(q22_1,q22_2) #유의 (이론강의 장점도가 실습강의에 보다 평균이 큼에 유의)

#이론강의 상위개념과 장점 회귀분석 
library(MASS)
lm_th <-lm(q22_1 ~ th_edu+th_con+th_th)
step <- stepAIC(lm_th,direction = "both")
summary(step)

#이론+실습강의 상위개념과 장점 회귀분석
th_p_p <- (q19+q20+q21)/3
lm_th_p <- lm(q22_2 ~ th_p_edu+th_p_con+th_p_th+th_p_p)
step <- stepAIC(lm_th_p,direction = "both")
summary(step)

#이론+실습강의 만족정도 회귀분석
th_p_p <- (q19+q20)/2
lm_th_p <- lm(q22_2 ~ th_p_edu+th_p_con+th_p_th+th_p_p)
step <- stepAIC(lm_th_p,direction = "both")
summary(step)


####상관분석######
str(p_question)
cor_question <- subset(p_question,select = c(-sex,-grade,-major))
str(cor_question)
cor(cor_question)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
install.packages("corrplot")
library(corrplot)

chart.Correlation(cor_question)
corrplot(cor(cor_question),method="number",type = "lower")
#########회귀분석#############
str(cor_question)

str(lm_question)

library(dplyr)
lm_question <- rename(lm_question,
                       "sat" = "survey.q40")
lm_question <- subset(lm_question,select = -satisfaction)
str(lm_question)

lm_model <- lm(sat ~ remote_en + learn_att + educator + contents + theory + 
                 practice + task + feedback +system + achiev,data=lm_question )
summary(lm_model)
AIC(lm_model)

#theory 제거
lm_model2 <- lm(sat ~ remote_en + learn_att + educator + feedback +contents+
                 practice + task + system + achiev,data=lm_question )
summary(lm_model2)
AIC(lm_model2)

#contents 제거 
lm_model2 <- lm(sat ~ remote_en + learn_att + educator +feedback+
                 practice + task + system + achiev,data=lm_question )
summary(lm_model2)
AIC(lm_model2)

#feedback 제거 
lm_model2 <- lm(sat ~ remote_en + learn_att + educator +
                  practice + task + system + achiev,data=lm_question )
summary(lm_model2)
AIC(lm_model2)

#remote_en 제거 -> 최적 모형
lm_model2 <- lm(sat ~  + learn_att + educator +
                  practice + task + system + achiev,data=lm_question )
summary(lm_model2)
AIC(lm_model2)



#다중공선성 확인
install.packages("perturb")
library(perturb)
install.packages("car")
library(car)

#colldiag 이용
colldiag(lm_model,center = T,add.intercept = F)

#vif이용
round(vif(lm_model),2)
round(vif(lm_model2),2)

#step 이용
library(MASS)
step <- stepAIC(lm_model,direction="both")  #학습자태도/교육자/실습/과제/시스템/성취도가 만족도에 영향
step$anova

#잔차 정규성
shapiro.test(residuals(lm_model)) #잔차가 정규성 따름


save(list=ls(),file="daejin_survey.RData")

#만족도+성취도평균값 회귀분석
temp_cor_question <- subset(cor_question,select=c(-satisfaction,-achiev))
temp_cor_question
lm_as_question <- data.frame(temp_cor_question,Y=(cor_question$achiev+cor_question$satisfaction)/2)

lm_model <- lm(Y ~ remote_en + learn_att + educator + contents + theory + 
                 practice + task + feedback +system,data=lm_as_question )
summary(lm_model)

#최적모형
lm_model2 <- lm(Y ~ remote_en + learn_att + educator + 
                  practice + task + feedback +system,data=lm_as_question  )
summary(lm_model2)
step <- stepAIC(lm_model,direction = "both")
