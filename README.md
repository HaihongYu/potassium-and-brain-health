# potassium-and-brain-health
library(R.utils)
library(data.table)
library(tidyverse)
library(car) 
library("survival")
diet<-fread("dietary_Na_K.csv")#fread函数快速读取文件

K_diet1<-diet[,c(1,2,7)]
setnames(K_diet1,c("X26024-0.0"),c("K1"))
setnames(K_diet1,c("X26052-0.0"),c("Na1"))
K_diet1<-na.omit(K_diet1)

K_diet2<-diet[,c(1,3,8)]
setnames(K_diet2,c("X26024-1.0"),c("K2"))
setnames(K_diet2,c("X26052-1.0"),c("Na2"))
K_diet2<-na.omit(K_diet2)

K_diet3<-diet[,c(1,4,9)]
setnames(K_diet3,c("X26024-2.0"),c("K3"))
setnames(K_diet3,c("X26052-2.0"),c("Na3"))
K_diet3<-na.omit(K_diet3)

K_diet4<-diet[,c(1,5,10)]
setnames(K_diet4,c("X26024-3.0"),c("K4"))
setnames(K_diet4,c("X26052-3.0"),c("Na4"))
K_diet4<-na.omit(K_diet4)

K_diet5<-diet[,c(1,6,11)]
setnames(K_diet5,c("X26024-4.0"),c("K5"))
setnames(K_diet5,c("X26052-4.0"),c("Na5"))
K_diet5<-na.omit(K_diet5)

K_diet<-merge(K_diet1,K_diet2,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet3,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet4,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet5,by="eid",all=TRUE)

##求平均钠钾摄入
K_diet<-K_diet%>%mutate(K24h_diet=(rowMeans(K_diet[,c(2,4,6,8,10)],na.rm=TRUE)))
K_diet<-K_diet%>%mutate(Na24h_diet=(rowMeans(K_diet[,c(3,5,7,9,11)],na.rm=TRUE)))
K_diet<-K_diet[,c(1,12,13)]

diet<-fread("yhh_data.csv")#fread函数快速读取文件
diet<-diet[,c(1,(10:14),(395:399))]
###选择饮食typical的
diet1<-diet[,c(1,2)]
setnames(diet1,c("100020-0.0"),c("typical1"))
diet1<-diet1%>%filter(diet1$typical1==1)
diet2<-diet[,c(1,3)]
setnames(diet2,c("100020-1.0"),c("typical2"))
diet2<-diet2%>%filter(diet2$typical2==1)
diet3<-diet[,c(1,4)]
setnames(diet3,c("100020-2.0"),c("typical3"))
diet3<-diet3%>%filter(diet3$typical3==1)
diet4<-diet[,c(1,5)]
setnames(diet4,c("100020-3.0"),c("typical4"))
diet4<-diet4%>%filter(diet4$typical4==1)
diet5<-diet[,c(1,6)]
setnames(diet5,c("100020-4.0"),c("typical5"))
diet5<-diet5%>%filter(diet5$typical5==1)

###选择energy real的500–3500 kcal/day in women and 800–4000 kcal/ day in men
energy1<-diet[,c(1,7)]
setnames(energy1,c("26002-0.0"),c("energy1"))
energy1<-na.omit(energy1)
energy2<-diet[,c(1,8)]
setnames(energy2,c("26002-1.0"),c("energy2"))
energy2<-na.omit(energy2)
energy3<-diet[,c(1,9)]
setnames(energy3,c("26002-2.0"),c("energy3"))
energy3<-na.omit(energy3)
energy4<-diet[,c(1,10)]
setnames(energy4,c("26002-3.0"),c("energy4"))
energy4<-na.omit(energy4)
energy5<-diet[,c(1,11)]
setnames(energy5,c("26002-4.0"),c("energy5"))
energy5<-na.omit(energy5)
diet1<-merge(diet1,energy1,by="eid")
diet2<-merge(diet2,energy2,by="eid")
diet3<-merge(diet3,energy3,by="eid")
diet4<-merge(diet4,energy4,by="eid")
diet5<-merge(diet5,energy5,by="eid")
sex<-covariate[,c(1,3)]
diet1<-diet1%>%mutate(energy_kcal1=diet1$energy1*0.2389)
diet2<-diet2%>%mutate(energy_kcal2=diet2$energy2*0.2389)
diet3<-diet3%>%mutate(energy_kcal3=diet3$energy3*0.2389)
diet4<-diet4%>%mutate(energy_kcal4=diet4$energy4*0.2389)
diet5<-diet5%>%mutate(energy_kcal5=diet5$energy5*0.2389)
diet1<-merge(diet1,sex,by="eid")
diet2<-merge(diet2,sex,by="eid")
diet3<-merge(diet3,sex,by="eid")
diet4<-merge(diet4,sex,by="eid")
diet5<-merge(diet5,sex,by="eid")
diet1_0<-diet1%>%filter(diet1$sex==0)
diet1_0<-diet1_0%>%filter(energy_kcal1 >= 500)
diet1_0<-diet1_0%>%filter(energy_kcal1 <= 3500)
diet1_1<-diet1%>%filter(diet1$sex==1)
diet1_1<-diet1_1%>%filter(energy_kcal1 >= 800)
diet1_1<-diet1_1%>%filter(energy_kcal1 <= 4000)
diet1<-rbind(diet1_0,diet1_1)
diet2_0<-diet2%>%filter(diet2$sex==0)
diet2_0<-diet2_0%>%filter(energy_kcal2 >= 500)
diet2_0<-diet2_0%>%filter(energy_kcal2 <= 3500)
diet2_1<-diet2%>%filter(diet2$sex==1)
diet2_1<-diet2_1%>%filter(energy_kcal2 >= 800)
diet2_1<-diet2_1%>%filter(energy_kcal2 <= 4000)
diet2<-rbind(diet2_0,diet2_1)
diet3_0<-diet3%>%filter(diet3$sex==0)
diet3_0<-diet3_0%>%filter(energy_kcal3 >= 500)
diet3_0<-diet3_0%>%filter(energy_kcal3 <= 3500)
diet3_1<-diet3%>%filter(diet3$sex==1)
diet3_1<-diet3_1%>%filter(energy_kcal3 >= 800)
diet3_1<-diet3_1%>%filter(energy_kcal3 <= 4000)
diet3<-rbind(diet3_0,diet3_1)
diet4_0<-diet4%>%filter(diet4$sex==0)
diet4_0<-diet4_0%>%filter(energy_kcal4 >= 500)
diet4_0<-diet4_0%>%filter(energy_kcal4 <= 3500)
diet4_1<-diet4%>%filter(diet4$sex==1)
diet4_1<-diet4_1%>%filter(energy_kcal4 >= 800)
diet4_1<-diet4_1%>%filter(energy_kcal4 <= 4000)
diet4<-rbind(diet4_0,diet4_1)
diet5_0<-diet5%>%filter(diet5$sex==0)
diet5_0<-diet5_0%>%filter(energy_kcal5 >= 500)
diet5_0<-diet5_0%>%filter(energy_kcal5 <= 3500)
diet5_1<-diet5%>%filter(diet5$sex==1)
diet5_1<-diet5_1%>%filter(energy_kcal5 >= 800)
diet5_1<-diet5_1%>%filter(energy_kcal5 <= 4000)
diet5<-rbind(diet5_0,diet5_1)

diet1<-diet1[,c(1,4)]
diet2<-diet2[,c(1,4)]
diet3<-diet3[,c(1,4)]
diet4<-diet4[,c(1,4)]
diet5<-diet5[,c(1,4)]

diet<-merge(diet1,diet2,by="eid",all=TRUE)
diet<-merge(diet,diet3,by="eid",all=TRUE)
diet<-merge(diet,diet4,by="eid",all=TRUE)
diet<-merge(diet,diet5,by="eid",all=TRUE)
###在有NA的情况下计算每一行的均值,求平均能量摄入
diet<-diet%>%mutate(energy_kcal=(rowMeans(diet[,c(2:6)],na.rm=TRUE)))
diet<-diet[,c(1,7)]

##合并饮食经典且能量摄入合理且有钠钾摄入
k_diet<-merge(K_diet,diet,by="eid")
write.csv(k_diet,"diet_K.csv")#保存合并后csv数据

covariate<-fread("covariate_last24.6.csv")#fread函数快速读取文件
covariate<-covariate[,c(2:15)]
K_diet<-merge(covariate,k_diet,by="eid")

K_diet<-K_diet%>%filter(CKD == 0)

##换算单位
K_diet<-K_diet%>%mutate(K24h_diet=K_diet$K24h_diet/1000)
K_diet<-K_diet%>%mutate(Na24h_diet=K_diet$Na24h_diet/1000)

##计算分位数
quantile(c(K_diet$K24h_diet),probs=seq(0,1,1/4 ))
K_diet1<-K_diet%>%filter(K_diet$K24h<=2.988085)
K_diet2<-K_diet%>%filter(K_diet$K24h>2.988085)
K_diet2<-K_diet2%>%filter(K_diet2$K24h<=3.595417)
K_diet3<-K_diet%>%filter(K_diet$K24h>3.595417)
K_diet3<-K_diet3%>%filter(K_diet3$K24h<=4.254772)
K_diet4<-K_diet%>%filter(K_diet$K24h>4.254772)

K_diet1<-K_diet1%>%mutate(K24h_diet_Q=1)
K_diet2<-K_diet2%>%mutate(K24h_diet_Q=2)
K_diet3<-K_diet3%>%mutate(K24h_diet_Q=3)
K_diet4<-K_diet4%>%mutate(K24h_diet_Q=4)
K_diet<-rbind(K_diet1,K_diet2)
K_diet<-rbind(K_diet,K_diet3)
K_diet<-rbind(K_diet,K_diet4)


Stroke<-fread("Stroke.csv")#fread函数快速读取文件
PD<-fread("PD.csv")#fread函数快速读取文件
Dementia<-fread("Dementia.csv")#fread函数快速读取文件
Epilepsy<-fread("Epilepsy.csv")#fread函数快速读取文件
MDD<-fread("MDD.csv")#fread函数快速读取文件
Schizophrenia<-fread("Schizophrenia.csv")#fread函数快速读取文件
Anxiety<-fread("Anxiety.csv")#fread函数快速读取文件
Bipolar<-fread("Bipolar.csv")#fread函数快速读取文件

Stroke<-Stroke%>%mutate(Stroke_years=Stroke_days/365)
PD<-PD%>%mutate(PD_years=PD_days/365)
Dementia<-Dementia%>%mutate(Dementia_years=Dementia_days/365)
Epilepsy<-Epilepsy%>%mutate(Epilepsy_years=Epilepsy_days/365)
MDD<-MDD%>%mutate(MDD_years=MDD_days/365)
Schizophrenia<-Schizophrenia%>%mutate(Schizophrenia_years=Schizophrenia_days/365)
Anxiety<-Anxiety%>%mutate(Anxiety_years=Anxiety_days/365)
Bipolar<-Bipolar%>%mutate(Bipolar_years=Bipolar_days/365)

Cox_braindisease<-merge(Dementia,Stroke,by="eid")
Cox_braindisease<-merge(Cox_braindisease,PD,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Epilepsy,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Schizophrenia,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Bipolar,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Anxiety,by="eid")
Cox_braindisease<-merge(Cox_braindisease,MDD,by="eid")

##去掉基线患有脑疾病的
Cox_braindisease<-Cox_braindisease%>%filter(Dementia_days>0&PD_days>0&Stroke_days>0&Epilepsy_days>0&Schizophrenia_days>0&MDD_days>0&Anxiety_days>0&Bipolar_days>0)

Cox_braindisease<-merge(K_diet,Cox_braindisease,by="eid")

####最终人群
K_diet_new<-Cox_braindisease[,c(1:18)]#提取这个文件中的这些列
write.csv(K_diet_new,"K_diet_new.csv")#保存合并后csv数据

diet<-fread("dietary_Na_K.csv")#fread函数快速读取文件

K_diet1<-diet[,c(1,2,7)]
setnames(K_diet1,c("X26024-0.0"),c("K1"))
setnames(K_diet1,c("X26052-0.0"),c("Na1"))
K_diet1<-na.omit(K_diet1)

K_diet2<-diet[,c(1,3,8)]
setnames(K_diet2,c("X26024-1.0"),c("K2"))
setnames(K_diet2,c("X26052-1.0"),c("Na2"))
K_diet2<-na.omit(K_diet2)

K_diet3<-diet[,c(1,4,9)]
setnames(K_diet3,c("X26024-2.0"),c("K3"))
setnames(K_diet3,c("X26052-2.0"),c("Na3"))
K_diet3<-na.omit(K_diet3)

K_diet4<-diet[,c(1,5,10)]
setnames(K_diet4,c("X26024-3.0"),c("K4"))
setnames(K_diet4,c("X26052-3.0"),c("Na4"))
K_diet4<-na.omit(K_diet4)

K_diet5<-diet[,c(1,6,11)]
setnames(K_diet5,c("X26024-4.0"),c("K5"))
setnames(K_diet5,c("X26052-4.0"),c("Na5"))
K_diet5<-na.omit(K_diet5)

K_diet<-merge(K_diet1,K_diet2,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet3,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet4,by="eid",all=TRUE)
K_diet<-merge(K_diet,K_diet5,by="eid",all=TRUE)

##求平均钠钾摄入
K_diet<-K_diet%>%mutate(K24h_diet=(rowMeans(K_diet[,c(2,4,6,8,10)],na.rm=TRUE)))
K_diet<-K_diet%>%mutate(Na24h_diet=(rowMeans(K_diet[,c(3,5,7,9,11)],na.rm=TRUE)))
K_diet<-K_diet[,c(1,12,13)]

diet<-fread("yhh_data.csv")#fread函数快速读取文件
diet<-diet[,c(1,(10:14),(395:399))]
###选择饮食typical的
diet1<-diet[,c(1,2)]
setnames(diet1,c("100020-0.0"),c("typical1"))
diet1<-diet1%>%filter(diet1$typical1==1)
diet2<-diet[,c(1,3)]
setnames(diet2,c("100020-1.0"),c("typical2"))
diet2<-diet2%>%filter(diet2$typical2==1)
diet3<-diet[,c(1,4)]
setnames(diet3,c("100020-2.0"),c("typical3"))
diet3<-diet3%>%filter(diet3$typical3==1)
diet4<-diet[,c(1,5)]
setnames(diet4,c("100020-3.0"),c("typical4"))
diet4<-diet4%>%filter(diet4$typical4==1)
diet5<-diet[,c(1,6)]
setnames(diet5,c("100020-4.0"),c("typical5"))
diet5<-diet5%>%filter(diet5$typical5==1)

###选择energy real的500–3500 kcal/day in women and 800–4000 kcal/ day in men
energy1<-diet[,c(1,7)]
setnames(energy1,c("26002-0.0"),c("energy1"))
energy1<-na.omit(energy1)
energy2<-diet[,c(1,8)]
setnames(energy2,c("26002-1.0"),c("energy2"))
energy2<-na.omit(energy2)
energy3<-diet[,c(1,9)]
setnames(energy3,c("26002-2.0"),c("energy3"))
energy3<-na.omit(energy3)
energy4<-diet[,c(1,10)]
setnames(energy4,c("26002-3.0"),c("energy4"))
energy4<-na.omit(energy4)
energy5<-diet[,c(1,11)]
setnames(energy5,c("26002-4.0"),c("energy5"))
energy5<-na.omit(energy5)
diet1<-merge(diet1,energy1,by="eid")
diet2<-merge(diet2,energy2,by="eid")
diet3<-merge(diet3,energy3,by="eid")
diet4<-merge(diet4,energy4,by="eid")
diet5<-merge(diet5,energy5,by="eid")
sex<-covariate[,c(1,3)]
diet1<-diet1%>%mutate(energy_kcal1=diet1$energy1*0.2389)
diet2<-diet2%>%mutate(energy_kcal2=diet2$energy2*0.2389)
diet3<-diet3%>%mutate(energy_kcal3=diet3$energy3*0.2389)
diet4<-diet4%>%mutate(energy_kcal4=diet4$energy4*0.2389)
diet5<-diet5%>%mutate(energy_kcal5=diet5$energy5*0.2389)
diet1<-merge(diet1,sex,by="eid")
diet2<-merge(diet2,sex,by="eid")
diet3<-merge(diet3,sex,by="eid")
diet4<-merge(diet4,sex,by="eid")
diet5<-merge(diet5,sex,by="eid")
diet1_0<-diet1%>%filter(diet1$sex==0)
diet1_0<-diet1_0%>%filter(energy_kcal1 >= 500)
diet1_0<-diet1_0%>%filter(energy_kcal1 <= 3500)
diet1_1<-diet1%>%filter(diet1$sex==1)
diet1_1<-diet1_1%>%filter(energy_kcal1 >= 800)
diet1_1<-diet1_1%>%filter(energy_kcal1 <= 4000)
diet1<-rbind(diet1_0,diet1_1)
diet2_0<-diet2%>%filter(diet2$sex==0)
diet2_0<-diet2_0%>%filter(energy_kcal2 >= 500)
diet2_0<-diet2_0%>%filter(energy_kcal2 <= 3500)
diet2_1<-diet2%>%filter(diet2$sex==1)
diet2_1<-diet2_1%>%filter(energy_kcal2 >= 800)
diet2_1<-diet2_1%>%filter(energy_kcal2 <= 4000)
diet2<-rbind(diet2_0,diet2_1)
diet3_0<-diet3%>%filter(diet3$sex==0)
diet3_0<-diet3_0%>%filter(energy_kcal3 >= 500)
diet3_0<-diet3_0%>%filter(energy_kcal3 <= 3500)
diet3_1<-diet3%>%filter(diet3$sex==1)
diet3_1<-diet3_1%>%filter(energy_kcal3 >= 800)
diet3_1<-diet3_1%>%filter(energy_kcal3 <= 4000)
diet3<-rbind(diet3_0,diet3_1)
diet4_0<-diet4%>%filter(diet4$sex==0)
diet4_0<-diet4_0%>%filter(energy_kcal4 >= 500)
diet4_0<-diet4_0%>%filter(energy_kcal4 <= 3500)
diet4_1<-diet4%>%filter(diet4$sex==1)
diet4_1<-diet4_1%>%filter(energy_kcal4 >= 800)
diet4_1<-diet4_1%>%filter(energy_kcal4 <= 4000)
diet4<-rbind(diet4_0,diet4_1)
diet5_0<-diet5%>%filter(diet5$sex==0)
diet5_0<-diet5_0%>%filter(energy_kcal5 >= 500)
diet5_0<-diet5_0%>%filter(energy_kcal5 <= 3500)
diet5_1<-diet5%>%filter(diet5$sex==1)
diet5_1<-diet5_1%>%filter(energy_kcal5 >= 800)
diet5_1<-diet5_1%>%filter(energy_kcal5 <= 4000)
diet5<-rbind(diet5_0,diet5_1)

diet1<-diet1[,c(1,4)]
diet2<-diet2[,c(1,4)]
diet3<-diet3[,c(1,4)]
diet4<-diet4[,c(1,4)]
diet5<-diet5[,c(1,4)]

diet<-merge(diet1,diet2,by="eid",all=TRUE)
diet<-merge(diet,diet3,by="eid",all=TRUE)
diet<-merge(diet,diet4,by="eid",all=TRUE)
diet<-merge(diet,diet5,by="eid",all=TRUE)
###在有NA的情况下计算每一行的均值,求平均能量摄入
diet<-diet%>%mutate(energy_kcal=(rowMeans(diet[,c(2:6)],na.rm=TRUE)))
diet<-diet[,c(1,7)]

##合并饮食经典且能量摄入合理且有钠钾摄入
k_diet<-merge(K_diet,diet,by="eid")
write.csv(k_diet,"diet_K.csv")#保存合并后csv数据

covariate<-fread("covariate_last24.6.csv")#fread函数快速读取文件
covariate<-covariate[,c(2:15)]
K_diet<-merge(covariate,k_diet,by="eid")

K_diet<-K_diet%>%filter(CKD == 0)

##换算单位
K_diet<-K_diet%>%mutate(K24h_diet=K_diet$K24h_diet/1000)
K_diet<-K_diet%>%mutate(Na24h_diet=K_diet$Na24h_diet/1000)

##计算分位数
quantile(c(K_diet$K24h_diet),probs=seq(0,1,1/4 ))
K_diet1<-K_diet%>%filter(K_diet$K24h<=2.988085)
K_diet2<-K_diet%>%filter(K_diet$K24h>2.988085)
K_diet2<-K_diet2%>%filter(K_diet2$K24h<=3.595417)
K_diet3<-K_diet%>%filter(K_diet$K24h>3.595417)
K_diet3<-K_diet3%>%filter(K_diet3$K24h<=4.254772)
K_diet4<-K_diet%>%filter(K_diet$K24h>4.254772)

K_diet1<-K_diet1%>%mutate(K24h_diet_Q=1)
K_diet2<-K_diet2%>%mutate(K24h_diet_Q=2)
K_diet3<-K_diet3%>%mutate(K24h_diet_Q=3)
K_diet4<-K_diet4%>%mutate(K24h_diet_Q=4)
K_diet<-rbind(K_diet1,K_diet2)
K_diet<-rbind(K_diet,K_diet3)
K_diet<-rbind(K_diet,K_diet4)


Stroke<-fread("Stroke.csv")#fread函数快速读取文件
PD<-fread("PD.csv")#fread函数快速读取文件
Dementia<-fread("Dementia.csv")#fread函数快速读取文件
Epilepsy<-fread("Epilepsy.csv")#fread函数快速读取文件
MDD<-fread("MDD.csv")#fread函数快速读取文件
Schizophrenia<-fread("Schizophrenia.csv")#fread函数快速读取文件
Anxiety<-fread("Anxiety.csv")#fread函数快速读取文件
Bipolar<-fread("Bipolar.csv")#fread函数快速读取文件

Stroke<-Stroke%>%mutate(Stroke_years=Stroke_days/365)
PD<-PD%>%mutate(PD_years=PD_days/365)
Dementia<-Dementia%>%mutate(Dementia_years=Dementia_days/365)
Epilepsy<-Epilepsy%>%mutate(Epilepsy_years=Epilepsy_days/365)
MDD<-MDD%>%mutate(MDD_years=MDD_days/365)
Schizophrenia<-Schizophrenia%>%mutate(Schizophrenia_years=Schizophrenia_days/365)
Anxiety<-Anxiety%>%mutate(Anxiety_years=Anxiety_days/365)
Bipolar<-Bipolar%>%mutate(Bipolar_years=Bipolar_days/365)

Cox_braindisease<-merge(Dementia,Stroke,by="eid")
Cox_braindisease<-merge(Cox_braindisease,PD,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Epilepsy,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Schizophrenia,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Bipolar,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Anxiety,by="eid")
Cox_braindisease<-merge(Cox_braindisease,MDD,by="eid")

##去掉基线患有脑疾病的
Cox_braindisease<-Cox_braindisease%>%filter(Dementia_days>0&PD_days>0&Stroke_days>0&Epilepsy_days>0&Schizophrenia_days>0&MDD_days>0&Anxiety_days>0&Bipolar_days>0)

Cox_braindisease<-merge(K_diet,Cox_braindisease,by="eid")

####最终人群
K_diet_new<-Cox_braindisease[,c(1:18)]#提取这个文件中的这些列
write.csv(K_diet_new,"K_diet_new.csv")#保存合并后csv数据

###结构方程模型
library(lavaan)

sem.model1<-'
         urine_K=~1*K24h
         inflammation=~1*X30200_0.0+X30140_0.0+X30180_0.0
         metabolism=~1*X30630_0.0+X23566+X23567
         status=~1*Dementia_status+Stroke_status+PD_status+Anxiety_status+MDD_status
         
         status~urine_K+inflammation+metabolism
         inflammation~urine_K
         metabolism~urine_K
        
        metabolism~~inflammation
        '
fit<-sem(sem.model1,data=urine_sem_new,se="boot",bootstrap=10000,estimator="ML")
summary(fit,fit.measures=T,standardized=TRUE)
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
