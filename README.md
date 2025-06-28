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

##brain_function_continuous
brain_function<-fread("brain_function_cross.csv")#fread函数快速读取文件
brain_function<-brain_function[,c((2:14),(16:20))]
brain_function<-merge(K_diet_new,brain_function,by="eid")
var1 <- colnames(brain_function)[c(19:35)]
var2 <- paste0(var1,'Z')
var3 <- paste0(var1,'log')
for (i in 1:length(var1)) {
  brain_function[[var2[[i]]]] <- bcPower(brain_function[[var1[[i]]]],
                                         powerTransform(brain_function[[var1[[i]]]],
                                                        family = 'bcPower')$roundlam)
}
var1 <- paste0(var1,'Z')
var2 <- c('K24h_diet')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+BMI+smoke+alchol')
Result <- list()
for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = brain_function)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw <- x[["coefficients"]][2,4]
      beta <- sprintf('%0.3f',x[["coefficients"]][2,1])
      res<-c(Var3,Var1,Var2, beta,p.value,p.value.raw)
      names(res)<-c("model","Outcome","Exposure", "beta","p.value",'p.value.raw')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
brain_function_result <- Result
write.csv(brain_function_result,"diet_brain_function_result_new.csv")

###brain_function_quartile
brain_function<-fread("brain_function_cross.csv")#fread函数快速读取文件
brain_function<-brain_function[,c((2:14),(16:20))]
brain_function<-merge(K_diet_new,brain_function,by="eid")
var1 <- colnames(brain_function)[c(19:35)]
var2 <- paste0(var1,'Z')
var3 <- paste0(var1,'log')
for (i in 1:length(var1)) {
  brain_function[[var2[[i]]]] <- bcPower(brain_function[[var1[[i]]]],
                                         powerTransform(brain_function[[var1[[i]]]],
                                                        family = 'bcPower')$roundlam)
}
var1 <- paste0(var1,'Z')
var2 <- c('as.factor(K24h_diet_Q)')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+BMI+smoke+alchol')
Result <- list()

for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = brain_function)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value2 <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw2 <- x[["coefficients"]][2,4]
      beta2 <- sprintf('%0.3f',x[["coefficients"]][2,1])
      p.value3 <- ifelse(x[["coefficients"]][3,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][3,4]))
      p.value.raw3 <- x[["coefficients"]][3,4]
      beta3 <- sprintf('%0.3f',x[["coefficients"]][3,1])
      p.value4 <- ifelse(x[["coefficients"]][4,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][4,4]))
      p.value.raw4 <- x[["coefficients"]][4,4]
      beta4 <- sprintf('%0.3f',x[["coefficients"]][4,1])
      res<-c(Var3,Var1,Var2, beta2, p.value2,p.value.raw2,beta3, p.value3,p.value.raw3, beta4, p.value4,p.value.raw4)
      names(res)<-c('model',"Outcome","Exposure","beta2", "p.value2",'p.value.raw2',"beta3", "p.value3",'p.value.raw3',"beta4", "p.value4",'p.value.raw4')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
brainfunction_result <- Result
write.csv(brainfunction_result,"diet_brain_function_result_Q_new.csv")

brainfunction_result<-fread("diet_brain_function_result_Q_new.csv")
brainfunction_result$FDR.raw <- p.adjust(brainfunction_result$p.value.raw2,"fdr")
brainfunction_result$FDR <- sprintf('%0.3f',brainfunction_result$FDR.raw)
write.csv(brainfunction_result,"brainfunction_result_diet.csv")#保存合并后csv数据

##brain structure
###皮层体积
stru_volume<-fread("UKB_2_0_aparc_GrayVol.csv")#fread函数快速读取文件
Imagingsite<-fread("gpy.Imagingsite.tsv")#fread函数快速读取文件
K_diet_stru<-merge(K_diet_new,Imagingsite,by="eid")
setnames(K_diet_stru,c("54-2.0"),c("imagingsite"))
TIV<-fread("gpy.TIV.2023.11.23.tsv")#fread函数快速读取文件
TIV<-TIV[,c(1,2)]
K_diet_stru<-merge(K_diet_stru,TIV,by="eid")
setnames(K_diet_stru,c("26521-2.0"),c("TIV"))
setnames(stru_volume,c("SubID"),c("eid"))
K_diet_cor_vol<-merge(K_diet_stru,stru_volume,by="eid")
library(car) 
var1 <- colnames(K_diet_cor_vol)[c(21:88)]
var2 <- paste0(var1,'Z')
for (i in 1:length(var1)) {
  K_diet_cor_vol[[var2[[i]]]] <- bcPower(K_diet_cor_vol[[var1[[i]]]],
                                         powerTransform(K_diet_cor_vol[[var1[[i]]]],
                                                        family = 'bcPower')$roundlam)
}
var1 <- paste0(var1,'Z')
var2 <- c('K24h_diet')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+imagingsite+TIV+BMI+smoke+alchol')
Result <- list()
for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = K_diet_cor_vol)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw <- x[["coefficients"]][2,4]
      beta <- sprintf('%0.3f',x[["coefficients"]][2,1])
      res<-c(Var3,Var1,Var2, beta,p.value,p.value.raw)
      names(res)<-c("model","Outcome","Exposure", "beta","p.value",'p.value.raw')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
stru_volume_result <- Result
write.csv(stru_volume_result,"diet_cor_vol_result_new.csv")#保存合并后csv数据

###皮层体积_四分位数
stru_volume<-fread("UKB_2_0_aparc_GrayVol.csv")#fread函数快速读取文件
Imagingsite<-fread("gpy.Imagingsite.tsv")#fread函数快速读取文件
K_diet_stru<-merge(K_diet_new,Imagingsite,by="eid")
setnames(K_diet_stru,c("54-2.0"),c("imagingsite"))
TIV<-fread("gpy.TIV.2023.11.23.tsv")#fread函数快速读取文件
TIV<-TIV[,c(1,2)]
K_diet_stru<-merge(K_diet_stru,TIV,by="eid")
setnames(K_diet_stru,c("26521-2.0"),c("TIV"))
setnames(stru_volume,c("SubID"),c("eid"))
K_diet_cor_vol<-merge(K_diet_stru,stru_volume,by="eid")
library(car) 
var1 <- colnames(K_diet_cor_vol)[c(21:88)]
var2 <- paste0(var1,'Z')
for (i in 1:length(var1)) {
  K_diet_cor_vol[[var2[[i]]]] <- bcPower(K_diet_cor_vol[[var1[[i]]]],
                                         powerTransform(K_diet_cor_vol[[var1[[i]]]],
                                                        family = 'bcPower')$roundlam)
}
var1 <- paste0(var1,'Z')
var2 <- c('as.factor(K24h_diet_Q)')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+imagingsite+TIV+BMI+smoke+alchol')
Result <- list()
for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = K_diet_cor_vol)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value2 <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw2 <- x[["coefficients"]][2,4]
      beta2 <- sprintf('%0.3f',x[["coefficients"]][2,1])
      p.value3 <- ifelse(x[["coefficients"]][3,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][3,4]))
      p.value.raw3 <- x[["coefficients"]][3,4]
      beta3 <- sprintf('%0.3f',x[["coefficients"]][3,1])
      p.value4 <- ifelse(x[["coefficients"]][4,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][4,4]))
      p.value.raw4 <- x[["coefficients"]][4,4]
      beta4 <- sprintf('%0.3f',x[["coefficients"]][4,1])
      res<-c(Var3,Var1,Var2, beta2, p.value2,p.value.raw2,beta3, p.value3,p.value.raw3, beta4, p.value4,p.value.raw4)
      names(res)<-c('model',"Outcome","Exposure","beta2", "p.value2",'p.value.raw2',"beta3", "p.value3",'p.value.raw3',"beta4", "p.value4",'p.value.raw4')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
stru_volume_result <- Result
write.csv(stru_volume_result,"diet_cor_vol_result_Q_new.csv")#保存合并后csv数据

diet_vol_result_new<-fread("diet_cor_vol_result_Q_new.csv")
diet_vol_result_new$FDR.raw <- p.adjust(diet_vol_result_new$p.value.raw2,"fdr")
diet_vol_result_new$FDR <- sprintf('%0.3f',diet_vol_result_new$FDR.raw)
write.csv(diet_vol_result_new,"diet_vol_result_new.csv")#保存合并后csv数据


#####brain disease 
#COX regression
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

Cox_braindisease<-merge(K_diet_new,Cox_braindisease,by="eid")

Cox_braindisease_male<-Cox_braindisease%>%filter(sex == 1)
Cox_braindisease_female<-Cox_braindisease%>%filter(sex == 0)
Cox_braindisease_young<-Cox_braindisease%>%filter(age < 60)
Cox_braindisease_old<-Cox_braindisease%>%filter(age >= 60)

median (Cox_braindisease$Na24h_diet)
Cox_braindisease_Nalow<-Cox_braindisease%>%filter(Na24h_diet <= 1.829517)
Cox_braindisease_Nahigh<-Cox_braindisease%>%filter(Na24h_diet > 1.829517)

##有序分类变量
Cox_braindisease$K24h_diet_Q<-as.factor(Cox_braindisease$K24h_diet_Q)
variable.names.y <- c('Dementia','PD','Stroke','Epilepsy','MDD','Anxiety','Bipolar','Schizophrenia')
variable.names.x <- colnames(Cox_braindisease_Nahigh)[c(18)]
Result <- list()
for (j in 1:length(variable.names.y)) {
  cox_formula <- sapply(variable.names.x,
                        function(x) as.formula(
                          paste0('Surv(',variable.names.y[[j]],'_days,',variable.names.y[[j]],'_status==1)~',x,'+age+sex+ethnic+education+TDI+BMI+smoke+alchol')))
  cox_model <- lapply(cox_formula, function(x){coxph(x, data = Cox_braindisease_Nahigh)})
  result <- list()
  for (i in 1:length(variable.names.x)) {
    x <- summary(cox_model[[i]])
    Var1 <- variable.names.y[[j]]
    Var2 <- variable.names.x[[i]]
    p.value1 <- sprintf('%0.3f',x[["coefficients"]][1,5])
    p.value2 <- sprintf('%0.3f',x[["coefficients"]][2,5])
    p.value3 <- sprintf('%0.3f',x[["coefficients"]][3,5])
    p.value.raw1 <- x[["coefficients"]][1,5]
    p.value.raw2 <- x[["coefficients"]][2,5]
    p.value.raw3 <- x[["coefficients"]][3,5]
    beta1 <- sprintf('%0.3f',x[["coefficients"]][1,1])
    beta2 <- sprintf('%0.3f',x[["coefficients"]][2,1])
    beta3 <- sprintf('%0.3f',x[["coefficients"]][3,1])
    HR1 <- sprintf('%0.2f',x[["coefficients"]][1,2])
    HR2 <- sprintf('%0.2f',x[["coefficients"]][2,2])
    HR3 <- sprintf('%0.2f',x[["coefficients"]][3,2])
    HR.confint.lower1 <- sprintf('%0.2f',x[["conf.int"]][,"lower .95"][1])
    HR.confint.lower2 <- sprintf('%0.2f',x[["conf.int"]][,"lower .95"][2])
    HR.confint.lower3 <- sprintf('%0.2f',x[["conf.int"]][,"lower .95"][3])
    HR.confint.upper1 <- sprintf('%0.2f',x[["conf.int"]][,"upper .95"][1])
    HR.confint.upper2 <- sprintf('%0.2f',x[["conf.int"]][,"upper .95"][2])
    HR.confint.upper3 <- sprintf('%0.2f',x[["conf.int"]][,"upper .95"][3])
    HR1 <- paste0(HR1, " (",
                  HR.confint.lower1, "-", HR.confint.upper1, ")")
    HR2 <- paste0(HR2, " (",
                  HR.confint.lower2, "-", HR.confint.upper2, ")")
    HR3 <- paste0(HR3, " (",
                  HR.confint.lower3, "-", HR.confint.upper3, ")")
    res<-data.frame('disease'=Var1,
                    'marker'=Var2,
                    'beta1'=beta1,
                    'HR (95% CI)1'=HR1,
                    'p.value1'=p.value1,
                    'p.value.raw1'=p.value.raw1,
                    'beta2'=beta2,
                    'HR (95% CI)2'=HR2,
                    'p.value2'=p.value2,
                    'p.value.raw2'=p.value.raw2,
                    'beta3'=beta3,
                    'HR (95% CI)3'=HR3,
                    'p.value3'=p.value3,
                    'p.value.raw3'=p.value.raw3)
    result <- rbind(result,res)
    rm(res)
  }
  Result <- rbind(Result,result)
}
Cox_result <- as.data.frame(Result)
Cox_result
write.csv(Cox_result,"diet_Cox_result_Q_new_Nahigh.csv")#保存合并后csv数据


#连续变量model1
variable.names.y <- c('Dementia','PD','Stroke','Epilepsy','MDD','Anxiety','Bipolar','Schizophrenia')
variable.names.x <- colnames(Cox_braindisease_Nahigh)[c(15)]
Result <- list()
for (j in 1:length(variable.names.y)) {
  cox_formula <- sapply(variable.names.x,
                        function(x) as.formula(
                          paste0('Surv(',variable.names.y[[j]],'_days,',variable.names.y[[j]],'_status==1)~',x,'+age+sex+ethnic+education+TDI+BMI+smoke+alchol')))
  cox_model <- lapply(cox_formula, function(x){coxph(x, data = Cox_braindisease_Nahigh)})
  result <- list()
  for (i in 1:length(variable.names.x)) {
    x <- summary(cox_model[[i]])
    Var1 <- variable.names.y[[j]]
    Var2 <- variable.names.x[[i]]
    p.value <- sprintf('%0.3f',x[["coefficients"]][1,5])
    p.value.raw <- x[["coefficients"]][1,5]
    beta <- sprintf('%0.3f',x[["coefficients"]][1,1])
    HR <- sprintf('%0.2f',x[["coefficients"]][1,2])
    HR.confint.lower <- sprintf('%0.2f',x[["conf.int"]][,"lower .95"][1])
    HR.confint.upper <- sprintf('%0.2f',x[["conf.int"]][,"upper .95"][1])
    HR <- paste0(HR, " (",
                 HR.confint.lower, "-", HR.confint.upper, ")")
    res<-data.frame('disease'=Var1,
                    'marker'=Var2,
                    'beta'=beta,
                    'HR (95% CI)'=HR,
                    'p.value'=p.value,
                    'p.value.raw'=p.value.raw)
    result <- rbind(result,res)
    rm(res)
  }
  Result <- rbind(Result,result)
}
Cox_result <- as.data.frame(Result)
Cox_result
write.csv(Cox_result,"diet_Cox_result_new_Nahigh.csv")#保存合并后csv数据

Cox_result<-fread("diet_Cox_result_Q_new_Nahigh.csv")
Cox_result$FDR.raw <- p.adjust(Cox_result$p.value.raw1,"fdr")
Cox_result$FDR <- sprintf('%0.3f',Cox_result$FDR.raw)
write.csv(Cox_result,"diet_Cox_result_Q_new_Nahigh.csv")#保存合并后csv数据

###限制性立方样条
install.packages("foreign")
install.packages("rms")
install.packages("Hmisc")
install.packages("splines")  
library(splines)
library(rms)
library(Hmisc)
library(foreign)

Cox_braindisease$sex<-as.factor(Cox_braindisease$sex)
Cox_braindisease$ethnic<-as.factor(Cox_braindisease$ethnic)
Cox_braindisease$education<-as.factor(Cox_braindisease$education)
Cox_braindisease$smoke<-as.factor(Cox_braindisease$smoke)
Cox_braindisease$alchol<-as.factor(Cox_braindisease$alchol)

dd <- datadist(Cox_braindisease)
options(datadist='dd')

fit3<-cph(Surv(PD_days,PD_status) ~ rcs(K24h_diet,3)+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
fit4<-cph(Surv(Dementia_days,Dementia_status) ~ rcs(K24h_diet,4)+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
fit5<-cph(Surv(Dementia_days,Dementia_status) ~ rcs(K24h_diet,5)+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
###然后查看AIC,选择AIC最小的
AIC(fit3)
AIC(fit4)
AIC(fit5)
##查看非线性p值
fit3
an<-anova(fit3)
an
HR<-Predict(fit3, K24h_diet,fun=exp,ref.zero = TRUE)
#进一步美化#anova=an, pval=T：增加卡方值和P值  
dd$limits$K24h_diet[2]<-3.573429

###中介分析，钾和炎症代谢物等的关联
biochemistry<-fread("gpy.biochemistry.tsv.gz")#fread函数快速读取文件
blood<-fread("gpy.blood.tsv.gz")#fread函数快速读取文件
metabolite<-fread("Metabolomics_baseline_raw.tsv.gz")#fread函数快速读取文件
blood<-merge(K_diet_new,blood,by="eid")
biochemistry<-merge(K_diet_new,biochemistry,by="eid")
metabolite<-merge(K_diet_new,metabolite,by="eid")
metabolite<-metabolite[,c((1:9),(21:274))]

colnames(blood) <- str_replace(colnames(blood),pattern = "-",replacement = "_")
var1 <- colnames(blood)[c(19:49)]
var2 <- paste0('X',var1)
for (i in 1:length(var1)) {
  blood[[var2[[i]]]] <- (blood[[var1[[i]]]]+0.1)}

blood<-blood[,c((1:18),(50:80))]

var1 <- colnames(blood)[c(19:49)]
var2 <- paste0(var1,'Z')
for (i in 1:length(var1)) {
  blood[[var2[[i]]]] <- bcPower(blood[[var1[[i]]]], 
                                powerTransform(blood[[var1[[i]]]], 
                                               family = 'bcPower')$roundlam)
}
var1 <- paste0(var1,'Z')
var2 <- c('K24h_diet')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+smoke+alchol+BMI')
Result <- list()

for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = blood)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw <- x[["coefficients"]][2,4]
      beta <- sprintf('%0.3f',x[["coefficients"]][2,1])
      res<-c(Var3,Var1,Var2, beta,p.value,p.value.raw)
      names(res)<-c("model","Outcome","Exposure", "beta","p.value",'p.value.raw')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
blood_result <- Result

###biochemistry
colnames(biochemistry) <- str_replace(colnames(biochemistry),pattern = "-",replacement = "_")

var1 <- colnames(biochemistry)[c(19:48)]
var2 <- paste0('X',var1)
for (i in 1:length(var1)) {
  biochemistry[[var2[[i]]]] <- bcPower(biochemistry[[var1[[i]]]], 
                                       powerTransform(biochemistry[[var1[[i]]]], 
                                                      family = 'bcPower')$roundlam)
}
var1 <- paste0('X',var1)
var2 <- c('K24h_diet')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+smoke+alchol+BMI')
Result <- list()

for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = biochemistry)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw <- x[["coefficients"]][2,4]
      beta <- sprintf('%0.3f',x[["coefficients"]][2,1])
      res<-c(Var3,Var1,Var2, beta,p.value,p.value.raw)
      names(res)<-c("model","Outcome","Exposure", "beta","p.value",'p.value.raw')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
biochemistry_result <- Result


###metabolites
var1 <- colnames(metabolite)[c(19:269)]
var2 <- paste0(var1,'log')
for (i in 1:length(var1)) {
  metabolite[[var2[[i]]]] <- log10(metabolite[[var1[[i]]]]+1)
}

var1 <- paste0(var1,'log')
var2 <- c('K24h_diet')##Exposure
var3 <- c('+age+sex+education+ethnic+TDI+smoke+alchol+BMI')
Result <- list()

for (i in 1:length(var2)) {
  for (j in 1:length(var1)) {
    result <- list()
    for (k in 1:length(var3)) {
      lm_formula <- paste0('scale(',var1[[j]],') ~ ',var2[[i]],var3[[k]])
      lm_model <- lm(lm_formula,data = metabolite)
      x <- summary(lm_model)
      Var1 <- var1[[j]]
      Var2 <- var2[[i]]
      Var3 <- k
      p.value <- ifelse(x[["coefficients"]][2,4]<0.001,'<0.001',sprintf('%0.3f',x[["coefficients"]][2,4]))
      p.value.raw <- x[["coefficients"]][2,4]
      beta <- sprintf('%0.3f',x[["coefficients"]][2,1])
      res<-c(Var3,Var1,Var2, beta,p.value,p.value.raw)
      names(res)<-c("model","Outcome","Exposure", "beta","p.value",'p.value.raw')
      res <- as.data.frame(t(as.data.frame(res)))
      result <- rbind(result,res)
      rm(res)
      print(j)
    }
    Result <- rbind(Result,result)
  }
}
metabolite_result <- Result

diet_peripheral_result<-rbind(blood_result,biochemistry_result)
diet_peripheral_result<-rbind(diet_peripheral_result,metabolite_result)

write.csv(diet_peripheral_result,"diet_peripheral_result.csv")

diet_peripheral_result<-fread("diet_peripheral_result.csv")
dictionary<-fread("dictionary.csv")
diet_peripheral_result<-merge(diet_peripheral_result,dictionary,by="Outcome")

diet_peripheral_result$bof.raw <- p.adjust(diet_peripheral_result$p.value.raw,"bonferroni")
diet_peripheral_result$bof <- sprintf('%0.3f',diet_peripheral_result$bof.raw)
write.csv(diet_peripheral_result,"diet_peripheral_result.csv")#保存合并后csv数据


###筛选同时相关
k_sig<-fread("diet_peripheral_result.csv")#fread函数快速读取文件
disease_sig<-fread("last_disease.csv")#fread函数快速读取文件
diet_sig_simul<-merge(k_sig,disease_sig,by="Outcome")
write.csv(diet_sig_simul,"diet_sig_simul.csv")#保存合并后csv数据

###整理sem文件
Stroke<-fread("Stroke.csv")#fread函数快速读取文件
PD<-fread("PD.csv")#fread函数快速读取文件
Dementia<-fread("Dementia.csv")#fread函数快速读取文件
Epilepsy<-fread("Epilepsy.csv")#fread函数快速读取文件
MDD<-fread("MDD.csv")#fread函数快速读取文件
Schizophrenia<-fread("Schizophrenia.csv")#fread函数快速读取文件
Anxiety<-fread("Anxiety.csv")#fread函数快速读取文件
Bipolar<-fread("Bipolar.csv")#fread函数快速读取文件
Cox_braindisease<-merge(Dementia,Stroke,by="eid")
Cox_braindisease<-merge(Cox_braindisease,PD,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Epilepsy,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Schizophrenia,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Bipolar,by="eid")
Cox_braindisease<-merge(Cox_braindisease,Anxiety,by="eid")
Cox_braindisease<-merge(Cox_braindisease,MDD,by="eid")
##去掉基线患有脑疾病的
Cox_braindisease<-Cox_braindisease%>%filter(Dementia_days>0&PD_days>0&Stroke_days>0&Epilepsy_days>0&Schizophrenia_days>0&MDD_days>0&Anxiety_days>0&Bipolar_days>0)
K_diet_new<-fread("K_diet_new.csv")#fread函数快速读取文件
Cox_braindisease<-merge(K_diet_new,Cox_braindisease,by="eid")
Cox_braindisease<-Cox_braindisease[,c(1,(3:10),16,19,(20:35))]
Cox_braindisease<-na.omit(Cox_braindisease)

biochemistry<-fread("gpy.biochemistry.tsv.gz")#fread函数快速读取文件
metabolite<-fread("Metabolomics_baseline_raw.tsv.gz")#fread函数快速读取文件
blood<-fread("gpy.blood.tsv.gz")#fread函数快速读取文件
biochemistry1<-grep("eid|30710|30780|30690",colnames(biochemistry), value = TRUE)
biochemistry1<-biochemistry[,..biochemistry1]
biochemistry1<-na.omit(biochemistry1)
blood1<-grep("eid|30200",colnames(blood), value = TRUE)
blood1<-blood[,..blood1]
blood1<-na.omit(blood1)
metabolite1<-grep("eid|23401|23404|23405|23413|23417|23421|23425|23437|23505|23531|23532|23533|23534|23535|23542|23549|23576",colnames(metabolite), value = TRUE)
metabolite1<-metabolite[,..metabolite1]
metabolite1<-na.omit(metabolite1)

colnames(biochemistry1) <- str_replace(colnames(biochemistry1),pattern = "-",replacement = "_")
var1 <- colnames(biochemistry1)[c(2:4)]
var2 <- paste0("X",var1)
for (i in 1:length(var1)) {
  biochemistry1[[var2[[i]]]] <- (biochemistry1[[var1[[i]]]])
}
biochemistry1<-biochemistry1[,c(1,(5:7))]#提取这个文件中的这些列
colnames(blood1) <- str_replace(colnames(blood1),pattern = "-",replacement = "_")
var1 <- colnames(blood1)[c(2)]
var2 <- paste0("X",var1)
for (i in 1:length(var1)) {
  blood1[[var2[[i]]]] <- (blood1[[var1[[i]]]])
}
blood1<-blood1[,c(1,(3))]#提取这个文件中的这些列
sem<-merge(Cox_braindisease,biochemistry1,by="eid")
sem<-merge(sem,blood1,by="eid")
sem<-merge(sem,metabolite1,by="eid")

library(car)
var1 <- colnames(sem)[c(32:48)]
var3 <- paste0(var1,'')
for (i in 1:length(var1)) {
  sem[[var3[[i]]]] <- log10(sem[[var1[[i]]]]+0.1)
}

var1 <- colnames(sem)[c(29:39)]
var2 <- paste0('',var1)
for (i in 1:length(var1)) {
  sem[[var2[[i]]]] <- scale(sem[[var1[[i]]]])
}

var1 <- colnames(sem)[c(32:48)]
var3 <- paste0(var1,'')
for (i in 1:length(var1)) {
  sem[[var3[[i]]]] <- (sem[[var1[[i]]]]+0.1)
}
var1 <- colnames(sem)[c(31)]
var2 <- paste0(var1,'')
for (i in 1:length(var1)) {
  sem[[var2[[i]]]] <- bcPower(sem[[var1[[i]]]],
                              powerTransform(sem[[var1[[i]]]],
                                             family = 'bcPower')$roundlam)}

var1 <- colnames(sem)[c(31)]
var2 <- paste0('',var1)
for (i in 1:length(var1)) {
  sem[[var2[[i]]]] <- scale(sem[[var1[[i]]]])
}

sem<-sem%>%mutate(Anxiety_years=Anxiety_days/365)
sem<-sem%>%mutate(MDD_years=MDD_days/365)
sem<-sem%>%mutate(PD_years=PD_days/365)
sem<-sem%>%mutate(Dementia_years=Dementia_days/365)
sem<-sem%>%mutate(Stroke_years=Stroke_days/365)

write.csv(sem,"diet_k_sem.csv")

infla<-grep("30690|30780|23401|23404|23405|23413|23417|23421|23425|23437|23505|23531|23532|23533|23534|23535|23542|23549|23576",colnames(sem), value = TRUE)
infla<-sem[,..infla]
res <- cor(infla)
round(res,2)

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
