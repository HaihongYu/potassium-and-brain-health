###以饮食钾分析为例
library(R.utils)
library(data.table)
library(tidyverse)
library(car) 
library("survival")

diet<-fread("dietary_Na_K.csv")#fread函数快速读取文件

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


##brain_function
brain_function<-fread("brain_function_cross.csv")#fread函数快速读取文件
brain_function<-merge(K_diet,brain_function,by="eid")

lm_formula <- lm(cognitive ~ diet_k+age+sex+education+ethnic+TDI+BMI+smoke+alchol,data = brain_functionn)
x <- summary(lm_model)

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
lm_formula <- lm(cognitive ~ diet_k+age+sex+education+ethnic+TDI+BMI+smoke+alchol+imagingsite+TIV,data = brain_functionn)
x <- summary(lm_model)

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
fit3<-cph(Surv(PD_days,PD_status) ~K24h_diet_Q+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
summary(fit3)
                    
连续变量model1
fit3<-cph(Surv(PD_days,PD_status) ~K24h_diet+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
summary(fit3)

###限制性立方样条
install.packages("foreign")
install.packages("rms")
install.packages("Hmisc")
install.packages("splines")  
library(splines)
library(rms)
library(Hmisc)
library(foreign)

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

lm_formula <- lm(biochemistry ~ diet_k+age+sex+education+ethnic+TDI+BMI+smoke+alchol+imagingsite+TIV,data = brain_functionn)
x <- summary(lm_model)

lm_formula <- lm(bloodcount ~ diet_k+age+sex+education+ethnic+TDI+BMI+smoke+alchol+imagingsite+TIV,data = brain_functionn)
x <- summary(lm_model)

lm_formula <- lm(metabolit ~ diet_k+age+sex+education+ethnic+TDI+BMI+smoke+alchol+imagingsite+TIV,data = brain_functionn)
x <- summary(lm_model)

cox_formula<-cph(Surv(PD_days,PD_status) ~biochemistry+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
y<-summary(cox_formula)


cox_formula<-cph(Surv(PD_days,PD_status) ~bloodcount+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
y<-summary(cox_formula)


cox_formula<-cph(Surv(PD_days,PD_status) ~metabolites+age+sex+ethnic+education+TDI+BMI+smoke+alchol,data=Cox_braindisease)
y<-summary(cox_formula)


###筛选同时相关,共线性分析，确定纳入sem的标志物

infla<-grep("30690|30780|23401|23404|23405|23413|23417|23421|23425|23437|23505|23531|23532|23533|23534|23535|23542|23549|23576",colnames(sem), value = TRUE)
infla<-sem[,..infla]
res <- cor(infla)
round(res,2)

###结构方程模型
library(lavaan)

sem.model1<-'
         K=~1*K24h
         inflammation=~1*X30200_0.0+X30140_0.0+X30180_0.0
         metabolism=~1*X30630_0.0+X23566+X23567
         status=~1*Dementia_status+Stroke_status+PD_status+Anxiety_status+MDD_status
         
         status~K+inflammation+metabolism
         inflammation~K
         metabolism~K
        
        metabolism~~inflammation
        '
fit<-sem(sem.model1,data=sem_new,se="boot",bootstrap=10000,estimator="ML")
summary(fit,fit.measures=T,standardized=TRUE)
fitMeasures(fit,c("chisq","df","pvalue","gfi","cfi","rmr","srmr","rmsea"))
