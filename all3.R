setwd('./logit')

#导入数据
bino<-read.csv("./logit/all-bino.csv")      
summary(bino)
for (i in c(1:nrow(bino)) )
  if (bino$x6F[i]>1){bino$x6F[i] = 0}else{bino$x6F[i] <- 1} 

#被解释变量设为factor
bino$x6F <- factor(bino$x6F)

str(bino)   
#查看变量类型

bino.train <- bino[1:2000,]
bino.try <- bino[2001:4000,]

bino.go <- glm(x6F~.,data=bino.train, family = binomial(link="logit"))      
#建立逻辑回归模型
summary(bino.go)    #查看模型

#筛选变量重做logit
bino.go2 <- glm(x6F~x1G_N+x1DA_N+x4D+x5I+x6D+x6E+x7B+x7GN,data=bino.train, family = binomial(link="logit"))  
#查看模型
summary(bino.go2)    
#构建混淆矩阵
bino.predict <- predict(bino.go2,bino.train,type="response")
bino.class = ifelse(bino.predict>0.4,1,0)
table(bino.class,bino.train[, 28])

###用probit试试看
bino.go3 <- glm(x6F~x1G_N+x4D+x6D+x6E+x7B+x7GN,data=bino.train, family = binomial(link="probit"))
summary(bino.go3)    
#构建混淆矩阵
bino.predict3 <- predict(bino.go3,bino.train,type="response")
bino.class3 = ifelse(bino.predict3>0.8,1,0)
table(bino.class3,bino.train[, 28])

###guji
bino.go3 <- glm(x6F~x1G_N+x4D+x6D+x6E+x7B+x7GN,data=bino.try, family = binomial(link="probit"))
summary(bino.go3)    
#构建混淆矩阵
bino.predict3 <- predict(bino.go3,bino.try,type="response")
bino.class3 = ifelse(bino.predict3>0.5,1,0)
table(bino.class3,bino.train[, 28])

newlogit.predict <- predict(newlogit,origin,type="response")
newlogit.class = ifelse(newlogit.predict>0.6,1,0)
table(newlogit.class,origin[, 28])   #构建混淆矩阵

#逐步回归
bino.step <- step(bino.go3)
summary(bino.step)
binostep.predict <- predict(bino.step,bino.try,type="response")         
#对训练集进行预测目标变量
data.class4=ifelse(binostep.predict>0.5,1,0)            
#预测结果分类，type="response"直接返回预测的概率值0~1之间
table(data.class4,bino.try[, 28])   #构建混淆矩阵


####多项
install.packages("mlogit")

library(Formula)
library(maxLik)
library(miscTools)
library(mlogit)

origin2 <- read.csv("./logit/all.csv")
origin2_1 <- origin2[1:2000,1:40]

str(origin2_1)

new2 <- mlogit.data(origin2_1, shape = 'long', alt.var ='x', alt.levels = 1:5)
new3 <- cbind(new2,x)
m_new2 <- mlogit(x1G_N ~ x1DA_N+x1H+x2A+x2B+x2F+x2G+x4A+x5A+x5C+x5D+x5G+x5I 
                 , data = origin2_1)

#x1B+x1CA+x1CB+x1DA_N+x1H+x2A+x2B+x2F+x2G+x4A+x5A+x5C+x5D+x5G+x5I          
#x1B|x1CA|x1CB|x1DA_N|x1H|x2A|x2B|x2F|x2G|x4A|x5A|x5C|x5D|x5G|x5I       ,data = new2     alt.levels = 1:5

summary(new2)
new2.predict <- predict(m_new2, new2, type="response")  
#summary(new2.predict)
table(new2.predict,new2[,6])   #构建混淆矩阵



####有序
# 建立数据文件
setwd('./logit')

xb=c(1,1,1,1,1,1,0,0,0,0,0,0)
lf=c(1,1,1,0,0,0,1,1,1,0,0,0)
lx=c(1,2,3,1,2,3,1,2,3,1,2,3)
ps=c(16,5,6,6,7,19,5,2,7,1,0,10)
table<-data.frame(xb,lf,lx,ps)  #合并成数据框

#导入数据
#载入MASS程序包，使用函数polr需要载入MASS包
#logistic回归模型中回归系数的最大似然估计及模型拟合检验可以采用统计软件R语言中的polr函数
#install.packages("MASS")    #安装MASS程序包
library(MASS)       #载入MASS程序包

origin <- read.csv("./logit/all-bino.csv")
origin1 <- origin[,1:20]
table <- origin1[1:200, 1:20]
str(table)
#table <- na.omit(table)

#logistic回归分析
fit <- polr(as.ordered(x2G)~., Hess=T, data=table) #polr见下方解释，更为详尽的polr介绍见附件文档
# MASS包，polr函数，可以做定序Logistic回归。 
#model1=polr(as.factor(y)~x1+x2+x3,method='losigtic',Hess=T)
#as.ordered(lx)~~xb+lf  为预测回归模型表达式
#weight=ps 指以ps为衡量指标参数，一般默认为1
#Hess=T 说明应当返回Hessian（观察到的信息矩阵）的逻辑，并打算在拟合上调用summary或vcov函数。
#data指可选的数据框为table（上述合并的表格），用于解释公式中出现的变量。
summary(fit)  #输出回归系数

fitpre <- predict(fit, data = table[1:200,-12])
a <- table(fitpre, table[1:200, 12])
a
#模拟拟合检验
fit1<-polr(as.ordered(lx)~1,data=table)  #统计量模型1
fit2<-polr(as.ordered(lx)~xb+lf,data=table)    #统计量模型2
anova(fit1,fit2)   #检验统计量为上述两个模型deviance之差


######无序的多项logit


library('nnet')

m<-read.csv("./logit/all-bino.csv")
m$x2G <- factor(m$x2G)
m.train <- m[1:2000,1:20]
m.try <- m[2001,4000,1:20]

m.go <- multinom(x2G~., Hess=T, data=m.train)
for (i in 1:20)
  {mult<-update(m.go,~.-m.train(i))
  x <- anova(m.go,mult)
  print(x)}
m.pre <- predict(m.go, data = m.train[,-12], type = 'class')
table(m.pre, m.train[,12])
summary(m.go)


######youxv
library('MASS')

o<-read.csv("./logit/all-bino.csv")
o$x2G <- factor(o$x2G)
o.train <- o[1:2000,1:20]
o.try <- o[2001,4000,1:20]

o.go <- polr(as.ordered(x2G)~., Hess=T, data=m.train)
o.pre <- predict(o.go, data = o.train[,-12], type = 'class')
table(o.pre, o.train[,12])
summary(o.pre)
