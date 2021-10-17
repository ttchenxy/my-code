library(lattice)
xyplot(Petal.Length~Petal.Width,data = iris,
       groups = Species,
       auto.key = list(corner=c(1,0)))

data(iris)
attach(iris)
subdata<- iris[iris$Species!='virginica',]
subdata$Species<- factor(subdata$Species)
model1<- svm (Species~Petal.Length+Petal.Width,
              data= subdata)

plot(model1,subdata,Petal.Length~Petal.Width)

model2 <- svm(Species~., data = iris)
summary(model2)

model2 <- svm(Species~., data = iris)
summary(model2)

svm(x, y = NULL, scale = TRUE, type = NULL, kernel = "radial",  
    degree = 3, gamma = if (is.vector(x)) 1 else 1 / ncol(x),  
    coef0 = 0, cost = 1, nu = 0.5, subset, na.action = na.omit) 

# 提取iris数据集中除第五列以外的数据作为特征变量

x<- iris[, -5] 

# 提取iris数据集中第五列数据作为结果变量

y <- iris[, 5]

model3 <- svm(x, y, kernel = "radial", 
              gamma = if (is.vector(x)) 1 else 1 / ncol(x))
pred <- predict(model3, x)
table(pred, y)
pred <- predict(model3, x, decision.values = TRUE)
attr(pred, "decision.values")[1:4, ]

plot(cmdscale(dist(iris[,-5])),
     col=c("orange","blue","green")[as.integer(iris[,5])],
     pch=c("o","+")[1:150 %in% model3$index+1])
legend(1.8,-0.5,c("setosa","versicolor","virgincia"),
       col=c("orange","blue","green"),lty=1,
       cex=0.6,
       bty="o",box.lty=1,box.col="black")

###############################

x<-c(runif(50,0,1),runif(100,1,3),runif(50,3,4))
y<-runif(200,0,1)
z<-c(rep(0,50),rep(1,100),rep(0,50))
data<-cbind(x,y,z)
plot(x,y,col=c(rep('red',50),rep('blue',100),rep('red',50)))

svm.fit<-svm(z~x+y,data=data)
summary(svm.fit)
predict(svm.fit)
svm.pre<-ifelse(predict(svm.fit)>0,1,0)
n<-ifelse(svm.pre==z,1,0)
sum(n)
col<-ifelse(svm.pre==0,'red','blue')
plot(x,y,col=col)

############线性核函数###########

svm.line<-svm(z~x+y,data=data,kernel='linear')
svm.linepre<-ifelse(predict(svm.line)>0,1,0)
n<-ifelse(svm.linepre==z,1,0)
sum(n)
col<-ifelse(svm.linepre==0,'red','blue')
plot(x,y,col=col)

################穷举法###########

a<-c()
for(i in 1:10){
  for(j in 1:10){
    for(k in 1:10){
      svm.fit<-svm(z~x+y,data=data,degree=i,cost=j,gamma=k)
      svm.pre<-ifelse(predict(svm.fit)>0,1,0)
      n<-ifelse(svm.pre==z,1,0)
      result<-c(i,j,k,sum(n))
      a<-rbind(a,result)
    }}}
a[which(a[,4]==max(a[,4])),]

