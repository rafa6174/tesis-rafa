library(tidyverse)

set.seed(2)
x<-rnorm(50,1/2,1)
set.seed(3)
y<-rnorm(50,1/2,1)

G1<-rbind(x,y)
G1<-rbind(G1,rep(1,50))

set.seed(15)
x<-rnorm(55,3.5,1)
set.seed(16)
y<-rnorm(55,3,1)

G2<-rbind(x,y)
G2<-rbind(G2,rep(2,55))
datos<-cbind(G1,G2)
datos<-t(datos)
datos<-as.data.frame(datos)
colnames(datos)<-c("Var1","Var2","Grupo")
datos$Grupo<-as.factor(datos$Grupo)

datos

mu1<-rbind(mean(G1[1,]),mean(G1[2,]))
mu2<-rbind(mean(G2[1,]),mean(G2[2,]))

sigma1<-matrix(c(cov(G1[1,],G1[1,]),cov(G1[1,],G1[2,]),cov(G1[2,],G1[1,]),cov(G1[2,],G1[2,])),ncol = 2)
sigma2<-matrix(c(cov(G2[1,],G2[1,]),cov(G2[1,],G2[2,]),cov(G2[2,],G2[1,]),cov(G2[2,],G2[2,])),ncol = 2)
sigma=(sigma1+sigma2)/2
Sinv<-solve(sigma)
b<-Sinv*(mu1-mu2)
mu1<-as.matrix(mu1)
mu2<-as.matrix(mu2)
b<-Sinv%*%(mu1-mu2)
muc<-(mu1+mu2)/2

datos |>
  ggplot()+
  geom_blank()+
  geom_point(aes(x =Var1, y =Var2,
                 shape = Grupo, color = Grupo, fill =Grupo))+
  geom_abline(slope =-0.9159786, intercept = 4.149884, color = "blue", linewidth = 1)
