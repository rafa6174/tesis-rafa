library(tidyverse)
library(ggforce)

set.seed(1)
x<-rnorm(50,1/2,1)
set.seed(2)
y<-rnorm(50,1/2,1)

G1<-rbind(x,y)
G1<-rbind(G1,rep(1,50))

set.seed(15)
x<-rnorm(55,3.5,1)
set.seed(14)
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
b<-Sinv%*%(mu1-mu2)
mu1<-as.matrix(mu1)
mu2<-as.matrix(mu2)
b<-Sinv%*%(mu1-mu2)
muc<-(mu1+mu2)/2

datos_circulo <- data.frame(
  Var1 = 2,
  Var2 = 2,
  r = 0.5
)

punto<-data.frame(Var1=2,Var2=2)
radio<-0.5

datos |>
  ggplot()+
  geom_blank()+
  geom_point(aes(x =Var1, y =Var2,
                 shape = Grupo, color = Grupo, fill =Grupo))+
  geom_point(data = punto, aes(x = Var1, y = Var2), shape = 18, size = 4, color = "purple")+
  geom_circle(data = datos_circulo, aes(x0 = 2, y0 = 2, r = 1))
  
  
