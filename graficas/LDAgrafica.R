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
datos$grupo<-as.factor(datos$grupo)

datos |>
  ggplot()+
  geom_blank()+
  geom_point(aes(x =Var1, y =Var2,
                 shape = Grupo, color = Grupo, fill =Grupo))+
  geom_abline(slope =-1.1882597, intercept = 3.2, color = "blue", linewidth = 1)
