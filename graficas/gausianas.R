library(ggplot2)
set.seed(123)  # Para reproducibilidad
datos1 <- data.frame(x = rnorm(1000, mean = 0, sd = 1))
set.seed(124)
datos2 <- data.frame(x = rnorm(1000, mean = 5, sd = 1))
datos1$grupo <- "Grupo 1"
datos2$grupo <- "Grupo 2"
datos <- rbind(datos1, datos2)

ggplot(datos, aes(x = x, fill = grupo)) +
  geom_blank()+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), col = "cyan", size = 0.5) +
  stat_function(fun = dnorm, args = list(mean = 5, sd = 1), col = "magenta", size = 0.5) +
  labs(title = "", x = "Función discriminante lineal", y = "") +
  scale_fill_manual(values = c("Grupo 1" = "cyan", "Grupo 2" = "magenta")) +
  geom_vline(xintercept = 2.5, linetype = "dashed", color = "black", size = 0.5)+
  annotate("text", x = 0, y = 0.1, label = "Grupo 1", color = "cyan", size = 5, vjust = -1) +
  annotate("text", x = 5, y = 0.1, label = "Grupo 2", color = "magenta", size = 5, vjust = -1)+
  theme_minimal()



