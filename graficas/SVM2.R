library(tidyverse)
library(e1071)

set.seed(10)
n <- 25  # número de puntos por clase
x1 <- matrix(rnorm(n * 2), ncol = 2)
set.seed(11)
x2 <- matrix(rnorm(n * 2) + 3, ncol = 2)
y <- c(rep(-1, n), rep(1, n))
datos <- data.frame(x = rbind(x1, x2), y = as.factor(y))
colnames(datos) <- c("x1", "x2", "Grupo")

modelo_svm <- svm(Grupo ~ ., data = datos, kernel = "linear", cost = 10, scale = FALSE)

w <- t(modelo_svm$coefs) %*% modelo_svm$SV
b <- -modelo_svm$rho

# Hiperplano de separación
hiperplano <- function(x1, w, b) {
  -(-b - w[1] * x1) 
}

# Márgenes basados en los vectores de soporte
margen1 <- function(x1, w, b, margin) {
  -(-b - w[1] * x1 + margin) 
}

margen2 <- function(x1, w, b, margin) {
  -(-b - w[1] * x1 - margin) 
}

# Calcula el margen basado en los vectores de soporte
margin <- 0.93


ggplot(datos, aes(x = x1, y = x2, color = Grupo)) +
  geom_point(size = 2) +
  stat_function(fun = hiperplano, args = list(w = w, b = b), color = "blue", size = 1) +
  stat_function(fun = margen1, args = list(w = w, b = b, margin = margin), color = "red", linetype = "dashed") +
  stat_function(fun = margen2, args = list(w = w, b = b, margin = margin), color = "red", linetype = "dashed") +
  labs(title = "Support Vector Machine: Plano de Separación y Márgenes",
       x = "X1",
       y = "X2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

