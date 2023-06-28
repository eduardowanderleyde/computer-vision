X <- as.matrix(diabetes[, 1:8])
Y <- diabetes$class
Y <- as.factor(Y)
Y <- as.numeric(Y) - 1

T <- 20
alpha <- 0.2

models <- list()
errors <- list()

for (restart in 1:20) {
  modelo <- perceptron(X, Y, T, alpha)
  wt <- modelo$modelo
  
  N <- nrow(X)
  preds <- vector()
  
  for (i in 1:10) {
    x <- c(X[i, ], -1)
    s <- t(wt) %*% x
    fj <- 1 / (1 + exp(-s))
    preds <- c(preds, ifelse(fj >= 0.5, 1, 0))
  }
  
  error <- sum(preds != Y) / N
  models[[restart]] <- wt
  errors[[restart]] <- error
  
  cat("Reinicialização:", restart, " - Erro:", error, "\n")
}

# Encontre a melhor reinicialização (menor erro)
best_index <- which.min(errors)
best_model <- models[[best_index]]
best_error <- errors[[best_index]]

# Visualizar as previsões
cat("Previsões:", preds, "\n")
cat("Melhor modelo (reinicialização", best_index, "):", best_model, "\n")
cat("Erro do melhor modelo:", best_error, "\n")


#Resultado do código rodado

#Previsões: 0 0 1 0 1 0 0 1 1 0 
#> cat("Melhor modelo (reinicialização", best_index, "):", best_model, "\n")
#Melhor modelo (reinicialização 3 ): 238.9969 11.43141 -71.69301 -53.83062 28.79127 10.08737 24.79444 -39.82843 116.1938 
#> cat("Erro do melhor modelo:", best_error, "\n")
#Erro do melhor modelo: 0.4557292 
