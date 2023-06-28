X <- as.matrix(diabetes[, 1:8])
Y <- diabetes$class
Y <- as.factor(Y)
Y <- as.numeric(Y) - 1

T <- 2000
alpha <- 0.2

modelo <- perceptron(X, Y, T, alpha)
wt <- modelo$modelo

N <- nrow(X)
preds <- vector()

for (i in 1:20) {
  x <- c(X[i, ], -1)
  s <- t(wt) %*% x
  fj <- 1 / (1 + exp(-s))
  preds <- c(preds, ifelse(fj >= 0.5, 1, 0))
}

# Visualizar as previsões
preds

