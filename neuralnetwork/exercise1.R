# Converter para matrizes
X <- as.matrix(diabetes[, 1:8])
Y <- diabetes$class
Y <- as.factor(Y)
Y <- as.numeric(Y) - 1

T <- 100
alpha <- 0.001

models <- list()
errors <- cbind()


for (t in 1:20) {

  modelo <- perceptron(X, Y, T, alpha)
  models[[t]] <- modelo$modelo
  errors = cbind(errors,modelo$sse)
}

best_index <- which.min(errors)
best_model <- models[[best_index]]
best_error <- errors[[best_index]]

#  o indice, o melhor modelo, o menor erro
print(best_index)
print(best_model)
print(best_error)

#conferindo
print(errors[15]==min(errors))