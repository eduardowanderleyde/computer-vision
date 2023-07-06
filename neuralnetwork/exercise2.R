
breast_cancer_csv = as.data.frame(breast_cancer_csv)
breast_cancer_csv$class = as.factor(breast_cancer_csv$class)

#conjunto de treino
ind_train = sample(286,200)
ind_test = setdiff((1:286),ind_train)

dados_train = breast_cancer_csv[ind_train,]
dados_test = breast_cancer_csv[ind_test,] 

listageral = cbind()
for (i in 1:30) {
  listamesmoneuronio <- cbind()
for (t in 1:10){
  nn_train = nnet(class~., data = dados_train, size = i, maxit = 1000)
  preds_teste = predict(nn_train,dados_test,type = "class")
  matrix_conf_test = table(preds_teste, dados_test$class)
  acertos_teste = diag(as.matrix(matrix_conf_test))
  acc_teste = sum(acertos_teste)/length(preds_teste)
  listamesmoneuronio = cbind(listamesmoneuronio,acc_teste)
  
}
 listageral = cbind(listageral,mean(listamesmoneuronio))

}
