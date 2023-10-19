summary(dfProduto$semana0)
dfSemana0<-data.frame("Produto"=dfProduto$semana0, "Preco"=dfFonecedor$precoFonecedor0)

#==============================Gráficos================================================= 
hist(dfSemana0$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
