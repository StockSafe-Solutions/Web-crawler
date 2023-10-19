#===============================Importação============================
library(jsonlite)




summary(dfProduto$semana0)
#dfFornecedor$precoFonecedor0 = dfFornecedor$precoFonecedor0/100
dfSemana0<-data.frame("Produto"=dfProduto$semana0, "Preco"=round(dfFornecedor$precoFonecedor0))

#==============================Gráficos================================================= 
hist(dfSemana0$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
hist(dfSemana0$Preco, col="#31572C", main="Preço do produto", ylab = "Frequência", xlab = "Valores")




#==============================================Exportação de dataset===================================
# Exporte o dataset para um JSON
json_data <- toJSON(dfSemana0)
caminho<-"~/Downloads/RGrupo/Web-crawler/analiseDados/teste.json"
write_json(json_data, path=caminho)

