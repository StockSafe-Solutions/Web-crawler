#===============================Importação============================
library(jsonlite)


summary(dfProduto$semana0)
dfSemana0<-data.frame("Produto"=dfProduto$semana0, "Preco"=dfFornecedor$precoFonecedor0)

#==============================Gráficos================================================= 
hist(dfSemana0$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
hist(dfSemana0$Preco, col="#31572C", main="Preço do produto", ylab = "Frequência", xlab = "Valores")




#==============================================Exportação de dataset===================================
# Exporte o dataset para um JSON
json_data <- toJSON(dfSemana0)
caminho<-"~/Downloads/RGrupo/Web-crawler/analiseDados/teste.json"
write_json(json_data, path=caminho)




#============================Conexão COM O BANCO DE DADOS=====================================
# #Conecte-se ao banco de dados MySQL
# con <- dbConnect(drv = MariaDB(),
#                  user = "aluno",
#                  password = "sptech",
#                  host = "localhost",
#                  dbname = "dadosDia")
# 
# # Carregue o dadosDia no banco de dados
# dbWriteTable(con, "dadosDia", dadosDia, overwrite = TRUE)
# 
# # Desconecte-se do banco de dados MySQL
# dbDisconnect(con)