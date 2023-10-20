#----Importação----
library(jsonlite)
library(dplyr)
library(ggplot2)
#library(esquisse)
#library(RMariaDB)





dfSemana0<-data.frame("Produto"=dfProduto$semana0, "Preco"=dfFornecedor$precoFonecedor0)

#----Amostras----
amostraProduto<-sample(dfSemana0$Produto, size = 50, replace = TRUE)
#amostraPreco<-sample(dfSemana0$Preco, size = 100, replace = TRUE)*100
amostraPreco<-sample(dfSemana0$Preco, size = 50, replace = TRUE)

#----Sumarios----
summary(dfProduto$semana0)
summary(amostraPreco)
summary(amostraProduto)

#----Gráficos----
hist(dfSemana0$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
hist(dfSemana0$Preco, col="#31572C", main="Preço do produto", ylab = "Frequência", xlab = "Valores")


plot(amostraProduto~amostraPreco)
coeficiente<-lm(amostraProduto~amostraPreco)
abline(coeficiente)

retas <- ggplot(mapping = aes(amostraProduto,amostraPreco)) +
   geom_point() +
   geom_smooth(se = FALSE, method = "lm") +
   geom_hline(yintercept = mean(amostraPreco))

regressao<-retas +
  ggtitle("Gráfico de dispersão da quantidade de Produto vendidos e os preços") +
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_segment(aes(x = amostraProduto, y = amostraPreco,
                   xend = amostraProduto, yend = mean(amostraPreco)), color="black", size=.9) +
  geom_point(color="#a31621", size=2) +
  geom_smooth(se = FALSE, method = "lm", color="#57C4E5", size=1) +
  geom_hline(yintercept = mean(amostraPreco), color="#ECA087", size=.9)+
  theme_bw()
#----Analise----
predict(amostraProduto)

#----Exportação de dataset----
# Exporte o dataset para um JSON
json_data <- toJSON(dfSemana0)

# Crie uma variável com o caminho
caminho <- "C:\\Users\\Stephany\\Desktop\\Somente-pastas\\Julia\\faculdade\\pi\\b3\\Web-crawler\\analiseDados\\json\\dados.json"

#Escreve o arquivi json
write_json(json_data, path=caminho)




#----Conexão COM O BANCO DE DADOS----
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