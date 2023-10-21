#----Importação----
library(jsonlite)
library(dplyr)
library(ggplot2)
#library(esquisse)
#library(RMariaDB)

#----Criação de dataset----

dfSemana0<-data.frame("Produto"=dfProduto$semana0, "Preco"=dfFornecedor$precoFonecedor0)
dfUltima<-data.frame("Produto"=dfProduto$semana51, "Preco" = dfFornecedor$precoFonecedor51)

#----Amostras----
amostraProduto<-sample(dfSemana0$Produto, size = 50, replace = TRUE)
#amostraPreco<-sample(dfSemana0$Preco, size = 100, replace = TRUE)*100
amostraPreco<-sample(dfSemana0$Preco, size = 50, replace = TRUE)

amostraProdutoUltima<-sample(dfUltima$Produto, size = 50, replace = TRUE)
amostraPrecoUltima<-sample(dfUltima$Preco, size = 50, replace = TRUE)


#----Sumarios----
summary(dfProduto$semana0)
summary(amostraPreco)
summary(amostraProduto)

summary(amostraPrecoUltima)
summary(amostraProdutoUltima)

#----Gráficos----

#----Semana 0----
hist(dfSemana0$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
hist(dfSemana0$Preco, col="#31572C", main="Preço do produto", ylab = "Frequência", xlab = "Valores")


plot(amostraProduto~amostraPreco) 
coeficiente<-lm(amostraProduto~amostraPreco)
abline(coeficiente)

retas <- ggplot(mapping = aes(amostraProduto,amostraPreco)) +
  xlab("Quantidade produto") +
  ylab("Preço") +
   geom_point() +
   geom_smooth(se = FALSE, method = "lm") +
   geom_hline(yintercept = mean(amostraPreco))

retas

retasPre <- ggplot(mapping = aes(amostraProduto,amostraPreco)) +
  xlab("Quantidade produto")+
  ylab("Preço")+
  geom_point() +
  geom_smooth( method = "lm") +
  geom_hline(yintercept = mean(amostraPreco))

retasPre

tabelaLegenda<- tribble(
  ~Cor, ~Explicação,
  "Ciano", "Representa a tendência dos dados",
  "Vermelho", "Representa a média dos dados",
  "Azul", "Representa os pontos de dados",
  "Vermelho bordo", "Representa uma linha",
)

regressao<-retas +
  #ggtitle("Gráfico de dispersão da quantidade de Produto vendidos e os preços") +
  labs(
    title = "Gráfico de dispersão da quantidade de Produto vendidos e os preços",
    subtitle = "Primeira semana"
  )+
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_segment(aes(x = amostraProduto, y = amostraPreco,
                   xend = amostraProduto, yend = mean(amostraPreco)), color="black", size=.9) +
  geom_point(color="#a31621", size=2) +
  geom_smooth(se = FALSE, method = "lm", color="#57C4E5", size=1) +
  geom_hline(yintercept = mean(amostraPreco), color="red", size=.9)+
  theme_bw()

regressao 

  regressaoPrev<-regressao +
  #ggtitle("Gráfico de dispersão da quantidade de Produto vendidos e os preços") +
  labs(
    title = "Gráfico de dispersão da quantidade de Produto vendidos e os preços",
    subtitle = "Primeira semana"
  )+
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_segment(aes(x = amostraProduto, y = amostraPreco,
                   xend = amostraProduto, yend = mean(predict(coeficiente))), color="black", size=.9) +
  geom_point(color="#a31621", size=2) +
  geom_smooth(se = FALSE, method = "lm", color="#57C4E5", size=1) +
  geom_hline(yintercept = mean(amostraPreco), color="red", size=.9)+
  theme_bw()

regressaoPrev

#----Ultima Semana----

hist(dfUltima$Produto,  col="#abf5bf",main="Produtos vendidos", ylab = "Frequência", xlab = "Quantidade")
hist(dfUltima$Preco, col="#31572C", main="Preço do produto", ylab = "Frequência", xlab = "Valores")


plot(amostraProdutoUltima~amostraPrecoUltima)
coeficienteUltimo<-lm(amostraProdutoUltima~amostraPrecoUltima)
abline(coeficienteUltimo)

retasUltima <- ggplot(mapping = aes(amostraProdutoUltima,amostraPrecoUltima)) +
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_point() +
  geom_smooth(se = FALSE, method = "lm") +
  geom_hline(yintercept = mean(amostraPrecoUltima))

retasUltima

retasPreUltima <- ggplot(mapping = aes(amostraProdutoUltima,amostraPrecoUltima)) +
  geom_point() +
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_smooth( method = "lm") +
  geom_hline(yintercept = mean(amostraPrecoUltima))

retasPreUltima

regressaoUltima<-retasUltima +
  #ggtitle("Gráfico de dispersão da quantidade de Produto vendidos e os preços") +
  labs(
    title = "Gráfico de dispersão da quantidade de Produto vendidos e os preços",
    subtitle = "Ultima semana"
  )+
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_segment(aes(x = amostraProdutoUltima, y = amostraPrecoUltima,
                   xend = amostraProdutoUltima, yend = mean(amostraPrecoUltima)), color="black", size=.9) +
  geom_point(color="#a31621", size=2) +
  geom_smooth(se = FALSE, method = "lm", color="#57C4E5", size=1) +
  geom_hline(yintercept = mean(amostraPrecoUltima), color="red", size=.9)+
  theme_bw()

regressaoUltima

regressaoPrevUltima<-regressaoUltima +
  #ggtitle("Gráfico de dispersão da quantidade de Produto vendidos e os preços") +
  labs(
    title = "Gráfico de dispersão da quantidade de Produto vendidos e os preços",
    subtitle = "Última semana"
  )+
  xlab("Quantidade de vendas") +
  ylab("Valores") +
  geom_segment(aes(x = amostraProdutoUltima, y = amostraPrecoUltima,
                   xend = amostraProdutoUltima, yend = mean(predict(coeficienteUltimo))), color="black", size=.9) +
  geom_point(color="#a31621", size=2) +
  geom_smooth(se = FALSE, method = "lm", color="#57C4E5", size=1) +
  geom_hline(yintercept = mean(amostraPrecoUltima), color="red", size=.9)+
  theme_bw()

regressaoPrevUltima

#----Analise----
resulta0<-predict(coeficiente)
hist(resulta0)

resultadoUltimo<-predict(coeficienteUltimo)
hist(resultadoUltimo)

#regressao + regressaoUltima + plot_layout(nrow = 2)


#----Exportação de dataset----
# Exporte o dataset para um JSON
dados<-data.frame(dfSemana0, dfUltima)
json_data <- toJSON(dados)

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