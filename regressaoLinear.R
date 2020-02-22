install.packages(c("e1071","nortest","car","caret"))
install.packages("Hmisc")

library(Hmisc)
library(readxl)
library(caret)
library(e1071)



custo_tratamento <- read_excel("Downloads/custo_tratamento.xlsx")
View(custo_tratamento)

attach(custo_tratamento)
dim(custo_tratamento)

dim(custo_tratamento)

#Qual é a variável que possui a maior correlação com a variável custo? É uma correlação positiva ou negativa? 
  #qual é a interpretação dessa correlação?
names(custo_tratamento)
correlacao_tr<-custo_tratamento[,c("id","gravidade_doenca","idade","tempo_internacao","custo")]
head(correlacao_tr)


rcorr(as.matrix(correlacao_tr))

#Separa o arquivo em amostra de treinamento e teste.
treinamento <-createDataPartition(custo_tratamento$id, p=.7, list = FALSE, times = 1)
tr_modelo <-custo_tratamento[treinamento,]
dim(tr_modelo)

teste<-custo_tratamento[-treinamento,]
dim(teste)


#Faça uma análise dos resíduos utilizando os gráficos (boxplot, histograma, qqnorm, plot e tira a média dos resíduos), 
#qual sua conclusão? O modelo está bom para prever novos dados ou não? Justifique sua resposta.
kurtosis(custo_tratamento$custo)
skewness(custo_tratamento$custo)
hist(custo_tratamento$custo)
boxplot(custo_tratamento$custo, outline = F)
qqnorm(custo_tratamento$custo)
qqline(custo_tratamento$custo, col="blue")



