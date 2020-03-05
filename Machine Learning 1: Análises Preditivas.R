install.packages(c("e1071","nortest","car","caret"))
install.packages("Hmisc")

library(Hmisc)
library(readxl)
library(caret)
library(e1071)

custo_tratamento <- read_excel("C:/Users/RTenorio/Downloads/custo_tratamento.xlsx")
View(custo_tratamento)

attach(custo_tratamento)
dim(custo_tratamento)

dim(custo_tratamento)

#Qual é a variável que possui a maior correlação com a variável custo? É uma correlação positiva ou negativa?
#qual é a interpretação dessa correlação?
names(custo_tratamento)
correlacao_tr<-custo_tratamento[,c("gravidade_doenca","idade","tempo_internacao","custo")]
head(correlacao_tr)


rcorr(as.matrix(correlacao_tr))

#Separa o arquivo em amostra de treinamento e teste.
treinamento <-createDataPartition(custo_tratamento$custo, p=.7, list = FALSE, times = 1)
tr_modelo <-custo_tratamento[treinamento,]
dim(tr_modelo)

teste<-custo_tratamento[-treinamento,]
dim(teste)


#Faça uma regressão linear simples na amostra de treinamento – Qual o valor do R^2 e sua interpretação?
simples_tr<-lm(tr_modelo$custo ~tr_modelo$tempo_internacao)
anova(simples_tr)
summary(simples_tr)

residuos_tr<-residuals(simples_tr)
ajuste_tr<-fitted(simples_tr)

tr_modelo$residuos_tr<-residuos_tr
tr_modelo$ajuste_tr<-ajuste_tr

head(tr_modelo)





#Interprete o coeficiente do modelo, inclusive se é significante






#Faça uma análise dos resíduos utilizando os gráficos (boxplot, histograma, qqnorm, plot e tira a média dos resíduos),
#qual sua conclusão? O modelo está bom para prever novos dados ou não? Justifique sua resposta.

boxplot(custo_tratamento$custo)
boxplot(custo_tratamento$gravidade_doenca)
boxplot(custo_tratamento$idade)
boxplot(custo_tratamento$tempo_internacao)

simples<- lm(custo_tratamento$custo~custo_tratamento$tempo_internacao)
anova(simples)
summary(simples)


residuos<-residuals(simples)
ajuste<-fitted(simples)
custo_tratamento$ajuste<-ajuste
custo_tratamento$residuos<-residuos

head(custo_tratamento)
mean(residuos)
median(residuos)
boxplot(residuos)
plot(residuos)
hist(residuos)
qqnorm(residuos)
qqline(residuos)


#Faça a previsão desse modelo na amostra de teste.


attach(teste)
names(teste)
simples_teste <-lm(custo~tempo_internacao, data = teste)
previsao_teste <-predict(simples_teste,teste)
teste$previsao <-previsao_teste
head(teste)

dim(teste)
