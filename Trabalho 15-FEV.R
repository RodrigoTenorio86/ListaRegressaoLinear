# Leitura Arquivo Excel
library(readxl)
custo_tratamento <- read_excel("~/Downloads/custo_tratamento.xlsx")
View(custo_tratamento)

attach(custo_tratamento)
names(custo_tratamento)

install.packages(c("e1071","caret","Hmisc","nortest","car","faraway"))
library(e1071)
library(caret)
library(Hmisc)
library(nortest)
library(car)
library(faraway)

# Gera correlação
rcorr(as.matrix(custo_tratamento))


# Amostra de Treinamento e Teste
treinamento <-createDataPartition(custo_tratamento$custo, p = .7, list = FALSE, times = 1)
tr_modelo <- custo_tratamento[treinamento,]
dim(tr_modelo)
head(tr_modelo)
attach(tr_modelo)

teste <- custo_tratamento[-treinamento,]
dim(teste)
head(teste)

# Regressao Linear
rcorr(as.matrix(tr_modelo))
modelo <-lm(custo ~ tempo_internacao)
summary(modelo)
modelo

# Analise dos residuos
erro <-residuals(modelo)
tr_modelo$erro <- erro
previsao <-predict(modelo)
tr_modelo$previsao <- previsao

attach(tr_modelo)
names(tr_modelo)

head(tr_modelo)

# Aplica na base teste
attach(teste)
names(teste)

previsao <- predict(modelo, teste)
teste$previsao <- previsao
head(teste)

dim(teste)
