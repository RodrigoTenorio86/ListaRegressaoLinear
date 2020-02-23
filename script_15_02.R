
library(readxl)
Banco <- read_excel("E:/Machine Learning/Banco.xlsx")
View(Banco)

attach(Banco)
names(Banco)

# e1071 - distribuição assimetria e curtose
# nortest - KS testando a normalidade dos dados
# car - teste de levene - testa homogeneidade entre grupos
# caret - Cria amostra de treinamento e teste


# Analise exploratoria dos dados - univariado

 # Medidas de tendencia central: média/ mediana
 # Medidas de posição: Quartil
 # Medidas de dispersão: Desvio Padrão
 # Medidas de distribuição: Simétrica/ Assimetrica Positiva e negativa.
 # Testando a normalidade Teste Ks.

# Analise exploratoria dos dados - bivariado
# Teste Lenene
# Teste de médias
# Amostra Treinamento e teste.

install.packages(c("e1071","nortest","car","caret"))
library(e1071)
library(nortest)
library(car)
library(caret)

mean(Banco$salario)
mean(Banco$salario, trim = 0.05)
median(Banco$salario)
sd(Banco$salario)

# library(e1071)
kurtosis(Banco$salario)
skewness(Banco$salario)


hist(Banco$salario)
boxplot(Banco$salario, outline = FALSE)
qqnorm(Banco$salario)
qqline(Banco$salario, col="blue")

#nortest

lillie.test(Banco$salario)
shapiro.test(Banco$salario)
dim(Banco)



boxplot(salario ~sexo , outline=FALSE)


# library(car)
# testa levene
library(car)

leveneTest(salario ~ sexo, center=mean)

# sd for igual acieta H0

t.test(salario ~sexo, var.equal= FALSE)

42434.73 - 26160.92


dim(Banco)

amostra300 <-Banco[sample(nrow(Banco),300),]
dim(amostra300)

library(caret)

treinamento <-createDataPartition(amostra300$id, p=.7, list=FALSE, times=1)
tr_modelo1 <-amostra300[treinamento,]
dim(tr_modelo1)

teste <-amostra300[-treinamento,]
dim(teste)

212 + 88

































