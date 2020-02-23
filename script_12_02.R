install.packages(c("e1071","nortest","car","caret"))

library(readx1)
library(e1071)
library(nortest)
library(car)
library(caret)

Banco <- read_excel("Desktop\Banco.xlsx")
attach(Banco)
View(Banco)

mean(Banco$salario)
mean(Banco$salario, trim = 0.05)
median(Banco$salario)
sd(Banco$salario)
quantile(Banco$salario)
max(Banco$salario)
min(Banco$salario)

#library(e1071)
#se os valores derem diferentes de 0 significa que voce tem valores fora que estragam o modelo
#kurtosi altura da curva
#skewness pontas da curva
kurtosis(Banco$salario)
skewness((Banco$salario))

#plota histograma
hist((Banco$salario))

#grafico de boxplot com outlier
boxplot(Banco$salario)
#grafico de boxplot sem outlier
boxplot(Banco$salario, outline = FALSE)

#Q-Q PLOT PARA MOSTRAR QUE NAO SEGUE UMA NORMALIDADE
qqnorm(Banco$salario)#valor total
qqline(Banco$salario, col="blue")#valor normal

#library(nortest) testa se a variavel segue a normalidade

lillie.test(Banco$salario)#precisa de mais de 50 linhas
shapiro.test(Banco$salario)#nativo do r precisa de menos de 50 linhas

dim(Banco)#>50 linhas entao usa a lillie

#	Lilliefors (Kolmogorov-Smirnov) normality test
#data:  Banco$salario
#D = 0.21101, p-value < 2.2e-16
#como o valor nao e menor que 0,5 ela nao segue uma normalidade.

boxplot(salario ~sexo)

boxplot(salario ~sexo, outline=FALSE)

#lib(car)
#test de levene

leveneTest(salario ~ sexo, center=mean)
# < H0 0,5 os grupos nao sao iguais

#sd for igual acieta H0 (TESTE T NAO PRECISA DE PACOTE)

t.test(salario ~sexo, var.equal = FALSE)

42434.73 - 26160.92 #Diferenca de medias

# se meu p valor rejeita h0 e menor 0,5 entao as minhas medias nao sao iguais 

dim(Banco)

amostra300 <- Banco[sample(nrow(Banco),300),]
dim(amostra300)

#pacote caret para dividr amostra em 70/30 treinamento

treinamento <-createDataPartition(amostra300$id, p=.7, list = FALSE, times = 1)
tr_modelo <-Banco[treinamento,]
dim(tr_modelo)

teste<-amostra300[-treinamento,]
dim(teste)

212+88
