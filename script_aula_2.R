##### Mais um pouco.

resultado <- c(14,21,32,49,55,0)

length(resultado)

mean(resultado)

sum(resultado)/length(resultado)


sqrt(resultado)

log10(resultado+1)

log(resultado)

rm(resultado)


# Sequencia

seq(from = 1, to = 10, by = 0.5 )
seq(1,10,2)


# Repetir

rep(x, times=y)
rep(5,10)

rep("a",5)

1:40

rep(1:44,2)
c("A","B")

rep(c("A","B"), each=5)
rep(c("A","B"), times=5)

rep(c("Três","Dois","Sete","Quatro"), c(3,2,7,4))



# Dados aleatórios

set.seed(42)

runif(100, min=0, max=1)

runif(200,80,100)

temp<-runif(200,80,100)

hist(temp)



# rnorm (Gerar dados aleatórios com distribuição normal)

rnorm(n, mean=0, sd=1)

temp2<-rnorm(200,8,10)

hist(temp2)





#Gráficos com variáveis explanatórias que são categóricas.

sex<-c("macho","fêmea")

sexo<-c("Ma","Ma","Ma","Ma","Ma",
        "Fe","Fe","Fe","Fe","Fe")

y<-c(110, 120, 90, 70, 50,
     80, 40, 40, 50, 30)
peso<-y 


plot(sexo,peso)

class(sexo)
sexo
factor(sexo)


plot(sexo,peso)
plot(x = factor(sexo),y = peso)
plot(factor(sexo), peso)

sexo.f<-factor(sexo)
sexo.f


stripchart(peso~sexo)

stripchart(peso~sexo, vertical=TRUE)

stripchart(peso~sexo, vertical=TRUE ,at=c(1.5,1.7), ylab="NOME123")

stripchart(peso~sexo, vertical=TRUE ,at=c(1.5,1.7), method="stack")





#### Importando Dados

spe <- read.csv("DoubsSpe.csv", header = TRUE, row.names = 1, sep = ";")
head(spe)

spe


spe <- read.table("clipboard", header = TRUE, row.names = 1)
head(spe)
spe<-spe[,-1]

### Estatística descritiva

mean(spe[,2])#média
var(spe[,2])# variancia
sd(spe[,2])# desvio padrão


summary(spe)

head(spe)# 6 primeiros valores

tail(spe)# 6 ultimos valores

nrow(spe)# n?mero de linhas 

ncol(spe)# numero de colunas

soma_linhas <- rowSums(mat)

sum(spe[,"CHA"]) 

colSums(spe)


#### Estatística univariada

# Em geral, a maioria das análises é feita especificando-se o modelo que se deseja testar. No R, para
# especificar um modelo é preciso usar a notação de formulas.

## Regressão Linear Simples (1866) Fancis Galton
# A regressão linear simples é utilizada para analisar relações entre variáveis contínuas

galap<-read.table("galapagos.txt", header = TRUE, row.names = 1)
head(galap)

riqueza<-galap[,3]

area<-galap[,2]
plot(area,riqueza)

hist(riqueza)
hist(area)
hist(log10(riqueza))
hist(log10(area))

riqueza.log<-log10(riqueza)

area.log<-log10(area)


plot(area.log,riqueza.log)

length(galap[,"Island"])# numero de ilhas


resultado<-lm(riqueza.log ~ area.log)

resultado

# Log_riqueza = 1.28 + 0.33Log_area

resultado

#Veja que o resultado da regressão mostra os coeficientes a (intercepto) e b (inclinação) da regressão

summary(resultado)

plot(area.log,riqueza.log)
abline(resultado)
text(2,1,"Log_riqueza = 1.28 + 0.33Log_area")

## Regressão Múltipla
# A regressão linear com uma única variável preditora X pode ser facilmente estendido a duas ou mais
# variáveis preditoras (regressão múltipla)

formigas<-read
lat<-formigas[,1] # latitude
alt<-formigas[,2] # altitude
dens.formig<-formigas[,3] # densidade de formigas
lm(log10(dens.formig) ~ lat+alt)

reg.mult<-lm(log10(dens.formig) ~ lat+alt)
reg.mult

summary(reg.mult)

avPlots(reg.mult)# pacote car


### Análise de Variância (Anova)

resposta<-c(10,12,12,13,9,11,11,12,12,13,15,16)
preditor<-c(n.m,n.m,n.m,n.m,c,c,c,c,t,t,
             t,t)

# As siglas significam, n.m = ―não manipulado‖, c = ―controle‖ e t = ―tratamento‖
resu.aov<-aov(resposta~preditor)
summary(resu.aov)

# Para fazer as análises usando a função lm basta trocar pela função lm()
lm(resposta~preditor)
summary.aov(lm(resposta~preditor))


### Árvore de regressão
# A árvore de regressão é uma alternativa interessante, ou um complemento, à análise de regressão
# múltipla. Como na regressão, na árvore de regressão a variável resposta é uma variável contínua, porém as
# variáveis preditoras podem ser categóricas e/ou contínuas


library(vegan)
library(rpart)

data(mite); data(mite.env)
riq<-rowSums(decostand(varespec,"pa"))
rpart(riq ~., mite.env)

# No modelo acima o ".," indica que todas as variáveis do conjunto mite.env serão utilizadas como
# preditores. Seria o equivalente a escrever o modelo da seguinte forma:
# riq~SubsDens+WatrCont+Substrate+Shrub+Topo. Para ver o significado das variáveis entre no help que
# descreve o conjunto de dados.
?mite.env

regtree<-rpart(riq ~., mite.env)
summary(regtree)

plot(regtree)
text(regtree)

#Note que parte do gráfico não aparece. Para corrigir, precisamos definir a margem do gráfico.
plot(regtree,margin=0.03)
text(regtree)# se o problema persistir, mude o valor de margin








