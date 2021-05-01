
# Parte 1 - R como calculadora

# (2+2)*2

4+1 # soma 4 + 1

2
2*4*10

# Atribuir a objetos

resultado <- 4+1
resultado_1 <- 2*4*10

# Concatenar c()

resultado <- c(1,2,3,4,5)# concatenar

resultado <- c(14,21,32,49,55)

class(resultado)
resultado # vetor

resultado[1:5]

obj <- resultado[c(1,3)]

c(resultado,resultado,resultado)

obj_2 <-c(resultado,obj)

obj_3 <- c(chuva,"seca","chuva","seca","chuva","ano1","ano2")

obj_3[1:4]

c(obj_2,obj_3)

# Concatenando colunas cbind()

cbind(obj_2,obj_3)

mat<-cbind(obj_2,obj_2)

mat[c(1,3),]# linhas

mat[,]

mat[,c(1,3)]# colunas

mat[c(1,3),2]

colnames(mat) <- c("resposta1", "resposra2")
mat
colnames(mat)


cbind(mat,mat)

# Concatenando linhas

rbind(mat[1,],mat[2,])

rbind(obj,obj)

mat[1,]+100

mat[4,2] <- 234
mat <- rbind(mat, mat)
mat

# Print prineiro programa
print("Onla Mundo")

# Princípios de Análise exploratória

head(mat)# 6 primeiros valores

tail(mat)# 6 ultimos valores

nrow(mat)# número de linhas 

ncol(mat)# numero de colunas

soma_linhas <- rowSums(mat)# somas das linhas

cbind(mat, rowSums(mat))

mat_2 <-cbind(mat, soma_linhas)

colnames(mat_2)<-c(var1,"var2","soma")

colSums(mat_2) # Soma das Colunas

#################################################
############### Data Frames #####################


cat <-c("seca","seca","seca","seca", 
        "cheia","cheia","cheia","cheia")
cat

numero <-c(12, 34, 62, 11, 
           34, 43, 67, 44)
numero
numero_2<-c(15, 87, 45, 23,
            68, 23, 88, 99)

df<-cbind(cat, numero, numero_2)

df<-as.data.frame(df) # data frame criado
class(df)
df$cat<-as.factor(df$cat)

plot(x = df[,2], y = df[,3], type = "b")#primeiro plot

boxplot(numero ~ cat)# prieiro boxplot


obj_2[2]


##### Importanto dados

# Esse exemplo importa direto da internet, na proxima aula importaremos de arwuivos csv esxcel etc...
doubs.spe<-read.csv('https://raw.githubusercontent.com/JaquesZanon/Disciplina_R_IBoT_2021/main/DoubsSpe.csv', 
                       row.names = 1)


head(doubs.spe)
tail(doubs.spe)

nrow(doubs.spe)
ncol(doubs.spe)

colnames(doubs.spe)
colSums(doubs.spe)
rowSums(doubs.spe)

doubs.spe[8,]

dim(doubs.spe)

