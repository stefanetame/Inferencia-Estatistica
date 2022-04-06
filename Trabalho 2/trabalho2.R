# TRABALHO 2 - ESTIMAÇÃO PONTUAL

rm(list = ls(all = TRUE))
setwd('/home/stefane/Documentos/2021.1/Inferencia/Trabalho2')
library(readxl)

dados <- read.delim("Amostra 27 .txt", header = FALSE)
View(dados)

N = 100 #quantidade de lotes, ou seja, tamanho da amostra
n = 5 #quantidade de tentativas, ou seja, a quantidade de computadores em cada lote
x <- dados[,1] #x recebe meus dados

p = seq(0.01,1,len=100)
#vetor p -> diferentes valores para p (probabilidade do computador ter defeito)
vero=c()
log_vero=c()

for(i in 1:length(p)){
  #calcula as funções de verossimilhança e log-verossimilhança para cada p
  vero[i]=prod(dbinom(x, n, p[i]))
  log_vero[i]=log(vero[i])
}

vero
log_vero

par(mfrow=c(1,2))
x11()
plot(p,vero,type='l',col='red',lwd=2,ylab='Verossimilhança',xlab=expression(p))
x11()
plot(p,log_vero,type='l',col='blue',lwd=2,ylab='ln(Verossimilhança)', xlab=expression(p))

EMV_p1=p[which(vero==max(vero),arr.ind=T)]
EMV_p1

EMV_p2=p[which(log_vero==max(log_vero),arr.ind=T)]
EMV_p2

EMV=mean(x)/n #estimador de p
EMV
