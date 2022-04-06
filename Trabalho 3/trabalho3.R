############################################################## 
#                       TRABALHO 3                           #
#                                                            #
#                        Grupo 4                             #
############################################################## 

rm(list = ls(all = TRUE)) # limpa a memoria
setwd("~/Inferencia/Trabalho 3")
# Caminho at√© a pasta que cont√©m o arquivo "Amostra_Grupo4.txt"

# recebendo os dados:
dados <- read.table("Amostra_Grupo4.txt", head=T)
#View(dados) # comando para visualizar os dados

alpha = 0.05 # nivel de significancia: 5%
z = 1.96 # z_alpha/2

# possibilidades de votos:
# [1] c1: candidato 1
# [2] c2: candidato 2
# [3] c3: candidato 3
# [4] o: outros candidatos
# [5] b: votos brancos
# [6] n: votos nulos 
# [7] i: indecisos

# regi√£o 1
n1 = 30000 # popula√ß√£o da regiao 1
reg1 <- subset(dados, dados$Regiao == 'R1') # dados da regi√£o 1
tam1 = length(reg1$Voto) # tamanho da amostra da regi√£o 1
# n√∫meros de pessoas que votam em C1 ou ...
r1 = c()
r1[1] = length(subset(reg1, reg1$Voto == 'C1')$Voto)
r1[2] = length(subset(reg1, reg1$Voto == 'C2')$Voto)
r1[3] = length(subset(reg1, reg1$Voto == 'C3')$Voto)
r1[4] = length(subset(reg1, reg1$Voto == 'O')$Voto)
r1[5] = length(subset(reg1, reg1$Voto == 'B')$Voto)
r1[6] = length(subset(reg1, reg1$Voto == 'N')$Voto)
r1[7] = length(subset(reg1, reg1$Voto == 'I')$Voto)
# probabilidades:
p_r1 = c()
for(i in 1:7){
  p_r1[i] = r1[i]/tam1
}
# erro amostral:
# abordagem otimista:
Eo_r1 = c()
for(i in 1:7){
  Eo_r1[i] = z*sqrt(p_r1[i]*(1-p_r1[i])/tam1)
}
# intervalos de confian√ßa:
IC_r1_otimista = rbind()
for(i in 1:7){
  ic = c(p_r1[i]-Eo_r1[i],p_r1[i]+Eo_r1[i])
  IC_r1_otimista = rbind(IC_r1_otimista, ic)
}
# abordagem conservativa:
Ec_r1 = z*sqrt(1/(4*tam1))
# intervalos de confian√ßa:
IC_r1_conservativa = rbind()
for(i in 1:7){
  ic = c(p_r1[i]-Ec_r1,p_r1[i]+Ec_r1)
  IC_r1_conservativa = rbind(IC_r1_conservativa, ic)
}

# regi√£o 2
n2 = 50000 # popula√ß√£o da regiao 2
reg2 <- subset(dados, dados$Regiao == 'R2') # dados da regi√£o 2
tam2 = length(reg2$Voto) # tamanho da amostra da regi√£o 2
# n√∫meros de pessoas que votam em C1 ou ...
r2 = c()
r2[1] = length(subset(reg2, reg2$Voto == 'C1')$Voto)
r2[2] = length(subset(reg2, reg2$Voto == 'C2')$Voto)
r2[3] = length(subset(reg2, reg2$Voto == 'C3')$Voto)
r2[4] = length(subset(reg2, reg2$Voto == 'O')$Voto)
r2[5] = length(subset(reg2, reg2$Voto == 'B')$Voto)
r2[6] = length(subset(reg2, reg2$Voto == 'N')$Voto)
r2[7] = length(subset(reg2, reg2$Voto == 'I')$Voto)
# probabilidades:
p_r2 = c()
for(i in 1:7){
  p_r2[i] = r2[i]/tam2
}
# erro amostral:
# abordagem otimista:
Eo_r2 = c()
for(i in 1:7){
  Eo_r2[i] = z*sqrt(p_r2[i]*(1-p_r2[i])/tam2)
}
# intervalos de confian√ßa:
IC_r2_otimista = rbind()
for(i in 1:7){
  ic = c(p_r2[i]-Eo_r2[i],p_r2[i]+Eo_r2[i])
  IC_r2_otimista = rbind(IC_r2_otimista, ic)
}
# abordagem conservativa:
Ec_r2 = z*sqrt(1/(4*tam2))
# intervalos de confian√ßa:
IC_r2_conservativa = rbind()
for(i in 1:7){
  ic = c(p_r2[i]-Ec_r2,p_r2[i]+Ec_r2)
  IC_r2_conservativa = rbind(IC_r2_conservativa, ic)
}

# regi√£o 3
n3 = 65000 # popula√ß√£o da regiao 3
reg3 <- subset(dados, dados$Regiao == 'R3') # dados da regi√£o 3
tam3 = length(reg3$Voto) # tamanho da amostra da regi√£o 3
# n√∫meros de pessoas que votam em C1 ou ...
r3 = c()
r3[1] = length(subset(reg3, reg3$Voto == 'C1')$Voto)
r3[2] = length(subset(reg3, reg3$Voto == 'C2')$Voto)
r3[3] = length(subset(reg3, reg3$Voto == 'C3')$Voto)
r3[4] = length(subset(reg3, reg3$Voto == 'O')$Voto)
r3[5] = length(subset(reg3, reg3$Voto == 'B')$Voto)
r3[6] = length(subset(reg3, reg3$Voto == 'N')$Voto)
r3[7] = length(subset(reg3, reg3$Voto == 'I')$Voto)
# probabilidades:
p_r3 = c()
for(i in 1:7){
  p_r3[i] = r3[i]/tam3
}
# erro amostral:
# abordagem otimista:
Eo_r3 = c()
for(i in 1:7){
  Eo_r3[i] = z*sqrt(p_r3[i]*(1-p_r3[i])/tam3)
}
# intervalos de confian√ßa:
IC_r3_otimista = rbind()
for(i in 1:7){
  ic = c(p_r3[i]-Eo_r3[i],p_r3[i]+Eo_r3[i])
  IC_r3_otimista = rbind(IC_r3_otimista, ic)
}
# abordagem conservativa:
Ec_r3 = z*sqrt(1/(4*tam3))
# intervalos de confian√ßa:
IC_r3_conservativa = rbind()
for(i in 1:7){
  ic = c(p_r3[i]-Ec_r3,p_r3[i]+Ec_r3)
  IC_r3_conservativa = rbind(IC_r3_conservativa, ic)
}

# regi√£o 4
n4 = 40000 # popula√ß√£o da regiao 4
reg4 <- subset(dados, dados$Regiao == 'R4') # dados da regi√£o 4
tam4 = length(reg4$Voto) # tamanho da amostra da regi√£o 4
# n√∫meros de pessoas que votam em C1 ou ...
r4 = c()
r4[1] = length(subset(reg4, reg4$Voto == 'C1')$Voto)
r4[2] = length(subset(reg4, reg4$Voto == 'C2')$Voto)
r4[3] = length(subset(reg4, reg4$Voto == 'C3')$Voto)
r4[4] = length(subset(reg4, reg4$Voto == 'O')$Voto)
r4[5] = length(subset(reg4, reg4$Voto == 'B')$Voto)
r4[6] = length(subset(reg4, reg4$Voto == 'N')$Voto)
r4[7] = length(subset(reg4, reg4$Voto == 'I')$Voto)
# probabilidades:
p_r4 = c()
for(i in 1:7){
  p_r4[i] = r4[i]/tam4
}
# erro amostral:
# abordagem otimista:
Eo_r4 = c()
for(i in 1:7){
  Eo_r4[i] = z*sqrt(p_r4[i]*(1-p_r4[i])/tam4)
}
# intervalos de confian√ßa:
IC_r4_otimista = rbind()
for(i in 1:7){
  ic = c(p_r4[i]-Eo_r4[i],p_r4[i]+Eo_r4[i])
  IC_r4_otimista = rbind(IC_r4_otimista, ic)
}
# abordagem conservativa:
Ec_r4 = z*sqrt(1/(4*tam4))
# intervalos de confian√ßa:
IC_r4_conservativa = rbind()
for(i in 1:7){
  ic = c(p_r4[i]-Ec_r4,p_r4[i]+Ec_r4)
  IC_r4_conservativa = rbind(IC_r4_conservativa, ic)
}

# regi√£o 5
n5 = 55000 # popula√ß√£o da regiao 5
reg5 <- subset(dados, dados$Regiao == 'R5') # dados da regi√£o 5
tam5 = length(reg5$Voto) # tamanho da amostra da regi√£o 5
# n√∫meros de pessoas que votam em C1 ou ...
r5 = c()
r5[1] = length(subset(reg5, reg5$Voto == 'C1')$Voto)
r5[2] = length(subset(reg5, reg5$Voto == 'C2')$Voto)
r5[3] = length(subset(reg5, reg5$Voto == 'C3')$Voto)
r5[4] = length(subset(reg5, reg5$Voto == 'O')$Voto)
r5[5] = length(subset(reg5, reg5$Voto == 'B')$Voto)
r5[6] = length(subset(reg5, reg5$Voto == 'N')$Voto)
r5[7] = length(subset(reg5, reg5$Voto == 'I')$Voto)
# probabilidades:
p_r5 = c()
for(i in 1:7){
  p_r5[i] = r5[i]/tam5
}
# erro amostral:
# abordagem otimista:
Eo_r5 = c()
for(i in 1:7){
  Eo_r5[i] = z*sqrt(p_r5[i]*(1-p_r5[i])/tam5)
}
# intervalos de confian√ßa:
IC_r5_otimista = rbind()
for(i in 1:7){
  ic = c(p_r5[i]-Eo_r5[i],p_r5[i]+Eo_r5[i])
  IC_r5_otimista = rbind(IC_r5_otimista, ic)
}
# abordagem conservativa:
Ec_r5 = z*sqrt(1/(4*tam5))
# intervalos de confian√ßa:
IC_r5_conservativa = rbind()
for(i in 1:7){
  ic = c(p_r5[i]-Ec_r5,p_r5[i]+Ec_r5)
  IC_r5_conservativa = rbind(IC_r5_conservativa, ic)
}

############################ Quest„o 2 ##########################################

#Regi„o 1 - intervalo de confianÁa

IC_q2_r1 = rbind()
for(i in 1:7){
  ic = c(p_r1[i]-0.02,p_r1[i]+0.02)
  IC_q2_r1= rbind(IC_q2_r1, ic)
}

colnames(IC_q2_r1)<-c("-2%", "+2%")
rownames(IC_q2_r1)<-c("C1", "C2", "C3", "O","B", "N", "I")

IC_q2_r1

#Gr·fico Regi„o 1 - sem o erro

IC_q2_r1_100 = rbind()
for(i in 1:7){
  ic = c(p_r1[i]*100)
  IC_q2_r1_100= rbind(IC_q2_r1_100, ic)
}

IC_q2_r1_100<- round(IC_q2_r1_100, 2)


pie(IC_q2_r1_100[,1], labels = paste0(IC_q2_r1_100, "%"),
    col = c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3" ), main = "Regi„o 1- sem erro")

legend("topleft", legend = c("C1", "C2", "C3", "O","B"),
       fill =  c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3"))


#Regi„o 2

IC_q2_r2 = rbind()
for(i in 1:7){
  ic = c(p_r2[i]-0.02,p_r2[i]+0.02)
  IC_q2_r2= rbind(IC_q2_r2, ic)
}

colnames(IC_q2_r2)<-c("-2%", "+2%")
rownames(IC_q2_r2)<-c("C1", "C2", "C3", "O","B", "N", "I")

IC_q2_r2

#Gr·fico Regi„o 2 - sem o erro

IC_q2_r2_100 = rbind()
for(i in 1:7){
  ic = c(p_r2[i]*100)
  IC_q2_r2_100= rbind(IC_q2_r2_100, ic)
}

IC_q2_r2_100<- round(IC_q2_r2_100,2)


pie(IC_q2_r2_100[,1], labels = paste0(IC_q2_r2_100, "%"),
    col = c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3" ), main = "Regi„o 2 - sem erro")

legend("topleft", legend = c("C1", "C2", "C3", "O","B"),
       fill =  c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3"))


#Regi„o 3

IC_q2_r3 = rbind()
for(i in 1:7){
  ic = c(p_r3[i]-0.02,p_r3[i]+0.02)
  IC_q2_r3= rbind(IC_q2_r3, ic)
}

colnames(IC_q2_r3)<-c("-2%", "+2%")
rownames(IC_q2_r3)<-c("C1", "C2", "C3", "O","B", "N", "I")

IC_q2_r3

#Gr·fico Regi„o 3 - sem o erro

IC_q2_r3_100 = rbind()
for(i in 1:7){
  ic = c(p_r3[i]*100)
  IC_q2_r3_100= rbind(IC_q2_r3_100, ic)
}

IC_q2_r3_100<-round(IC_q2_r3_100, 2)


pie(IC_q2_r3_100[,1], labels = paste0(IC_q2_r3_100, "%"),
    col = c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3" ), main = "Regi„o 3 - sem erro")

legend("topleft", legend = c("C1", "C2", "C3", "O","B"),
       fill =  c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3"))


#Regi„o 4

IC_q2_r4 = rbind()
for(i in 1:7){
  ic = c(p_r4[i]-0.02,p_r4[i]+0.02)
  IC_q2_r4= rbind(IC_q2_r4, ic)
}

colnames(IC_q2_r4)<-c("-2%", "+2%")
rownames(IC_q2_r4)<-c("C1", "C2", "C3", "O","B", "N", "I")

IC_q2_r4

#Gr·fico Regi„o 4 - sem o erro

IC_q2_r4_100 = rbind()
for(i in 1:7){
  ic = c(p_r4[i]*100)
  IC_q2_r4_100= rbind(IC_q2_r4_100, ic)
}

IC_q2_r4_100<-round(IC_q2_r4_100, 2)


pie(IC_q2_r4_100[,1], labels = paste0(IC_q2_r4_100, "%"),
    col = c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3" ), main = "Regi„o 4 - sem erro")

legend("topleft", legend = c("C1", "C2", "C3", "O","B"),
       fill =  c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3"))


#Regi„o 5

IC_q2_r5 = rbind()
for(i in 1:7){
  ic = c(p_r5[i]-0.02,p_r5[i]+0.02)
  IC_q2_r5= rbind(IC_q2_r5, ic)
}

colnames(IC_q2_r5)<-c("-2%", "+2%")
rownames(IC_q2_r5)<-c("C1", "C2", "C3", "O","B", "N", "I")

IC_q2_r5

#Gr·fico Regi„o 5 - sem o erro

IC_q2_r5_100 = rbind()
for(i in 1:7){
  ic = c(p_r5[i]*100)
  IC_q2_r5_100= rbind(IC_q2_r5_100, ic)
}

IC_q2_r5_100<-round(IC_q2_r5_100, 2)


pie(IC_q2_r5_100[,1], labels = paste0(IC_q2_r5_100, "%"),
    col = c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3" ), main = "Regi„o 5 - sem erro")

legend("topleft", legend = c("C1", "C2", "C3", "O","B"),
       fill =  c("lavender", "lightblue", "mistyrose","cadetblue1", "khaki1","lightskyblue4" ,"tan3"))

