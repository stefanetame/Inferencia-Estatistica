############################################################## 
#                   Banco de Dados - COVID 19                #
#                                                            #
#                        Análise                             #
##############################################################  

rm(list = ls(all = TRUE)) #limpa a memoria
#setwd('/home/stefane/Documentos/2021.1/Inferencia') # definindo nosso diretorio de trabalho
setwd("~/Documentos/SME0211_INFERENCIA/Trabalho1")
#library(readr)
library(readxl)
library(fdth)
#library(data.table)

#-------------------------------
# Leitura de dados em arquivos

#dados= read_excel("/home/stefane/Documentos/2021.1/Inferencia/Dados_Sudeste.xlsx")
dados = read_excel("Dados_Sudeste.xlsx")
View(dados)

#-------------------------------
# Selecionando as variáveis de interesse da tabela

# Definindo escala: casos por milhao e mortes por milhao
dados$casosNovos_100mil = (dados$casosNovos*100000)/dados$populacaoTCU2019
dados$obitosNovos_100mil = (dados$obitosNovos*100000)/dados$populacaoTCU2019

# casos por dia

attach(dados)

casosNovosMG = subset(casosNovos_100mil, estado == 'MG') 
casosNovosSP = subset(casosNovos_100mil, estado == 'SP') 
casosNovosES = subset(casosNovos_100mil, estado == 'ES') 
casosNovosRJ = subset(casosNovos_100mil, estado == 'RJ') 

# obitos por dia

obitosNovosMG = subset(obitosNovos_100mil, estado == 'MG') 
obitosNovosSP = subset(obitosNovos_100mil, estado == 'SP') 
obitosNovosES = subset(obitosNovos_100mil, estado == 'ES') 
obitosNovosRJ = subset(obitosNovos_100mil, estado == 'RJ') 

n_MG = length(casosNovosMG)
n_SP = length(casosNovosSP)
n_ES = length(casosNovosES)
n_RJ = length(casosNovosRJ)

detach(dados)

#-------------------------------
# item (b)

SP <- subset(dados, dados$estado == "SP")
RJ <- subset(dados, dados$estado == "RJ")
MG <- subset(dados, dados$estado == "MG")
ES <- subset(dados, dados$estado == "ES")

SP$casosAcumulado = SP$casosAcumulado*100000/SP$populacaoTCU2019
RJ$casosAcumulado = RJ$casosAcumulado*100000/RJ$populacaoTCU2019
MG$casosAcumulado = MG$casosAcumulado*100000/MG$populacaoTCU2019
ES$casosAcumulado = ES$casosAcumulado*100000/ES$populacaoTCU2019

SP$obitosAcumulado = SP$obitosAcumulado*100000/SP$populacaoTCU2019
RJ$obitosAcumulado = RJ$obitosAcumulado*100000/RJ$populacaoTCU2019
MG$obitosAcumulado = MG$obitosAcumulado*100000/MG$populacaoTCU2019
ES$obitosAcumulado = ES$obitosAcumulado*100000/ES$populacaoTCU2019

# Grafico Acumulado

x11()
plot(SP$casosAcumulado, 
                 xaxt = "n", 
                 xlab = "data", ylab = "Casos por cem mil habitantes", 
                 col = ("red"),
                 type = "l",
                 main = "Numero de casos diarios acumulado")

axis(side = 1, 
     at =  seq(1, n_SP, n_SP%/%4),
     labels = SP$data[seq(1, n_SP, n_SP%/%4)])

legend("bottomright",
       col = c("red", "blue", "green", "purple"),
       legend = c("SP", "MG", "RJ", "ES"), 
       lty = 1,
       lwd = 3, 
       bty = "n")

op <- par(new = TRUE)
plot(MG$casosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("blue"))

op <- par(new = TRUE)
plot(RJ$casosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("green"))

op <- par(new = TRUE)
plot(ES$casosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("purple"))

op <- par(lty = 1, lwd = 1)

plot(SP$obitosAcumulado, 
     xaxt = "n", 
     xlab = "data", ylab = "obitos por cem mil habitantes", 
     col = ("red"),
     type = "l",
     main = "Numero de obitos diarios acumulado")

axis(side = 1, 
     at =  seq(1, n_SP, n_SP%/%4),
     labels = SP$data[seq(1, n_SP, n_SP%/%4)])

op <- par(new = TRUE)
plot(MG$obitosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("blue"))

op <- par(new = TRUE)
plot(RJ$obitosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("green"))

op <- par(new = TRUE)
plot(ES$obitosAcumulado, type = "l", xlab = "", ylab = "", xaxt = "n", yaxt = "n", col = ("purple"))

legend("bottomright",
       col = c("red", "blue", "green", "purple"),
       legend = c("SP", "MG", "RJ", "ES"), 
       lty = 1,
       lwd = 3, 
       bty = "n")

op <- par(lty = 1, lwd = 1)

# Graficos casos diarios // obitos diarios

x11()
plot(1:n_MG,casosNovosMG, type='b', pch=16, col='blue', cex=1.5, xlab='Dia', ylab='Casos diários por 100.000 hab. (MG)')

x11()
plot(1:n_SP,casosNovosSP, type='b', pch=16, col='red', cex=1.5, xlab='Dia', ylab='Casos diários por 100.000 hab. (SP)')

x11()
plot(1:n_ES,casosNovosES, type='b', pch=16,col='purple', cex=1.5, xlab='Dia', ylab='Casos diários por 100.000 hab. (ES)')

x11()
plot(1:n_RJ,casosNovosRJ, type='b', pch=16,col='green', cex=1.5, xlab='Dia', ylab='Casos diários por 100.000 hab. (RJ)')


x11()
plot(1:n_MG,obitosNovosMG, type='b', pch=16,col='blue', cex=1.5, xlab='Dia', ylab='Óbitos diários por 100.000 hab. (MG)')

x11()
plot(1:n_SP,obitosNovosSP, type='b', pch=16,col='red', cex=1.5, xlab='Dia', ylab='Óbitos diários por 100.000 hab. (SP)')

x11()
plot(1:n_ES,obitosNovosES, type='b', pch=16,col='purple', cex=1.5, xlab='Dia', ylab='Óbitos diários por 100.000 hab. (ES)')

x11()
plot(1:n_RJ,obitosNovosRJ, type='b', pch=16,col='green', cex=1.5, xlab='Dia', ylab='Óbitos diários por 100.000 hab. (RJ)')


#-------------------------------
# item (c)

summary(casosNovosMG)
summary(obitosNovosMG)
summary(casosNovosSP)
summary(obitosNovosSP)
summary(casosNovosRJ)
summary(obitosNovosRJ)
summary(casosNovosES)
summary(obitosNovosES)


#-------------------------------
# itens (d) e (e)

# MINAS GERAIS
# casos Novos
minCasosMG = min(casosNovosMG)
maxCasosMG = max(casosNovosMG)
AmplitudeCasosMG = maxCasosMG - minCasosMG 
mediaCasosMG = mean(casosNovosMG)
medianaCasosMG = median(casosNovosMG)
quantilCasosMG = quantile(casosNovosMG, c(0.25,0.75), tipe=2)
AmplitudeQCasosMG = quantilCasosMG[1] - quantilCasosMG[2]
desvioPadraoCasosMG = sd(casosNovosMG)

# obitos Novos
minMortesMG = min(obitosNovosMG)
maxMortesMG = max(obitosNovosMG)
AmplitudeMortesMG = maxMortesMG - minMortesMG
mediaMortesMG = mean(obitosNovosMG)
medianaMortesMG = median(obitosNovosMG)
quantilMortesMG = quantile(obitosNovosMG, c(0.25,0.75), tipe=2)
AmplitudeQMortesMG = quantilMortesMG[1] - quantilMortesMG[2]
desvioPadraoMortesMG = sd(obitosNovosMG)

# Sao Paulo
# casos Novos
minCasosSP = min(casosNovosSP)
maxCasosSP = max(casosNovosSP)
AmplitudeCasosSP = - minCasosSP + maxCasosSP
mediaCasosSP = mean(casosNovosSP)
medianaCasosSP = median(casosNovosSP)
quantilCasosSP = quantile(casosNovosSP, c(0.25,0.75), tipe=2)
AmplitudeQCasosSP = quantilCasosSP[1] - quantilCasosSP[2]
desvioPadraoCasosSP = sd(casosNovosSP)

# obitos Novos
minMortesSP = min(obitosNovosSP)
maxMortesSP = max(obitosNovosSP)
AmplitudeMortesP = - minMortesSP + maxMortesSP
mediaMortesSP = mean(obitosNovosSP)
medianaMortesSP = median(obitosNovosSP)
quantilMortesSP = quantile(obitosNovosSP, c(0.25,0.75), tipe=2)
AmplitudeQMortesSP = quantilMortesSP[1] - quantilMortesSP[2]
desvioPadraoMortesSP = sd(obitosNovosSP)

# Rio de Janeiro
# casos Novos
minCasosRJ = min(casosNovosRJ)
maxCasosRJ = max(casosNovosRJ)
AmplitudeCasosRJ = - minCasosRJ + maxCasosRJ
mediaCasosRJ = mean(casosNovosRJ)
medianaCasosRJ = median(casosNovosRJ)
quantilCasosRJ = quantile(casosNovosRJ, c(0.25,0.75), tipe=2)
AmplitudeQCasosRJ = quantilCasosRJ[1] - quantilCasosRJ[2]
desvioPadraoCasosRJ = sd(casosNovosRJ)

# obitos Novos
minMortesRJ = min(obitosNovosRJ)
maxMortesRJ = max(obitosNovosRJ)
AmplitudeMortesRJ = - minMortesRJ + maxMortesRJ
mediaMortesRJ = mean(obitosNovosRJ)
medianaMortesRJ = median(obitosNovosRJ)
quantilMortesRJ = quantile(obitosNovosRJ, c(0.25,0.75), tipe=2)
AmplitudeQMortesRJ = quantilMortesRJ[1] - quantilMortesRJ[2]
desvioPadraoMortesRJ = sd(obitosNovosRJ)

# Espirito Santo
# casos Novos
minCasosES = min(casosNovosES)
maxCasosES = max(casosNovosES)
AmplitudeCasosES = - minCasosES + maxCasosES
mediaCasosES = mean(casosNovosES)
medianaCasosES = median(casosNovosES)
quantilCasosES = quantile(casosNovosES, c(0.25,0.75), tipe=2)
AmplitudeQCasosES = quantilCasosES[1] - quantilCasosES[2]
desvioPadraoCasosES = sd(casosNovosES)

# obitos Novos
minMortesES = min(obitosNovosES)
maxMortesES = max(obitosNovosES)
AmplitudeMortesES = - minMortesES + maxMortesES
mediaMortesES = mean(obitosNovosES)
medianaMortesES = median(obitosNovosES)
quantilMortesES = quantile(obitosNovosES, c(0.25,0.75), tipe=2)
AmplitudeQMortesES = quantilMortesES[1] - quantilMortesES[2]
desvioPadraoMortesES = sd(obitosNovosES)



#-------------------------------
# item (f)

# Histograma (amplitude das classes eh dada pela regra de Sturges)
# MINAS GERAIS
hist(casosNovosMG,
     xlab = "Numero de casos",
     ylab = "Frequencia",
     col = "blue",
     main = "Histograma de casos diarios registrados em MG")
hist(obitosNovosMG,
     xlab = "Numero de obitos",
     ylab = "Frequencia",
     col = "blue",
     main = "Histograma de obitos diarios registrados em MG")

# SÃO PAULO
hist(casosNovosSP,
     xlab = "Numero de casos",
     ylab = "Frequencia",
     col = "red",
     main = "Histograma de casos diarios registrados em SP")
hist(obitosNovosSP,
     xlab = "Numero de obitos",
     ylab = "Frequencia",
     col = "red",
     main = "Histograma de obitos diarios registrados em SP")


# Rio de Janeiro
hist(casosNovosRJ,
     xlab = "Numero de casos",
     ylab = "Frequencia",
     col = "green",
     main = "Histograma de casos diarios registrados em RJ")
hist(obitosNovosRJ,
     xlab = "Numero de obitos",
     ylab = "Frequencia",
     col = "green",
     main = "Histograma de obitos diarios registrados em RJ")

# Espirito Santo
hist(casosNovosES,
     xlab = "Numero de casos",
     ylab = "Frequencia",
     col = "purple",
     main = "Histograma de casos diarios registrados em ES")
hist(obitosNovosES,
     xlab = "Numero de obitos",
     ylab = "Frequencia",
     col = "purple",
     main = "Histograma de obitos diarios registrados em ES")

# Boxplot
# Casos Novos
casos = data.frame(MG = casosNovosMG,
                   SP = casosNovosSP,
                   ES = casosNovosES,
                   RJ = casosNovosRJ)
boxplot(casos, 
        ylab = "Numero de casos", 
        main = "Boxplot do numero de casos diarios",
        col = c("blue", "red", "purple","green"))

# Obitos Novos
obitos = data.frame(MG = obitosNovosMG,
                   SP = obitosNovosSP,
                   ES = obitosNovosES,
                   RJ = obitosNovosRJ)
boxplot(obitos, 
        ylab = "Numero de Obitos", 
        main = "Boxplot do numero de obitos diarios",
        col = c("blue", "red", "purple","green"))


#-------------------------------
# Extra: Fizemos uma tabela de frequancia por classes

# MINAS GERAIS
TabelaCasosMG = fdt(casosNovosMG, breaks = "Sturges")
TabelaCasosMG

TabelaObitosMG = fdt(obitosNovosMG)
TabelaObitosMG

# SÃO PAULO
TabelaCasosSP = fdt(casosNovosSP)
TabelaCasosSP

TabelaObitosSP2 = fdt(obitosNovosSP)
TabelaObitosSP2

# ESPÍRITO SANTO
TabelaCasosES = fdt(casosNovosES)
TabelaCasosES

TabelaObitosES = fdt(obitosNovosES)
TabelaObitosES

# RIO DE JANEIRO
TabelaCasosRJ = fdt(casosNovosRJ)
TabelaCasosRJ

TabelaObitosRJ = fdt(obitosNovosRJ)
TabelaObitosRJ