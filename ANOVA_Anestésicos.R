library(ggplot2)
library(agricolae)
library(readxl)

library(readxl)
EUG <- read_excel("Ydelsa.xlsx", sheet = "Hoja1")
View(EUG)

View(EUG)
EUG

#NORMALIDAD
nortest::ad.test(EUG$I.A)
nortest::ad.test(EUG$II.A)
nortest::ad.test(EUG$III.A)
nortest::ad.test(EUG$I.R)
nortest::ad.test(EUG$II.R)
nortest::ad.test(EUG$A.Total) #No
nortest::ad.test(EUG$R.Total). #no

#HOMOCEDASTICIDAD
bartlett.test(I.A ~ ETAPA, data=EUG)
bartlett.test(II.A ~ ETAPA, data=EUG)
bartlett.test(III.A ~ ETAPA, data=EUG)
bartlett.test(I.R ~ ETAPA, data=EUG)
bartlett.test(II.R ~ ETAPA, data=EUG)
bartlett.test(III.R ~ ETAPA, data=EUG)
bartlett.test(A.Total ~ ETAPA, data=EUG) #no
bartlett.test(R.Total ~ ETAPA, data=EUG) #si


#ANOVA
anova.I.A <- lm(I.A ~ ETAPA, data=EUG)
summary(anova.I.A) 
kruskal.test(II.A ~ ETAPA, data=EUG)
kruskal.test(III.A ~ ETAPA, data=EUG)
anova.I.R <- lm(I.R ~ ETAPA, data=EUG)
summary(anova.I.R) 
kruskal.test(II.R ~ ETAPA, data=EUG)
kruskal.test(III.R ~ ETAPA, data=EUG)

kruskal.test(A.Total ~ ETAPA, data=EUG) #Si
kruskal.test(R.Total ~ ETAPA, data=EUG) #Si

#POSTHOC

gl <- df.residual(anova.I.A)
cm <- deviance(anova.I.A)/gl
Homogeneidad <- LSD.test(EUG$I.A, EUG$ETAPA, gl, cm)
Homogeneidad

ESP1 <- as.numeric(EUG$II.A)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(EUG$III.A)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

gl <- df.residual(anova.I.R)
cm <- deviance(anova.I.R)/gl
Homogeneidad <- LSD.test(EUG$I.R, EUG$ETAPA, gl, cm)
Homogeneidad

ESP1 <- as.numeric(EUG$II.R)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(EUG$III.R)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

#Total

ESP1 <- as.numeric(EUG$A.Total)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(EUG$R.Total)
TRA <- as.factor(EUG$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

################################################################

MS <- read_excel("Desktop/MS_Anestésicos/Ydelsa.xlsx", 
                  sheet = "Hoja2")

MS <- read_excel("Ydelsa.xlsx", sheet = "Hoja2") #Estamos trabajando con este

View(MS)
MS

#NORMALIDAD
nortest::ad.test(MS$I.A)
nortest::ad.test(MS$II.A)
nortest::ad.test(MS$III.A)
nortest::ad.test(MS$I.R)
nortest::ad.test(MS$II.R)
nortest::ad.test(MS$III.R)

nortest::ad.test(MS$A.Total) #Si
nortest::ad.test(MS$R.Total) #No

#HOMOCEDASTICIDAD
bartlett.test(I.A ~ ETAPA, data=MS)
bartlett.test(II.A ~ ETAPA, data=MS)
bartlett.test(III.A ~ ETAPA, data=MS)
bartlett.test(I.R ~ ETAPA, data=MS)
bartlett.test(II.R ~ ETAPA, data=MS)
bartlett.test(III.R ~ ETAPA, data=MS)

bartlett.test(A.Total ~ ETAPA, data=MS) #Si
bartlett.test(R.Total ~ ETAPA, data=MS) #Si

#ANOVA
kruskal.test(I.A ~ ETAPA, data=MS)
kruskal.test(II.A ~ ETAPA, data=MS)
kruskal.test(III.A ~ ETAPA, data=MS)
anova.I.R <- lm(I.R ~ ETAPA, data=MS)
summary(anova.I.R) 
kruskal.test(II.R ~ ETAPA, data=MS)
anova.III.R <- lm(III.R ~ ETAPA, data=MS)
summary(anova.III.R) 

anova.ATotal <- lm(A.Total ~ ETAPA, data=MS)
summary(anova.ATotal)              #No
kruskal.test(R.Total ~ ETAPA, data=MS) #Si


#POSTHOC

ESP1 <- as.numeric(MS$I.A)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(MS$II.A)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(MS$III.A)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

gl <- df.residual(anova.I.R)
cm <- deviance(anova.I.R)/gl
Homogeneidad <- LSD.test(MS$I.R, MS$ETAPA, gl, cm)
Homogeneidad

ESP1 <- as.numeric(MS$II.R)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

ESP1 <- as.numeric(MS$III.R)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1

##TOTAL

gl <- df.residual(anova.ATotal)
cm <- deviance(anova.ATotal)/gl
Homogeneidad <- LSD.test(MS$A.Total, MS$ETAPA, gl, cm)
Homogeneidad

ESP1 <- as.numeric(MS$R.Total)
TRA <- as.factor(MS$ETAPA)
DunnetESP1 <- kruskal(ESP1,TRA,group=TRUE,p.adj="holm")
DunnetESP1


############################ CORRELATION ################

# Datos
ETAPA <- c(20, 20, 20, 40, 40, 40, 60, 60, 60, 80, 80, 80, 100, 100, 100)
IA <- c(288, 315, 298, 193, 176, 191, 46, 52, 53, 39, 46, 40, 22, 23, 28)
IIA <- c(0, 0, 0, 0, 0, 0, 80, 114, 100, 105, 119, 108, 52, 54, 53)
IIIA <- c(0, 0, 0, 0, 0, 0, 197, 200, 215, 136, 152, 130, 70, 72, 76)
IR <- c(20, 24, 19, 18, 25, 15, 42, 12, 10, 15, 20, 23, 25, 24, 30)
IIR <- c(53, 55, 52, 49, 55, 50, 56, 53, 45, 70, 74, 80, 106, 116, 122)
IIIR <- c(195, 192, 189, 190, 195, 180, 221, 236, 205, 230, 233, 240, 299, 272, 282)

# Crear dataframe
datos <- data.frame(ETAPA, IA, IIA, IIIA, IR, IIR, IIIR)

# Calcular correlación de Pearson y Spearman con ETAPA
cor_pearson <- cor(datos, method = "pearson")["ETAPA", ]
cor_spearman <- cor(datos, method = "spearman")["ETAPA", ]

# Mostrar resultados
print("Correlación de Pearson con ETAPA:")
print(cor_pearson)

print("Correlación de Spearman con ETAPA:")
print(cor_spearman)


############################################################

library(ggplot2)
library(gridExtra)

# Crear dataframe
datos <- data.frame(ETAPA, IA, IIA, IIIA, IR, IIR, IIIR)

# Crear gráficos de dispersión con línea de tendencia lineal
plot1 <- ggplot(datos, aes(x=ETAPA, y=IA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IA")
plot2 <- ggplot(datos, aes(x=ETAPA, y=IIA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIA")
plot3 <- ggplot(datos, aes(x=ETAPA, y=IIIA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIIA")
plot4 <- ggplot(datos, aes(x=ETAPA, y=IR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IR")
plot5 <- ggplot(datos, aes(x=ETAPA, y=IIR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIR")
plot6 <- ggplot(datos, aes(x=ETAPA, y=IIIR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIIR")

# Mostrar gráficos en una cuadrícula
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)




#########################################

# R de eugenol

# Cargar librerías necesarias
library(ggplot2)
library(gridExtra)

# Datos
ETAPA <- c(20, 20, 20, 40, 40, 40, 60, 60, 60, 80, 80, 80, 100, 100, 100)
IA <- c(48, 55, 53, 38, 48, 42, 36, 46, 39, 17, 20, 24, 14, 18, 15)
IIA <- c(60, 95, 92, 79, 67, 68, 77, 98, 84, 29, 34, 31, 24, 31, 32)
IIIA <- c(80, 118, 112, 71, 110, 102, 90, 105, 107, 54, 40, 39, 36, 38, 31)
IR <- c(60, 92, 86, 96, 116, 100, 106, 100, 98, 151, 143, 147, 114, 117, 113)
IIR <- c(155, 180, 175, 220, 238, 229, 230, 238, 230, 236, 270, 268, 228, 207, 220)
IIIR <- c(382, 400, 390, 522, 530, 525, 592, 610, 598, 598, 595, 600, 538, 552, 555)

# Crear dataframe
datos <- data.frame(ETAPA, IA, IIA, IIIA, IR, IIR, IIIR)

# Calcular correlación de Pearson y Spearman con ETAPA
cor_pearson <- cor(datos, method = "pearson")["ETAPA", ]
cor_spearman <- cor(datos, method = "spearman")["ETAPA", ]

# Mostrar resultados
print("Correlación de Pearson con ETAPA:")
print(cor_pearson)

print("Correlación de Spearman con ETAPA:")
print(cor_spearman)

# Graficar dispersión con lí

######## 
# Grafico deEugenol 

# Cargar librerías necesarias
library(ggplot2)
library(gridExtra)

# Datos
ETAPA <- c(20, 20, 20, 40, 40, 40, 60, 60, 60, 80, 80, 80, 100, 100, 100)
IA <- c(48, 55, 53, 38, 48, 42, 36, 46, 39, 17, 20, 24, 14, 18, 15)
IIA <- c(60, 95, 92, 79, 67, 68, 77, 98, 84, 29, 34, 31, 24, 31, 32)
IIIA <- c(80, 118, 112, 71, 110, 102, 90, 105, 107, 54, 40, 39, 36, 38, 31)
IR <- c(60, 92, 86, 96, 116, 100, 106, 100, 98, 151, 143, 147, 114, 117, 113)
IIR <- c(155, 180, 175, 220, 238, 229, 230, 238, 230, 236, 270, 268, 228, 207, 220)
IIIR <- c(382, 400, 390, 522, 530, 525, 592, 610, 598, 598, 595, 600, 538, 552, 555)

# Crear dataframe
datos <- data.frame(ETAPA, IA, IIA, IIIA, IR, IIR, IIIR)

# 1️⃣ Graficar dispersión
plot1 <- ggplot(datos, aes(x=ETAPA, y=IA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IA")
plot2 <- ggplot(datos, aes(x=ETAPA, y=IIA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIA")
plot3 <- ggplot(datos, aes(x=ETAPA, y=IIIA)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIIA")
plot4 <- ggplot(datos, aes(x=ETAPA, y=IR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IR")
plot5 <- ggplot(datos, aes(x=ETAPA, y=IIR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIR")
plot6 <- ggplot(datos, aes(x=ETAPA, y=IIIR)) + geom_point() + geom_smooth(method="lm") + ggtitle("ETAPA vs IIIR")

# Mostrar gráficos en una cuadrícula
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=2)

# 2️⃣ Prueba de normalidad Shapiro-Wilk para cada variable
shapiro_test_results <- apply(datos, 2, shapiro.test)

# Imprimir los valores p de la prueba de Shapiro-Wilk
print("Prueba de normalidad Shapiro-Wilk:")
sapply(shapiro_test_results, function(x) x$p.value)

# 3️⃣ Elegir Pearson o Spearman según normalidad
cor_pearson <- cor(datos, method = "pearson")
cor_spearman <- cor(datos, method = "spearman")

print("Corrxelación de Pearson:")
print(cor_pearson["ETAPA", ])

print("Correlación de Spearman:")
print(cor_spearman["ETAPA", ])

#################

# Datos (copiá y pegá tal cual, o usá read.csv si tenés un archivo)
etapa <- c(20, 20, 20, 40, 40, 40, 60, 60, 60, 80, 80, 80, 100, 100, 100)
A1 <- c(188, 268, 257, 188, 225, 212, 203, 249, 230, 100, 94, 94, 74, 87, 78)
R1 <- c(597, 672, 651, 838, 884, 854, 928, 948, 926, 985, 1008, 1015, 880, 876, 888)
A2 <- c(288, 315, 298, 193, 176, 191, 323, 366, 368, 280, 317, 278, 144, 149, 157)
R3 <- c(268, 271, 260, 257, 275, 245, 319, 301, 260, 315, 327, 343, 430, 412, 434)

# Cálculo del coeficiente de correlación de Pearson y p-valor

# Correlación de ETAPA con A1, A2, R1, R3
cat("Correlación entre ETAPA y A1:\n")
cor.test(etapa, A1, method = "pearson")

cat("\nCorrelación entre ETAPA y A2:\n")
cor.test(etapa, A2, method = "pearson")

cat("\nCorrelación entre ETAPA y R1:\n")
cor.test(etapa, R1, method = "pearson")

cat("\nCorrelación entre ETAPA y R3:\n")
cor.test(etapa, R3, method = "pearson")

