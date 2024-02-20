nuevo_dir <- "C:/practica_spearheads"
setwd(nuevo_dir)
install.packages("readxl")

spear <- read_excel("C:/spearheadsexcel/spearheads.xlsx")
str(spear)
class(spear)

#1
spear <- as.data.frame(spear)
#2
names(spear)[names(spear) == "Mat"] <- "Materiales"
names(spear)[names(spear) == "Con"] <- "Contexto"
names(spear)[names(spear) == "Loo"] <- "Loop"
names(spear)[names(spear) == "Peg"] <- "Remache"
names(spear)[names(spear) == "Cond"] <- "Conservación"
names(spear)[names(spear) == "Date"] <- "Fecha"
names(spear)[names(spear) == "Maxle"] <- "Longitud_max"
names(spear)[names(spear) == "Socle"] <- "Longitud_encaje"
names(spear)[names(spear) == "Maxwi"] <- "Ancho_max"
names(spear)[names(spear) == "Upsoc"] <- "Ancho_encaje"
names(spear)[names(spear) == "Maxwit"] <- "Ancho_max_encjaes"
names(spear)[names(spear) == "Weight"] <- "Peso"

View(spear)
#3
spear$Contexto = factor(spear$Contexto, levels = c('1','2','3'), labels=c("s/c","Habitacional","Funerario"))
spear$Conservación=factor(spear$Conservación, levels = c('1','2','3','4'), labels=c("Excelente","Bueno","Regular","Malo"))
spear$Remache=factor(spear$Remache, levels = c('1','2'), labels=c("Si","No"))
spear$Materiales=factor(spear$Materiales, levels = c('1','2'), labels=c("Bronce","Hierro"))
View(spear)
#4
freq.Mat=table(spear$Materiales)
freq.Con=table(spear$Contexto)
freq.Co=table(spear$Conservación)
#5
TablaCruzada_Materiales_Contexto = xtabs(~ Materiales + Contexto, data = spear )
TablaCruzada_Materiales_Conservación = xtabs(~ Materiales+ Conservación, data = spear)
#6
TablaPorcentaje_Materiales = prop.table(freq.Mat) *100
TablaPorcentaje_Contexto = prop.table(freq.Con) *100
TablaPorcentaje_Conservacion = prop.table(freq.Co) *100
#7
porcentajes_Mat_Con = prop.table(TablaCruzada_Materiales_Contexto, margin = 1) *100
porcentajes_Mat_Co = prop.table(TablaCruzada_Materiales_Conservación, margin = 1) *100
#8
GráficoBarras_Conservación = barplot(freq.Co,
                                      main = "Frecuencia de Conservación",
                                      xlab = "Grado de Conservación",
                                      ylab = "Frecuencia (%)",
                                      col = "red")

GráficoBarras_Contexto = barplot(freq.Con,
                                   main = "Frecuencia de Contextos",
                                   xlab = "Contextos",
                                   ylab = "Frecuencia (%)",
                                   col = "red")
#9
GráficoBarras_Materiales = barplot(freq.Mat,
                                    horiz = TRUE,
                                    main = "Frecuencia de materiales",
                                    xlab = "Frecuencia (%)",
                                    ylab = "Tipos de materiales",
                                    col = "brown")
#10
barplot(TablaCruzada_Materiales_Conservación,
        beside = TRUE,
        main = "Frecuencia de grado de conservación por tipo de material",
        xlab = "Grado de conservación",
        ylab = "Frecuencia (%)",
        col = c("green","red"),
        legend = rownames(TablaCruzada_Materiales_Conservación))
#11
pie(freq.Co,
    main = "Gráfico de los grados de conservación",
    col = c("brown","pink","orange","red"),
    labels = paste(names(freq.Co),"(", (TablaPorcentaje_Conservacion),"%)"))
#12
Histograma_VContinuas = spear[sapply(spear, is.numeric)]
windows(width = 10, height = 10)
histograma_probabilidad = hist(unlist(Histograma_VContinuas), 
                               main = "Histograma de Probabilidad de Variables Continuas",
                               xlab = "Valor", 
                               prob = TRUE) 
