# datos hidrologicos ejercicio explorativo

inp <- read.csv("FDC.csv", na.strings="")
inp <- read.csv("FDC.csv")

head(inp)
dim(inp)

inp[!complete.cases(inp),]


plot(inp[,2], type = "l", col="green",
     
     main = "Tiempo de caudal",
     xlab = ("Río Estrella"),
     ylab = ("Río Banano")
)

lines(inp[,3],col="blue")

summary(inp[,2:3])
hist(inp[,2],
     main = "Histograma Río Estrella",
     xlab = ("Rango absoluto"),
     ylab = ("Frecuencia"),
     col="yellow"
)
hist(inp[,3],
     main = "Histrograma Río Banano",
     xlab = ("Rango absoluto"),
     ylab = ("Frecuencia"),
     col="brown"
)

names(inp) <- c("fecha", "Estrella","Banano")
attach(inp)
plot(Estrella)

Tempdate <- strptime(inp[,1], format= "%d/%m/%Y")

MAQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%Y"), FUN = sum) 
MAQ_Banano <- tapply(Banano, format(Tempdate, format = "%Y"), FUN = sum)
write.csv(rbind(MAQ_Estrella, MAQ_Banano),file= "MAQ.csv")

plot(MAQ_Banano,ylim = c(100,3000),
     main = "Valores anuales de los caudales",
     xlab = ("Meses"),
     ylab = ("Años"),
     col="purple"
)
lines(MAQ_Estrella, col = 2)

MMQ_Estrella <- tapply(Estrella, format(Tempdate, format = "%m"), FUN = sum)   
MMQ_Banano <- tapply(Banano, format(Tempdate, format = "%m"), FUN = sum)   

#Analisis de correlación

corinp <- cor(inp[,2:3], method= "spearman")

plot(inp[,2], inp[,3],
     main = "Coeficiente de correlación",
     xlab = ("Estrella"),
     ylab = ("Banano"),
     col="blue"
)

inp.lm <- lm(inp[,2] ~ inp[,3], data=inp)
summary(inp.lm)