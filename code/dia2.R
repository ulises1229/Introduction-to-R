## Alberto Prado
## 18 enero 2022
## Curso Intersemestral día 2


rm(list=ls()) # Borrar memoría

setwd("~/Desktop/Curso R/2021-1") # Lugar de trabajo

mtcars <-  read.csv("mtcars.csv") # si no funciona tratar read.csv2
str(mtcars)

rownames(mtcars) <- mtcars$X
rownames(mtcars)

mtcars <- mtcars[,-1] # Eliminar columna X

names(mtcars) <- c("mpg","Num.Cil", "Desp", "CF","Eje",
                   "Peso","Seg.4.mi","forma","Transmisión",
                   "Vel","Carb")
names(mtcars)

mtcars <- mtcars[,c(1,4,2,3,5,6,7,8,9,10,11)]

# Histograma
hist(mtcars$mpg)

# Gráfico XY
plot(x=mtcars$Peso, y=mtcars$CF)
plot(mtcars$CF~mtcars$Peso) # Usando virgulilla

plot(x=mtcars$Peso, y=mtcars$CF, col= mtcars$Transmisión+1)
str(mtcars)

plot(CF~Peso, data=mtcars)

## Boxplot o Caja y bigote
boxplot(mtcars$Peso~mtcars$Transmisión)

# cambiar etiquetas de los niveles de un factor
mtcars$Transmisión <- factor(mtcars$Transmisión, 
                             labels=c("automático","manual"))

boxplot(mtcars$Peso~mtcars$Transmisión, col=c(4,2))
levels(mtcars$Transmisión)

mtcars$Transmisión <- factor(mtcars$Transmisión, 
                             levels=c("manual","automático"))


### Ejercicio 2
# Cajas y bigote CF en funcion de forma del motor

boxplot(CF~forma,data=mtcars)
boxplot(mtcars$CF~mtcars$forma)
# prueba de T student
t.test(CF~forma,data=mtcars)
# varianza
var(mtcars[mtcars$forma=="0","CF"])
var(mtcars[mtcars$forma=="1","CF"])
# Pueba de homogeniedad de varianzas
bartlett.test(CF~forma,data=mtcars)
# Prueba de normalidad
shapiro.test(mtcars[mtcars$forma=="0","CF"])
shapiro.test(mtcars[mtcars$forma=="1","CF"])

# Prueba no parametrica 
wilcox.test(CF~forma,data=mtcars)
# Ho: No hay differencia (p >=0.05)
# Ha: Sí hay differencia (p < 0.05)

#####################################
#####################################
### Distribuciones
# Cleaveland plot por grupo
dotchart(mtcars$CF,groups=mtcars$Transmisión)

# Datos cars.csv
cars <- read.csv2("cars.csv")
# Cleavlandplot
dotchart(cars$speed)
dotchart(cars$dist)

# Histogramas
hist(cars$speed)
hist(cars$dist)

# Dibujar curva de distribución (densidad)
plot(density(cars$speed))
plot(density(cars$dist))

### Instalar la librería 
# install.packages("MASS")
## Cargar librería
library(MASS)

help(package = "MASS")

# Obtener parametros de curva PDF (funcion de densidad de probabilidad)
# Velocidad
fitdistr(cars$speed,"normal")
# Distancia
fitdistr(cars$dist,"lognormal")

### Usar Visualize para visualizar distribución
library(visualize)
# Velocidad
visualize.norm(mu=15.4,sd=5.2345, stat=25, section = "upper")
visualize.norm(mu=15.4,sd=5.2345, stat=c(10,25), section = "bounded")

# Distancia
visualize.lnorm(meanlog = 3.53591, sdlog=0.78668, 
                stat=10, section="lower")


### Ejercicio 8
lluvia <- read.csv("lluvia.csv")
hist(lluvia$mm.lluvia)
fitdistr(lluvia$mm.lluvia,"exponential")
visualize.exp(theta=0.90074,stat=2,section="upper")

####### Correlaciones
# Covarianza
cov(cars$speed,cars$dist)
# Correlación
cor(cars$speed,cars$dist)

#  Cargar advertising
adv <- read.csv("advertising.csv")
str(adv)
cor(adv)


##################################
#### Modelo lineal

lm1 <- lm(cars$dist~cars$speed)
lm1
summary(lm1)
plot(cars$dist~cars$speed)
abline(a=-17.5791,b=3.9324,lty=3, lwd=4, col="blue")
text(x=6,y=100,"r2 = 0.64")
text(x=10, y= 80, "y= -17.58 + 3.93x")


############
str(adv)
# modelo con dos variables indenpendientes

mm <- lm(Sales~TV+Radio, data=adv)
mm
summary(mm)

############
# Ejercicio 10 

M1 <- lm(Sales~TV, data=adv)
summary(M1)
# r2=0.81

M2<- lm(Sales~Radio, data=adv)
summary(M2)
# r2=0.12

M3<- lm(Sales~Newspaper, data=adv)
summary(M3)
# r2=0.02

### Graficar modelo TV
plot(Sales~TV, data=adv, pch=25, col="coral")
abline(a=6.975,b=0.0555,col="blue",lwd=3, lty=3)
text(x=30,y=25, "r2=0.81")






