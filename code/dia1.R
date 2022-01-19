# Alberto Prado
# 17 agosto 2021
# Curso Introducción R
# Version R 3.5.1

rm(list=ls()) ## Eliminar todas las listas
## Este comando asegura que empazmos con el ambiente R vacío

setwd("~/Desktop/Curso R/2021-1")

a <- 1
b <- 7

## Funciones 
## Una función es una secuencia de líneas que realizan una acción específica.
## Algunas funciones vienen ya con la librería base, miles de otras se pueden obtener al 
## descargar nuevas librerías. Otra opción es crear tus propias funciones.

### ¿Como crear una función?
### Usando la función "function" 
### Le asignamos un nombre a nuestra función
### Tenemos que especificar ¿qué va recibir como input?
### ¿qué va a hacer con ese input? 
### ¿qué me va a regresar?

elevar.cuadrado <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}	

f_a_c <- function(temp_f){
        temp_c = (temp_f-32)*5/9
        return(temp_c)
}

f_a_c(120)

suma_resta <- function(x,y,z){
     res = (x+y) - z 
     return(res)
}

suma_resta(10,3,2)

ejercicio_1 <- function(a,b) {
  res= sqrt(a)*sqrt(b)
  return(res)
}

ejercicio_1(1,2)

mul_raiz <- function(x,y){
  res=(sqrt(x))*(sqrt(y))
  return(res)
  }

mul_raiz(3,2)


#########################
iris <- read.csv("iris.csv")

str(iris)
levels(iris$Species) #conocer niveles de un factor
plot(iris)


dotchart(iris$Petal.Length, groups=iris$Species) # Cleaveland plot

plot(x=iris$Sepal.Length,y=iris$Sepal.Width,
     col=iris$Species, xlab="Longitud del pétalo (cm)",
     main="Mi gráfica R", xlim=c(0,10))















