#Script Proyecto Final 
#Grupo 9: Denisse Ichau, Tamia Sisa, Jeniffer Valbuena

#Descargar paquetes----
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dyplr")

#Cargar paquetes----
library(tidyverse)
library(openxlsx)
library(ggplot2)
library(tibble)

#Crear tibble empresas de balance_2014----

balances_2014 <- read.xlsx("Data/balances_2014.xlsx",detectDates=TRUE)
balance_2014 <- data.frame()

#Manipular base y crear dataframe 


#Visualización balances 2014
str(balances_2014)

#Tabla resumen----

#Grafico 1----

#Grafico 2----


