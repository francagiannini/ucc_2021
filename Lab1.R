#Encuesta de trabajadores en la industria de software 
#Desde 2014 sysarmy lleva a cabo las Encuestas de Sueldos en la región, 
#abarcando a Argentina y el resto de Latinoamérica. 
#Trabajaremos sobre los datos de los salarios argentinos

##Objetivo:
#Introducir R vía RStudio indentificar tipos de variables. Generar tablas y medidas  resumenes y gráficos descriptivos 


#leer tabla de datos
#setwd("C:/Users/franc/Dropbox/Franca/UCAsueldossys")

#librerias
#install.packages("readxl")

library(readxl)

sueldossys <- read_excel("datos/sys2021.xlsx")

View(sueldossys)

#Población, n de la muestra
dim(sueldossys)

#Primer variable Actividad principal
class(sueldossys$`Actividad principal`)

act <- table(sueldossys$`Actividad principal`)

plot(act)

colors <- c("green3", "purple", "grey") 

barplot(act, col=colors )

pie(act, col = colors)

#Segunda variable edad 

class(sueldossys$Tengo)

sueldossys$Tengo <- as.integer(sueldossys$Tengo)

summary(sueldossys$Tengo)

plot(sueldossys$Tengo)

hist(sueldossys$Tengo)

which(sueldossys$Tengo>80)

##depuracion 

library(tidyverse)

sueldossys_dep <- sueldossys %>% filter(Tengo < 80)

hist(sueldossys_dep$Tengo)

sueldossys_dep %>%
  group_by(`Actividad principal`) %>%
  summarise(edad_promedio = mean(Tengo), 
            sd = sd(Tengo),
            cv = sd(Tengo)/mean(Tengo)*100,
            n=n())

#library(ggplot2)

ggplot(data=sueldossys_dep, aes(Tengo)) +
  geom_histogram(binwidth = 2) +
  ylab("Frecuencia absoluta") +
  xlab("Edad")

ggplot(sueldossys_dep, aes(Tengo, fill=`Actividad principal`)) +
  geom_histogram(binwidth = 2)+
  ylab("Frecuencia absoluta")+
  xlab("Edad")

ggplot(sueldossys_dep, aes(`Actividad principal`,Tengo, color=`Actividad principal`))+
  geom_boxplot()+
  ylab("Edad")+
  xlab("Actividad")


ggplot(sueldossys_dep, aes(`Actividad principal`, fill=`Actividad principal`))+
  geom_bar()+
  ylab("Frecuencia absoluta")+
  xlab("Actividad")

##Lugar de trabajo
provincias <- as.data.frame(table(sueldossys$`Dónde estás trabajando`))

colnames(provincias) <- c("Provincia", "Frec abs")

ggplot(sueldossys_dep, aes(x=`Dónde estás trabajando`, fill=`Dónde estás trabajando`))+
  geom_bar()+
  ylab("Frecuencia absoluta")+
  xlab("Provincia")

##sueldos
summary(sueldossys$`Salario mensual o retiro NETO (en tu moneda local)`)

hist(sueldossys$`Salario mensual o retiro NETO (en tu moneda local)`)

salario_neto <- sueldossys %>% 
  filter(`Salario mensual o retiro NETO (en tu moneda local)` < 500000) %>% 
  select(c(`Salario mensual o retiro NETO (en tu moneda local)`,`Pagos en dólares`)) %>%
  rename(neto =`Salario mensual o retiro NETO (en tu moneda local)`)

salario_neto %>%
  group_by(`Pagos en dólares`) %>% 
  summarise(`neto media`= mean(neto), 
            sd = sd(neto), 
            cv = sd(neto)/mean(neto)*100,
            n = n())  

ggplot(salario_neto, aes(neto, fill=`Pagos en dólares`))+
  geom_histogram(binwidth = 10000)+
  ylab("Frecuencia absoluta")+
  xlab("Salario bruto mensual")


ggplot(salario_neto, aes(`Pagos en dólares`,neto, color=`Pagos en dólares`))+
  geom_boxplot()+
  ylab("salario neto")+
  xlab("Tipo de cobro")

## función de distribución acumulada  

salario_neto <-  salario_neto %>% 
  mutate(dolarizado=recode(`Pagos en dólares`,.missing="No", .default="Si")) %>% 
  filter(neto>10000)

table(salario_neto$dolarizado)

ggplot(salario_neto, aes(neto, fill=dolarizado, col=dolarizado))+
  geom_histogram() +
  ylab("Frecuencia")+
  xlab("Salario neto mensual")

## summary

salario_neto %>% group_by(dolarizado) %>% 
  summarise(neto_medio = mean(neto),
            neto_DE = sd(neto),
            neto_CV= sd(neto)/mean(neto)*100,
            neto_P50=median(neto),
            Q1_P25=quantile(neto, probs=0.25),
            Q3_P75=quantile(neto, probs=0.75),
            n=n())


ggplot(salario_neto, aes(neto, fill=`dolarizado`, col=`dolarizado`))+
  geom_step(stat="ecdf") +
  ylab("Frecuencia acumulada")+
  xlab("Salario neto mensual")


ggplot(salario_neto, aes(dolarizado,neto, fill=dolarizado))+
  geom_boxplot() +
  ylab("Frecuencia")+
  xlab("Salario neto mensual")

# Para que categoría son más variables los sueldos?

# Que probabilidad existe de que una persona que trabaja en la industria del software 
# cobre un sueldo de alguna manera dolarizado?

salario_neto %>% count(dolarizado)

# y que cobre más de 100.000 pesos?

salario_neto %>% count(neto>100000)

# y que cobre más de  100.000 pesos DADO que tiene el sueldo dolarizado

salario_neto %>% group_by(dolarizado) %>% count(neto>100000)

# y que cobre más de  100.000 pesos DADO que NO tiene el sueldo dolarizado

# A trabajar un rato solitos un rato!!! 40 min 

# Consignas

# Caracterizar el salario neto segun la variable Me identifico, 
# Intentar responder si existe brecha salarial

# calcular la probabilidad de que una persona que trabaja en la industria del software en argentina sea Mujer Cis

#Ayudita depurando

salario_neto_gen <- sueldossys %>%
  rename(neto =`Salario mensual o retiro NETO (en tu moneda local)`) %>%
  filter(neto< 500000 & neto>10000) %>%
  select(c(neto,`Me identifico`)) %>%
  mutate(genero = fct_lump(`Me identifico`, n = 2, other_level = "Otre"))


