install.packages("gtrendsR")
library(gtrendsR)

cov_21_may <- gtrends(keyword = c("covid"),time = "today 12-m", 
                         low_search_volume=TRUE ,geo=c("IT", "NP","ES","GB", "JP","AR"), onlyInterest = FALSE)

cov_21_may_all <- gtrends(keyword = c("covid"),time = "today 12-m", 
                          low_search_volume=TRUE , onlyInterest = FALSE)

## Cada uno de estos objetos es una lista, esdecir los objetos son listas de data_frames

# En el siguiente link explica como se obtienen los datos https://support.google.com/trends/answer/4365533?hl=es

## para acceder a cada tabla o cada data frame 
#cada data frame hace referencia a uno de los siguientes temas:

#Serie temporal de la relevancia de la busqueda escala semanal

cov_21_may_all$interest_over_time

#Por pais
cov_21_may_all$interest_by_country

#región y ciudad
cov_21_may_all$interest_by_city
cov_21_may_all$interest_by_dma

#temas relacionados
cov_21_may_all$related_topics

#Consultas relacionadas
cov_21_may_all$related_queries

### Responder las preguntas generales con la base interest_over_time global y de paises

## Caracterizar los temas relacionados y las consultas relacionadas  en cada uno de los paises propuestos 

## Evaluar si hay diferencias entre los paises respecto a la busqueda interest_by_country, tener en cuenta que la mnedida es relativa

## Caracterizar la dinámica temporal global y en cada pais


