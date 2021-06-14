# Calcular probabilidad
library(ggplot2)
library(ordinal)

Calcular_probabilidad = function(location, scale, x1,x2){
  x <- seq(-5,300,length=100)
  hx <- dgumbel(x,location=location,scale = scale)
  
  #location es alpha y scale es beta
  gum <- data.frame(x,hx)
  
  funcShaded <- function(x) {
    hx <- dgumbel(x,location=location,scale = scale)
    hx[x < x1 | x > x2] <- NA
    return(hx)
  }
  
  area <- pgumbel(x2, location ,scale ) - pgumbel(x1, location ,scale)
  result <- paste("P(",x1,"< X <",x2,") =",
                  signif(area, digits=3))
  
  ggplot(gum, aes(x=x, y=hx))+
    stat_function(fun=funcShaded, geom="area", fill="#84CA72", alpha=0.8) +
    geom_line()+
    labs(x="Q", y="f(x)", title="Distribución Gumbel", subtitle = result)
}

Calcular_probabilidad(location=37.327, scale=21.444, x1=130, x2=Inf)

