


library(tidyverse)
library(ggpubr)

bicis <- read.table("datos/bicis.txt", header = T, sep = "\t")

head(bicis)

cols=c(casual="#d1495b", registrados="#00798c")

accum_rentas <- bicis %>% rename("totales"=cnt) %>% 
ggplot()+
  geom_step(aes(totales, color="totales"), stat="ecdf")+
  geom_step(aes(registrados, color="registrados"), stat="ecdf")+
  geom_step(aes(casual, color="casual"), stat="ecdf")+
  scale_x_continuous(n.breaks = 15)+
  scale_y_continuous(n.breaks = 10)+
  labs(color="tipo de renta", 
       y="Frecuencia relativa acumulada",
       x="Rentas diarias")+
  theme_light()

boxplot_rentas <- bicis %>% pivot_longer(cols = c(cnt,registrados,casual)) %>%
  mutate(name=recode(name,cnt="totales") )%>% 
ggplot()+
  geom_boxplot(aes(name,value,fill=name))+
  scale_y_continuous(n.breaks = 10)+
  labs(fill="tipo de renta", 
        y="Rentas diarias",
       x="tipo de renta")+
  theme_light()

ggarrange(accum_rentas, boxplot_rentas, nrow=1)

#3
bicis %>% group_by(estacion) %>% summarise(
  Media=mean(cnt),
  DE=sd(cnt),
  n=n(), 
  cv=sd(cnt)/mean(cnt)*100
)

bicis %>% group_by(tiempo) %>% summarise(
  Media=mean(cnt),
  DE=sd(cnt),
  n=n()
)


boxplot_tiempo <- bicis %>%  
  ggplot()+
  geom_boxplot(aes(as.factor(tiempo),cnt,fill=as.factor(tiempo)))+
  scale_y_continuous(n.breaks = 10)+
  scale_fill_brewer(palette="Dark2")+
  labs(fill="tiempo", 
       y="Rentas diarias",
       x="tiempo")+
  theme_light()

tab <- xtabs(~tiempo + estacion, data = bicis); tab

chisq.test(tab)

laborable= bicis[bicis$dia_lab==1, "cnt"]
no_laborable= bicis[bicis$dia_lab==0, "cnt"]

t.test(x=laborable, y=no_laborable, alternative="two.sided", mu=0, 
       paired=FALSE, var.equal=TRUE, conf.level=0.95)

bicis <- bicis %>% mutate(
  estacion_ = dplyr::recode_factor(as.factor(estacion),
                            '1' = "inv",'2' = "prim",'3' = "ver",'4' = "oto"),
  atemp2=atemp^2)

mod_clas <- lm(cnt~estacion_, data=bicis)
anova(mod_clas)
summary(mod_clas)
LSD.test(mod_clas, "estacion_", console = TRUE)

library(GGally)
bicis %>% select(c(temp,atemp,hum,vel_viento,cnt))%>%
  GGally::ggpairs(lower = list(continuous = wrap("smooth", alpha = 0.5, size=0.2)))+
  theme_bw()


mod_reg <- lm(cnt~atemp+hum+atemp2, data = bicis)
anova(mod_reg)
summary(mod_reg)



bicis$pred_mod_reg <- predict(mod_reg, bicis)

ggplot(bicis, aes(x=atemp, y=pred_mod_reg, color=estacion_))+
  geom_point()+
  labs(color="Estación",y='Rentas diarias', x='Sensación térmica')+
  geom_point(aes(x=atemp, y=cnt),size=0.1)+
  theme_light()+
  scale_y_continuous(n.breaks = 10)+
  scale_x_continuous(n.breaks = 10)


mod_gral <- lm(cnt~estacion_+atemp+estacion_:atemp, data = bicis)
anova(mod_gral)
summary(mod_gral)

bicis$pred_mod_gral <- predict(mod_gral, bicis)

ggplot(bicis, aes(x=atemp, y=pred_mod_gral, color=estacion_))+
  geom_point()+
  labs(color="Estación",y='Rentas diarias', x='Sensación térmica')+
  geom_point(aes(x=atemp, y=cnt),size=0.1)+
  theme_light()+
  scale_y_continuous(n.breaks = 10)+
  scale_x_continuous(n.breaks = 10)

plot(mod_gral)


