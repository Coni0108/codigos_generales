install.packages("DT")
install.packages("stargazer")
install.packages("moments")
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggthemes)
library(forcats)
library(patchwork)
library(moderndive)
library(plotly)
library(rgl)
library(gganimate)

EME %>%
  filter(sector!="NA") %>%
  ggplot(aes(x=Ganancia_2018, y=sector, fill=sector)) +
  geom_col() +
  theme(axis.title.x = element_text(vjust=0.1,face="bold",size=rel(1.5))) +
  theme(axis.title.y = element_text(vjust=0.1, face="bold",size=rel(1.5))) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())+
  theme(axis.text.x = element_text(face="bold",colour="black", size=rel(1)),
        axis.text.y = element_text(face="bold",colour="black", size=rel(0.9)
                                   , angle=0, hjust=0.5)) +
  ylab("Sector Industrial")+
  xlab("Ganancias Año 2018")+
  ggtitle("Ganancias por sector Industrial 2018")+
  theme(plot.title = element_text(face = "bold", size = rel(1.5)))  +
  theme(legend.position="none")
# Importar dataset ----
EME <- read_excel("Base de datos Full EXCEL EME 6.xlsx") %>% 
  select(Enc_rph, region, sexo, nacionalidad, cine_EME, motivacion_eme,
         financiamiento_inicial, c1_caenes_red, c3_h, c3_m, c6_1, d7_2, Registro_UE,
         e9, f4_trabajadores,capacitacion, grupo_edad, gasto_anual)


# GRÁFICOS ----


## Nivel de Educación por Sexo HISTOGRAMA
ggplot(EME, aes(educ)) +
  geom_bar() +
  facet_grid(~sexo) +
  xlab("Nivel de Educación") +
  ggtitle("Nivel de Educación según Sexo") +
  theme(axis.text.x = element_text(angle = 30,    #Roto 30° las etiquetas del eje x
                                   size = 6))
View(table(EME$sexo))

## Ganancias por Sector Industrial

### INTENTO 3
EME <-  EME %>% 
  arrange(sector, desc(Ganancia_2018))

EME %>%
  ggplot( aes(x=sector, y=Ganancia_2018)) +
  geom_col() +
  ggtitle("Ganancias 2018 por industria") +
  xlab("Ganancia Anual 2018") +
  ylab("Tipo de Industria") +
  coord_flip() +
  theme_base() 

### INTENTO 4
EME %>%
  filter(sector!="NA") %>% 
  mutate(sector =reorder(sector, desc(Ganancia_2018))) %>%
  ggplot(aes(x=sector, y=Ganancia_2018)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  ylab("Ganancias 2018") +
  xlab("Industria") +
  ggtitle("Ganancias por sector Industrial 2018")+
  theme_base() 

### INTENTO 5
EME %>%
  mutate(sector = fct_reorder(sector, desc(Ganancia_2018))) %>%
  ggplot(aes(x=sector, y=Ganancia_2018)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

### INTENTO 6
EME %>%
  mutate(sector = fct_reorder(sector, Ganancia_2018, .fun='median')) %>%
  ggplot(aes(x=reorder(sector, Ganancia_2018), y=Ganancia_2018, fill=sector)) + 
  geom_col() +
  coord_flip() +
  xlab("Ganancia 2018") +
  ylab("Sector") +
  theme(legend.position="none")

### INTENTO 7
EME %>%
  arrange(Ganancia_2018) %>%
  mutate(sector=factor(sector, levels=sector)) %>%
  ggplot(aes(x=sector, y=Ganancia_2018)) +
  geom_segment(aes(xend=sector, yend=0)) +
  geom_point( size=4, color="orange") +
  coord_flip() +
  theme_bw() +
  xlab("")


## Grupo de Edad por Sexo BARPLOT
g1 <- ggplot(EME, aes(grupo_edad)) +
  geom_bar() +
  facet_wrap(~sexo)
ggplotly(g1)
## Ganancias 2018
EME %>% 
  filter(sector!="NA" & Ganancia_2018<50000000 & Ganancia_2018 >= 0) %>% 
ggplot(aes(Ganancia_2018)) +
  geom_boxplot() +
  facet_wrap(~sector)


# Ganancias 2018 vs Gastos Anuales GEOMPOINT
EME %>% 
  filter(gasto_anual<1000000000 & Ganancia_2018<200000000 &
        gasto_anual > 0  & Ganancia_2018 > 0) %>% 
  ggplot(aes(gasto_anual, Ganancia_2018)) +
  geom_jitter(alpha=0.3) +
  geom_smooth(method = "lm", se=FALSE)

# Patente según sexo BARPLOT
ggplot(EME, aes(nacionalidad)) +
  geom_bar() +
  facet_wrap(~sexo)


gr1=ggplot(EME, aes(d7_2)) +
  geom_histogram() +
  facet_wrap(~ c1_caenes_red) +
  xlab("Ingresos Año 2018")

gr2=EME %>% 
  filter(d7_2<200000000 & d7_2 > 0 ) %>% 
  ggplot(aes(d7_2)) +
  geom_histogram() +
  facet_wrap(~ c1_caenes_red) +
  xlab("Ingresos Año 2018")

gr1+gr2

# Modelo Econométrico
library(moderndive)
library(ggplot2)
library(ISLR)
library(moderndive)
library(patchwork)

g1 <- ggplot(EME, aes(Ganancia_2018, gasto_anual,color=sexo)) +
  geom_point() +
  geom_smooth(method = "lm", se=F)


g2 <- ggplot(EME, aes(Ganancia_2018, gasto_anual,color=sexo)) +
  geom_point() +
geom_parallel_slopes(se = FALSE)

g1+g2

# INTENTO 1
m1 <- lm(Ganancia_2018 ~ sexo+nacionalidad+educ+region+grupo_edad, EME)
m1

get_regression_summaries(m1)

# INTENTO 2
m2 <- lm(Ganancia_2018 ~ sexo+nacionalidad+educ+region+grupo_edad+capacitacion, EME)
get_regression_summaries(m2)


# INTENTO 3
m3 <-  lm(Ganancia_2018 ~ sexo+nacionalidad+educ+region+grupo_edad+capacitacion+motivacion, EME)
get_regression_summaries(m3)


View(table(EME$nacionalidad))
