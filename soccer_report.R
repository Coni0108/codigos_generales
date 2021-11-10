# LIBRERIA ----
library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(skimr)
library(tables)
library(kableExtra)
library(ggthemes)


# CARGAR DATAS ----
goals=read.csv("C:/Users/Connie/Desktop/CADEMI LABS/goals.csv") # bien
values= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/values.csv") # bien
roster= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/roster.csv") # aqui ta la caga
id= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/id.csv") # aquí tmb
matches= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/matches.csv")
lineups= read.csv("C:/Users/Connie/Desktop/CADEMI LABS/lineups.csv")


# Pregunta 1: ¿Cuántos goles anota cada jugador en cada mes? ----


# Nombre del jugador, si es gol o no y su valor

goals= goals %>% 
  select(-is_own_goal, -is_penalty_goal, -minute) %>% 
  merge(id) %>%
  merge(values) %>% 
  select(-id)
View(goals)

# Juntamos todo en una sola data

data_final= matches %>% 
  merge(goals) %>% 
  merge(roster) %>% 
  select(-team_name, -dob, -matchday)


#Manipular fechas
data_final$match_date= as.Date(data_final$match_date)

# Goles de los jugadores por mes y el total
goles_por_mes= select(data_final, playername,
                      is_goal, match_date) %>% 
  separate(match_date,c("año","mes"),sep="-") %>%
  select(mes, playername) %>%
  dcast(playername~mes) %>% rename(JAN=`01`,
                                   FEB=`02`,MAR=`03`,
                                   APR=`04`,
                                   MAY=`05`, AUG=`08`,
                                   SEP=`09`, OCT=`10`,
                                   NOV=`11`,DEC=`12`)
goles_por_mes$total = rowSums (goles_por_mes[ , 2:11])
# ALEXIS GOLEADOR (16 GOLES)

# Pregunta 2: jugador m?s joven ----
edades=data_final %>%
  select(playername,age)%>%
  group_by(playername) %>% 
  distinct() %>%
  arrange(age)
View(edades) #Calum Chambers 19 y H?ctor Beller?n 19
# Tomas Rosicky 33

# Pregunta 3: jugador m?s alto/m?s bajo ----
alturas= data_final %>% 
  select(playername, height) %>% 
  distinct() %>% 
  arrange(height)
View(alturas)

# El jugador más bajo es Aléxis Sánchez (1.68), el más alto es
# Olivier Giroud (1.68)

# Pregunta 4: ¿De qué países son los goleadores?
goleadores = goles_por_mes %>%
  select(playername,total) %>%
  merge(roster) %>%
  arrange(desc(total))

# TOP: CHILE, FRANCIA, ESPAÑA, REINO UNIDO.

head(goleadores,5)
table(goleadores$region)
# 1 país de AMÉRICA y 15 países de europa.
table(goleadores$country)
# 7 Reino Unido, 4 Francia, 2 España

table(goleadores$foot)
# Hay 8 diestros, 4 zurdos y 4 ambos.

# Los principales goleadores:Sánchez diestro, Giroud zurdo.
top11= head(goleadores,11)

table(top11$foot)

# WORDCLOUD para portada

# Modificamos la data_final eliminando match_date para q no se repitan
# los nombres
data_final <- data_final %>%
  select(-match_date,-team_field) %>% 
  distinct()

# GRAFICO NICO ----
ggplot(data = data_final, mapping = aes(x = playervalue, y = playername)) + 
  geom_col(aes(x= playervalue, y=reorder(playername,playervalue)),
           fill="red2") +
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
                         panel.border = element_blank(),
                         axis.ticks.y = element_blank()) +
  labs(title = "Valor de los goleadores", 
       caption = "FUENTE: Base de Datos Soccer 2021 (F.Morales)")+
  ylab("Jugador") +
  xlab("Valor en Dólares")
colors()

write.csv(alturas,
          file="alturas.csv",
          row.names = F)
write.csv(data_final,
          file = "data_final.csv",
          row.names = T)
data_paises_jugadores <- merge(roster, values)
data_paises_jugadores <- data_paises_jugadores %>% 
  merge(id)

View(data_paises_jugadores)

# Grafico de nacionalidad
ggplot(data = data_paises_jugadores, mapping = aes(x = playervalue,
                                                   y = playername,
                                                   fill = country)) + 
  geom_col(aes(x= playervalue, y=reorder(playername,playervalue))) +
  theme_light() +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks.y = element_blank()) +
  labs(title = "Valores de los Jugadores por nacionalidad", 
       caption = "FUENTE: Base de Datos Soccer 2021 (F.Morales)")+
  ylab("Jugador") +
  xlab("Valor en Dólares") +
  scale_fill_manual(values = c("#54CFDF","#1D9D34",
                               "#FF0000","#E8F70D",
                               "#0F00FF","#FF3636",
                               "#000000","#DB1135",
                               "#FFC300","#0814B1"))
# Grafico por posicion
ggplot(data = data_paises_jugadores, mapping = aes(x = playervalue, y = playername, fill = position)) + 
  geom_col(aes(x= playervalue, y=reorder(playername,playervalue)), color = "black") +
  labs(title = "Valores de los Jugadores por posición", 
       caption = "FUENTE: Base de Datos Soccer 2021 (F.Morales)")+
  ylab("Jugador") +
  xlab("Valor en Dólares") +
  scale_fill_manual(values = c("#FFFFFF","#9C824A",
                               "#063672","#EF0107"))
