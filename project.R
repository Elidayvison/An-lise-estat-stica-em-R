#bucar planilha

setwd('/Users/usuario/Documents/LC UFRPE/2022.3/Estatistica/Projeto')
library(janitor)
library(readxl)
library(dplyr)
library(tidyverse)
library(MASS)
library(ggplot2)
library(scales)


#guarda a tabela do excel no R
dados = read_excel("praia/sp_agua.XLSX", sheet = 1)

#REALIZA O FILTRO NA TABELA
df = dados %>%
  filter(City == "SÃO VICENTE")%>%
  mutate(Enterococcus = as.numeric(Enterococcus))%>%
  group_by(Beach)

#Questão 1:Encontre média, desvio-padrão, mediana, Q1, Q3, 
#mínimo e máximo dos enterococos de cada praia (summarise)

q1 = df %>%
  group_by(Beach) %>% 
  summarise(Media = mean(Enterococcus), Mediana = median(Enterococcus),Desvio_Padrao = sd(Enterococcus),
            Quartil_Q1 = quantile(Enterococcus,0.25), Quartil_Q3 = quantile(Enterococcus,0.75),
            Minimo = min(Enterococcus),Maximo = max(Enterococcus))
 
#Questão 2:grafico de barras

#ggplot(q1,aes(x = Beach,y = Maximo, fill = Beach, label = Maximo)) +
 # geom_bar(stat = "identity", position = "dodge") +
 # geom_label()

#ggplot(q1,aes(x = reorder(Beach,-Maximo),y = Maximo, fill = Beach, label = Maximo)) +
 # geom_bar(stat = "identity", position = "dodge") +
#  geom_label()
  
ggplot(q1,aes(x = reorder(Beach,-Maximo),y = Maximo, fill = Beach, label = scales::percent(Maximo/sum(Maximo)))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_label()

#Questão 3:grafico de pizza

ggplot(q1,aes(x = "",y = Maximo, fill = Beach, label = scales::percent(Maximo/sum(Maximo))))+
  geom_bar(stat = "identity")+
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  geom_label(position = position_stack(vjust = 0.5))

#Questão 4: hisstograma

ggplot(df) +
  geom_histogram(aes(x=Enterococcus),
  bins = 30,
  colour = "grey22", fill= "grey80") +
  labs(title = "Praias", x = "Enterococcus", y= "count", subtitle = "Histogram")

#ggplot(df,aes(x=Enterococcus)) +
 # geom_histogram(sepal.length, bins = 32, colours("grey22"),fill = "grey80")

#Questão 5: Box-plot


ggplot(df,aes(x = Beach)) +
  geom_boxplot(aes(color = factor(Beach)))

