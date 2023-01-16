# 156
Código da análise da plataforma 156
#pacotes utilizados

library(rvest)
library(stringr)
library (dplyr)
library (ggplot2)
library(maps)
library(mapdata)
library (tidyverse)
library (geojsonio)
library (openxlsx)
library (readxl)
library (rio)
library (dplyr)
library (viridis)
library (magrittr)
library(RColorBrewer)
library (rgdal)
library (sp)
library (rnaturalearth)
library (rnaturalearthhires)
library (devtools)
library (usethis)
library (RSQLite)
library (raster)
library(broom)
library(rgeos)
library(maptools)
library(ggsn)
library (sf)
library (leaflet)
library (GEOmap)
library (plyr)

# carrega os dados

dados2 <-read.csv("http://dados.prefeitura.sp.gov.br/dataset/0aecfa2b-aa3a-40d4-8183-0d4351b7fd0a/resource/a4f1a3ad-6cac-4283-869f-ee32e7ca1119/download/arquivofinal2tri2022.xlsx.csv",encoding="UTF-8",check.names=F)
dfv1a <- data.frame(dados2)
names (dfv1a)
dfv1a<-dfv1a[,-c(6,7,8,10,11,12,13,14,15,16)]
dfv1a[dfv1a==""]<-NA
dfv1b<-na.omit (dfv1a)
dfv1c <- as.data.frame (apply (dfv1b, 2, function (x) gsub ("\\ s +", "", x)))  
View (dfv1a)
dfv1c%>% group_by(dfv1c$Subprefeitura) %>%tally()

dadostotal=dfv1c

#visualização de dados

View (dadostotal)
count (dadostotal$Subprefeitura)
count (dadostotal$Tema)
View (dadostotal$Tema)
class (dadostotal$Tema)
hist (dadostotal$Tema)
count (dadostotal$Serviço)
count (dadostotal$Assunto)
countable <-table(dadostotal$Subprefeitura)
class (countable)
countable1=as.data.frame (countable)
class (countable1)
colnames(countable1)[1] <- "sp_nome"
View (countable1)

dadostotal%>% group_by(dadostotal$Subprefeitura) %>%tally()
dadostotal[dadostotal==""]<-NA
dadostotal<-na.omit (dadostotal)
summary (dadostotal)
names (dadostotal)
glimpse (dadostotal)

#inclusão de ID
dadostotal$ID=c("ID")
nrow(dadostotal)
dadostotal$ID<-1:103585

#gravar (alterar o diretório)
write_csv (dadostotal, "C:/Users/GUERREIRO/Desktop/FCD/BigDataRAzure/Cap05/dadostotal4.csv", fileEncoding-"UTF-8")

#gerar mapa
MAPARS <- st_read("C:/Users/GUERREIRO/Desktop/FCD/BigDataRAzure/Cap05/SIRGAS_SHP_subprefeitura/SIRGAS_SHP_subprefeitura_polygon.shp")
MAPARS
plot (MAPARS)
class (MAPARS)
ggplot (MAPARS)+
  geom_sf()
view (MAPARS)

acesso_san1<-merge(countable1, MAPARS)
View (acesso_san1)
colnames(acesso_san1) [2]<- "com_sol"
acesso_san1<-subset(acesso_san1, select = c(sp_codigo, com_sol))

MAPARS %>% 
  left_join(acesso_san1, by = "sp_codigo") %>% 
  ggplot(aes(fill = com_sol), color = "black") +
  geom_sf() +
  scale_fill_viridis(name = "Subprefeituras - Chamadas", direction = -1)+
  geom_sf_text(aes(label=sp_nome,geometry=geometry), colour="black", size=1.5)

plot (countable1)

#fim



