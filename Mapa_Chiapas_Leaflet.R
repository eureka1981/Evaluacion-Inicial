library( kableExtra )
library(tidyverse)
library(datos)
library(readxl)
library(knitr)
library(dplyr)
library(htmlwidgets)
library(readxl)
library(rgdal)
library(sp)
library(sf)
library(ggplot2)
library (tmap)
library(leaflet.extras)
library (mapview)
library (maptools)

##Leemos base de datos
chiapas <- read_excel("Downloads/chiapas.xlsx", sheet = "Base")
names (chiapas)[1]  = "CVEGEO"

#De la base de datos de incidencia delictiva, se filtran bases de 2 delitos de alto impacto
Narcomenudeo<-subset(chiapas, chiapas$Delito=="Narcomenudeo")
Homicidio_Doloso<-subset(chiapas, chiapas$Delito=="Homicidio doloso")

## Se lee el archivo con la cartografía, que contiene a todos los municipios del
# país y se filtra sólo a los de Chiapas
mapa_chiapas<- readOGR( 
  dsn="/Users/eunicesanchez/Downloads/muni_2018gw/", 
  layer="muni_2018gw",
  verbose=FALSE,
)


mapa_chiapas<-subset(mapa_chiapas, mapa_chiapas@data$CVE_ENT=="07")
mapa_chiapas@data$CVEGEO<-as.numeric(mapa_chiapas@data$CVEGEO)

## Se lee el archivo de pobreza
POBREZA <- read_excel("Eunice/POBREZA_.xlsx")
POBREZA<-subset(POBREZA, POBREZA$CVE_ENT=="07")
POBREZA$CVE_MUN<-as.numeric(POBREZA$CVE_MUN)

#Se crea la variable CVEGEO para que la base de datos pueda unirse a la cartografía

POBREZA$CVE_MUN<-(7000 + POBREZA$CVE_MUN)
POBREZA$CVEGEO<-POBREZA$CVE_MUN

### Se unen bases
POBREZA= merge(x=mapa_chiapas,y=POBREZA,by=  c("CVEGEO"), all.x=TRUE)
Homicidio_Doloso= merge(x=mapa_chiapas, y=Homicidio_Doloso,by=  c("CVEGEO"), all.x=TRUE)
Narcomenudeo= merge(x=mapa_chiapas,y=Narcomenudeo,by=  c("CVEGEO"), all.x=TRUE)

### Se cargan librerías
library(leaflet)
library (mapview)
library (maptools)
library(ggplot2)
library(RColorBrewer)

library(leaflet.extras)

library(tidyverse)

### Se describen las variables de interes para que puedan definirse los bins

summary(Narcomenudeo@data$Tasa)
summary(POBREZA@data$Porcentaje)
summary(Homicidio_Doloso@data$Tasa)

# Se definen bins

mybins_pobreza <- c(42,  60,  80, 100)
mybins_Narcomenudeo<- c(0, 1.2, 50,  100, 150, 185.2)
mybins_Homicidio<- c(0, 1.2,  5, 15 , 25, 34)

## Se crean las etiquetas
mytext_p <- paste(
  "Municipio: ", POBREZA@data$MUN,"<br/>", 
  "% Personas en pobreza: ", round(POBREZA@data$Porcentaje, 2), 
  sep="") %>%
  lapply(htmltools::HTML)

mytext_n <- paste(
  "Municipio: ", Narcomenudeo@data$Municipio,"<br/>", 
  "Incidencia 2021: ", round(Narcomenudeo@data$`2021`, 2), "<br/>", 
  "Incidencia ene - ago 2022: ", round(Narcomenudeo@data$`2022`, 2), "<br/>", 
  "Tasa x 100 mil habs 2022: ", round(Narcomenudeo@data$Tasa, 2), 
  sep="") %>%
  lapply(htmltools::HTML)


mytext_h <- paste(
  "Municipio: ", Homicidio_Doloso@data$Municipio,"<br/>", 
  "Incidencia 2021: ", round(Homicidio_Doloso@data$`2021`, 2), "<br/>", 
  "Incidencia ene - ago 2022: ", round(Homicidio_Doloso@data$`2022`, 2), "<br/>", 
  "Tasa x 100 mil habs 2022: ", round(Homicidio_Doloso@data$Tasa, 2), 
  sep="") %>%
  lapply(htmltools::HTML)


### Se definen las paletas de colores

mypalette_homicidio<-colorBin( palette="Reds", domain=Homicidio_Doloso@data$Tasa, na.color="transparent", bins=mybins_Homicidio)
mypalette_narco<-colorBin( palette="Oranges", domain=Narcomenudeo@data$Tasa, na.color="transparent", bins=mybins_Narcomenudeo)
mypalette_pobreza<- colorBin( palette="YlOrBr", domain=POBREZA@data$Porcentaje, na.color="transparent", bins=mybins_pobreza)

### Se crea el mapa

m=leaflet (Homicidio_Doloso) %>% 
  addTiles()  %>% 
  setView( lat=16.55, lng=-92.10 , zoom=5) %>% 
  fitBounds(-94.1392, 14.5311, -90.3715, 17.9853)%>%
  addPolygons( 
    fillColor = ~mypalette_homicidio(Homicidio_Doloso@data$Tasa), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    group = "Homicidio Doloso",
    label = mytext_h,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette_homicidio, values=~Homicidio_Doloso@data$Tasa, opacity=0.9, title = "Homicidio Doloso <br/> Tasa x c/100 mil habs", 
             position = "bottomright", group = "Homicidio Doloso" )

#Capa 2
m <- m %>% 
  addPolygons( 
    fillColor = ~mypalette_narco(Narcomenudeo@data$Tasa), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    group = "Narcomenudeo",
    label = mytext_n,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>%
  addLegend( pal=mypalette_narco, values=~Narcomenudeo@data$Tasa, opacity=0.9, title = "Narcomenudeo <br/> Tasa x c/100 mil habs", position = "bottomright", 
             group = "Narcomenudeo" )


#Capa 3

m <- m %>% 
  addPolygons( 
    fillColor = ~mypalette_pobreza(POBREZA@data$Porcentaje), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    group = "Pobreza",
    label = mytext_p,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>%
  addLegend( pal=mypalette_pobreza, values=~POBREZA@data$Porcentaje,  opacity=0.9, title = "% de personas en pobreza", position = "bottomright", group = "Pobreza" )




m <- m %>% addLayersControl(overlayGroups = c("Homicidio Doloso",  "Narcomenudeo",  "Pobreza"), 
                            options = layersControlOptions(collapsed = TRUE))
m

