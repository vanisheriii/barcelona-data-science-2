#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
#Instalar paquete XML

install.packages("httr")
install.packages("XML")
install.packages("dplyr")
install.packages("stringr")
install.packages("writexl")


#Usando las librerias
library(XML)
library(dplyr)
library(writexl)


#Pregunta 1.1 agregando Descargar la pagina web y parsear a formato xml
url <- "https://www.mediawiki.org"
web_page <-  httr::GET(url)
xml_page <-  htmlParse(web_page, asText=TRUE)
print(web_page)

#Pregunta 1.2  buscar el tag title e imprimirlo
xml_title <- xpathApply(xml_page,"//title")
xml_title
print(xml_title[1])


#feature1.3
#print(xml_page)
#xml_page <- htmlParse(web_page, asText=TRUE)

#xml_a <- xpathApply(xml_page,"//a",xmlGetAttr, 'href')
#xml_a_href <- xpathApply(xml_page,"//a",xmlGetAttr, 'href')
#xml_a_desc <- xpathApply(xml_page,"//a//span")
#xml_tile <- xml_tile ifelse(is.na(xml_tile),"")
#xml_a_desc
##Usar el R vest
#busar el r markdown


#Pregunta 1.3 buscar los valores a y el contenido de este mismo
# Buscar todos los enlaces en la página
enlaces <- xpathSApply(xml_page, "//a", xmlAttrs)

# Obtener los valores de href y texto
df <- data.frame(
  HREF = xpathSApply(xml_page, "//a", xmlGetAttr, "href"),
  TEXTO = xpathSApply(xml_page, "//a", xmlValue)
)

df$TEXTO = df$TEXTO <- ifelse(is.na(df$TEXTO),na,df$TEXTO)


#FILTRO (NO ES PARTE DE LA PREGUNTA)
filter_duplicate  <- dplyr::filter(df,stringr::str_like(HREF,"/wiki/MediaWiki",ignore_case = TRUE))
 

#Pregunta 1.4 ver la cantidad de URLS que han sido llamado mas de una vez y agruparlo con su texto
 
 df2 <- df %>% group_by(HREF,TEXTO) %>% summarise(cantidad_peticiones  = n())
 
 
#Pregunta 1.5 de la lista anterior se tiene que iterar y obtener los valores del URL
 
 url_test <- httr::GET(url)
 url_test$status_code
 
 ## NORMALIZANDO la informaciòn de HREF, conviertinedo una URL Relativa a absoluta (tipo1)
 #df2 <- df2 %>%
 #   mutate(HREF = ifelse(stringr::str_starts(HREF, "^//", negate = FALSE),
 #                       paste0(url, gsub("//", "/", HREF)),
 #                        HREF),url_type = "relativo")

 ## NORMALIZANDO la informaciòn de HREF, convirtiendo una URL Relativa a absoluta (tipo2)
 df2 <- df2 %>%
   mutate(
     url_type = case_when(
       stringr::str_starts(HREF, "^//", negate = FALSE) ~ "relativo",
       stringr::str_starts(HREF, "^/\\w+", negate = FALSE) ~ "relativo",
       grepl("^#", HREF) ~ "relativo",
       TRUE ~ "absoluto"
     ),
     HREF = case_when(
       stringr::str_starts(HREF, "^//", negate = FALSE) ~ paste0(url, gsub("//", "/", HREF)),
       stringr::str_starts(HREF, "^/\\w+", negate = FALSE) ~ paste0(url, HREF),
       grepl("^#", HREF) ~ paste0(url, HREF),
       TRUE ~ HREF
     )
   )
 
 # ...
 
 # Agregar la columna responseCode a df2
 df2$responseCode <- NA
 
 # Iterar sobre las filas de df2
 for (i in 1:nrow(df2)) {
   row <- df2[i,]
   url_local <- row$HREF
   
   # Realizar la solicitud HTTP y almacenar el código de respuesta
   if (!is.na(url_local)) {
     if (startsWith(url_local, "https")) {
       response <- httr::GET(url_local)
       df2[i, "responseCode"] <- response$status_code
     } else {
       # Si la URL no comienza con "http", agregar el prefijo de la URL principal
       full_url <- paste0(url, url_local)
       response <- httr::GET(full_url)
       df2[i, "responseCode"] <- response$status_code
     }
   }
   
   # Pausa para evitar solicitudes demasiado frecuentes
   Sys.sleep(0.2)
 }
 
 # ...

#Pregunta 2.1
 install.packages("tidyverse")
 library(ggplot2)
 library(readr)
 library(dplyr)
 library(scales)
 
 
   df3 <- df2 %>%
     mutate(DOMINIO = gsub("^(https?://[^/]+).*", "\\1", HREF))
   
   df3 <- df3 %>% arrange(desc(cantidad_peticiones))
  
   ggplot(data = df3, aes(x = DOMINIO, fill = factor(url_type))) +
     geom_bar(color = "black", position = "stack", alpha = 0.7) +
     scale_fill_manual(values = c("yellow", "green")) +
     geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
     labs(title = "Frecuencia de Dominios", x = "Dominio", y = "Frecuencia") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotar etiquetas en el eje x
     facet_wrap(~url_type, scales = "free_y")  # Facetas por url_type con escalas libres en el eje y
   
   
   ggplot(data = df3, aes(x = reorder(DOMINIO, -..count..), fill = factor(url_type))) +
     geom_bar(color = "black", position = "stack", alpha = 0.7) +
     scale_fill_manual(values = c("yellow", "green")) +
     geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
     labs(title = "Frecuencia de Dominios", x = "Dominio", y = "Frecuencia") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     facet_wrap(~url_type, scales = "free_y")

#Pregunta 2.2   
   df3 <- df3 %>%
     mutate(enlace_interno = ifelse(grepl("^https://www.mediawiki.org", DOMINIO), "Interno", "Externo"))
  
   ggplot(data = df3, aes(x = enlace_interno, fill = factor(url_type))) +
     geom_bar(color = "black", position = "stack", alpha = 0.7) +
     scale_fill_manual(values = c("yellow", "green")) +
     geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, position = position_stack(vjust = 0.5)) +
     labs(title = "Suma de enlaces internos y externos", x = "Tipo de enlace", y = "Frecuencia") +
     theme_minimal() +
     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
     facet_wrap(~url_type, scales = "free_y")
   
   
   # Crear el gráfico de torta
   ggplot(df3, aes(x = factor(responseCode))) +
     geom_bar(stat = "count", fill = "blue", color = "green") +
     pie("y") +  # Convertir el gráfico en un gráfico de torta
     labs(title = "Distribución de responseCode", x = "Response Code") +
     theme_minimal()

#Pregunta 2.3
      
   install.packages("lessR")
   library(lessR)
   
   pie_chart <- PieChart(
     responseCode,
     hole = 0,
     values = "%",
     data = df3,
     fill = c("blue", "red"),
     main = "Porcentajes de Status Code",
     percent_label_color = "black", 
     show_legend = FALSE  
   )
   
   legend("topleft", legend = c("200", "404"),
          fill =  c("blue", "red"))
   
