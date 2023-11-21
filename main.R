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
url <- "https://www.mediawiki.org/wiki/MediaWiki"
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
 

#1.4 ver la cantidad de URLS que han sido llamado mas de una vez y agruparlo con su texto
 
 df2 <- df %>% group_by(HREF) %>% summarise(cantidad_peticiones  = n())
 
 inner_merged <- merge(df, df2, by = "HREF")
 inner_merged
 print(df2)
 
#1.5 de la lista anterior se tiene que iterar y obtener los valores del URL
 
 url_test <- httr::GET(url)
 url_test$status_code
 
 ##Para ver el header
 header <- httr::headers(url_test)
 header
 
 
 inner_merged <- cbind(inner_merged, responseCode=NA)
 
 for(i in 1:nrow(inner_merged))
 {
   row <- inner_merged[i,]
   url_local <- row$HREF
   
   ## aca tenemos que poner la condicional para saber comovamos tratar las url
   
   #if(stringr::str_starts(url,"^//",negate = FALSE))
    # print(url)
   
   if(stringr::str_starts(url_local,"^//",negate = FALSE)){
     # Reemplazar las dobles barras por una sola
     url_local <- gsub("//", "/", url_local)
     full_url = paste0(url,url_local)
     print(full_url)
   }
   
   
    #if(stringr::str_starts(url,"^/\\w+",negate = FALSE))
   #  print(url)
    

   if(stringr::str_starts(url_local,"https",negate = FALSE)){
     url_test <- httr::GET(url_local)
     print(url_local)
     print(url_test$status_code)
     inner_merged[i,4] <- url_test$status_code
     Sys.sleep(0.2)
   }
   
   #if(stringr::str_starts(url,"#",negate = FALSE))
   #  print(url)

 }


#Para exportar 
#write_xlsx(df, "df_pregunta1_3..xlsx")
#write_xlsx(inner_merged, "df_pregunta1_3_inner..xlsx")
