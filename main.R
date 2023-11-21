#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
#Instalar paquete XML
install.packages("httr")
install.packages("XML")
install.packages("dplyr")
install.packages("stringr")

library(XML)
library(dplyr)



#Pregunta 1.1 agregando
url <- "https://www.mediawiki.org/wiki/MediaWiki"
web_page <-  httr::GET(url)

#feature1.3
xml_page <- htmlParse(web_page)
print(xml_page)

xml_page <- htmlParse(web_page, asText=TRUE)


#Pregunta 1.2 
xml_title <- xpathApply(xml_page,"//title")
xml_title

#pregunta 1.3
xml_a <- xpathApply(xml_page,"//a",xmlGetAttr, 'href')
xml_a_href <- xpathApply(xml_page,"//a",xmlGetAttr, 'href')
xml_a_desc <- xpathApply(xml_page,"//a//span")
#xml_tile <- xml_tile ifelse(is.na(xml_tile),"")
xml_a_desc
##Usar el R vest
#busar el r markdown


#Pregunta 1.3
# Buscar todos los enlaces en la página
enlaces <- xpathSApply(xml_page, "//a", xmlAttrs)

# Obtener los valores de href y texto
df <- data.frame(
  HREF = xpathSApply(xml_page, "//a", xmlGetAttr, "href"),
  TEXTO = xpathSApply(xml_page, "//a", xmlValue)
)

df$TEXTO = df$TEXTO <- ifelse(is.na(df$TEXTO),na,df$TEXTO)


#FILTRO 
filter_duplicate  <- dplyr::filter(df,stringr::str_like(HREF,"/wiki/MediaWiki",ignore_case = TRUE))
 
 
 df2 <- df %>% group_by(HREF) %>% summarise(cantidad_peticiones  = n())
 
 inner_merged <- merge(df, df2, by = "HREF")
 inner_merged
 
 url_test <- httr::GET(url)
 url_test$status_code
 
 ##Para ver el header
 header <- httr::headers(url_test)
 header
 
 
 for(i in 1:nrow(df))
 {
   row <- df[i,]
   #print(row$HREF)
   url <- row$HREF
   
   ## aca tenemos que poner la condicional para saber comovamos tratar las url
   
   if(url == "https://www.mediawiki.org/wiki/Template:Main_page/bs")
     print("found")
   
   
 }
 
 
 

 header
 # Imprimir el dataframe
print(df)


install.packages("writexl")
library(writexl)
write_xlsx(df, "df_pregunta1_3..xlsx")
write_xlsx(inner_merged, "df_pregunta1_3_inner..xlsx")
