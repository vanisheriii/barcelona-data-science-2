#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
#Instalar paquete XML

install.packages("httr")
install.packages("XML")
install.packages("dplyr")
install.packages("stringr")
install.packages("writexl")


library(XML)
library(dplyr)
library(writexl)


#Pregunta 1.1 agregando
url <- "https://www.mediawiki.org/wiki/MediaWiki"
web_page <-  httr::GET(url)
xml_page <-  htmlParse(web_page, asText=TRUE)
print(web_page)

#Pregunta 1.2 
xml_title <- xpathApply(xml_page,"//title")
xml_title
print(xml_title[1])


#feature1.3
print(xml_page)
xml_page <- htmlParse(web_page, asText=TRUE)

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


#FILTRO (NO ES PARTE DE LA PREGUNTA)
filter_duplicate  <- dplyr::filter(df,stringr::str_like(HREF,"/wiki/MediaWiki",ignore_case = TRUE))
 

#1.4
 
 df2 <- df %>% group_by(HREF) %>% summarise(cantidad_peticiones  = n())
 
 ##esto no va
 #inner_merged <- merge(df, df2, by = "HREF")
 #inner_merged
 print(df2)
 
 #1.5
 
 url_test <- httr::GET(url)
 url_test$status_code
 
 ##Para ver el header
 header <- httr::headers(url_test)
 header
 
 
 for(i in 1:nrow(df2))
 {
   row <- df2[i,]
   #print(row$HREF)
   url <- row$HREF
   
   ## aca tenemos que poner la condicional para saber comovamos tratar las url
   
  # if(url == "https://www.mediawiki.org/wiki/Template:Main_page/bs")
   #  print("found")
   #if(stringr::str_starts(url,"^//",negate = FALSE))
    # print(url)
   
   
    #if(stringr::str_starts(url,"^/\\w+",negate = FALSE))
   #  print(url)
    

   #if(stringr::str_starts(url,"https",negate = FALSE))
   # print(url)
   
   if(stringr::str_starts(url,"http:",negate = FALSE))
    print(url)
   
   #if(stringr::str_starts(url,"#",negate = FALSE))
   #  print(url)

 }

 var1 <- df2$HREF
 
#Para exportar 
 
#write_xlsx(df, "df_pregunta1_3..xlsx")
#write_xlsx(inner_merged, "df_pregunta1_3_inner..xlsx")
