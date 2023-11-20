#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
install.packages("httr")
#Instalar paquete XML
install.packages("XML")

library(XML)

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

# Imprimir el dataframe
print(df)

