#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
install.packages("httr")
#Instalar paquete XML
install.packages("XML")

library(XML)

#Pregunta 1.1 agregando
url <- "https://www.mediawiki.org/wiki/MediaWiki"
web_page <-  httr::GET(url)
xml_page <- htmlParse(web_page)
print(xml_page)


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
