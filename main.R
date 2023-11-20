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
xml_page


#Pregunta 1.3
# Buscar todos los enlaces en la página
enlaces <- xpathSApply(xml_page, "//a", xmlAttrs)

# Crear un data frame para almacenar los resultados
resultados <- data.frame(
  Texto_del_Enlace = sapply(enlaces, function(x) xmlValue(x[[1]])),
  URL_de_Destino = sapply(enlaces, function(x) x[['href']])
)

# Imprimir los resultados
print(resultados)