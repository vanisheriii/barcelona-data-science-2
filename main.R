#Inicio ejercicio 2
#Instalar el paquete httr para descargar paginas web
install.packages("httr")
#Instalar paquete XML
install.packages("XML")

library(XML)


url <- "https://www.mediawiki.org/wiki/MediaWiki"
web_page <-  httr::GET(url)
xml_page <- htmlParse(web_page)
class(xml_page)
class(web_page)
