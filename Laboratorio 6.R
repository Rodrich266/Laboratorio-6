#------------------------------------------------------------#
# Laboratorio 6
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("tm","twitteR","ROAuth","ggplot2","wordcloud","RCurl")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#------------------------ Autenticación en Twitter -----------------------------------#
consumerKey<-"consumerKey"
consumerSecret<-"consumerSecret"

accessToken <-	"accessToken"
accessTokenSecret <-	"accessTokenSecret"

setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#---------------------------- Lectura de Datos ---------------------------------------#
#Se buscan los tweets del usuario designado
Tweets <- searchTwitteR("traficogt",n=150,lang = "es")

#Se pasan a un dataframe
Datos<-twListToDF(Tweets)

#Se proporciona una dimensión para los datos
dif_UTC_gt <- 6*60*60
Datos$created2 <- Datos$created-dif_UTC_gt

#Creación del csv para guardar los datos
write.csv(Datos,"Tweets.csv")

#-------------------------- Limpieza y preprocesamiento ------------------------------#

#Se realiza un vector de los datos y se convierten en volátiles para cambiar su contenido
VectorDatos <- VectorSource(Datos)
DatosLimpios <- VCorpus(VectorDatos)

#Preparar transformación
DatosLimpios <- tm_map(DatosLimpios, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

#Se transforman los caracteres a minúsculas
DatosLimpios<-tm_map(DatosLimpios, content_transformer(tolower))

#Se eliminan los espacios en blanco adicionales
DatosLimpios<-tm_map(DatosLimpios, content_transformer(stripWhitespace))

#Se eliminan los URLs, se detiene al encontrar un espacio
RemoverURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
DatosLimpios <- tm_map(DatosLimpios, RemoverURL)
Espacio <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
DatosLimpios <- tm_map(DatosLimpios, Espacio, "/")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "@")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "\\|")

#Se eliminan las puntuaciones y símbolos
DatosLimpios<-tm_map(DatosLimpios, content_transformer(removePunctuation))

#Se eliminan los números para que no interfieran con la predicción
#DatosLimpios<-tm_map(DatosLimpios, content_transformer(removeNumbers)) #Depende del tema

#Se eliminan artículos, preposiciones y conjunciones (stopwords)
DatosLimpios<-tm_map(DatosLimpios, removeWords, stopwords('spanish'))

#------------------------ Análisis Exploratorio -----------------------------------#

#Tokenización de Unigramas
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))

#Función para obtener las frecuencias de cada palabra
Frecuencia <- function(tdm){
  Frec <- sort(rowSums(as.matrix(tdm)), decreasing=TRUE)
  Frecuencia <- data.frame(word=names(Frec), Frec=Frec)
  return(Frecuencia)
}

#Remoción de unigramas que reaparecen menos de una vez
Unigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = UnigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los unigramas
FrecUnigrama <- Frecuencia(Unigrama)

#Función para graficación de los datos
PlotFrec <- function(data, title) {
  ggplot(data[1:25,], aes(reorder(word, -Frec), Frec)) +
    labs(x = "Palabras/Frases", y = "Frecuencia") +
    ggtitle(title) +
    theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) +
    geom_bar(stat = "identity")
}

#--------------------------- Gráficas de frecuencia --------------------------------------#

#Unigramas
PlotFrec(FrecUnigrama, "Unigramas más comúnes (Top 25)")

#--------------------------------- Wordclouds ---------------------------------------------#

#Unigrama
#Primero se coloca el unigrama en un data frame y se visualiza una tabla de frecuencias
MatU <- as.matrix(Unigrama)
OrdenU <- sort(rowSums(MatU),decreasing=TRUE)
DFU <- data.frame(word = names(OrdenU),freq=OrdenU)
head(DFU, 10)

#Luego, se realiza la nube de palabras
wordcloud(words = DFU$word, freq = DFU$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))