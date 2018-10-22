#------------------------------------------------------------#
# Laboratorio 6
# Grupo 1
#------------------------------------------------------------#

#----- Instalación e importación de librerias necesarias para correr el programa ----#
for (libreria in c("tm","twitteR","ROAuth","ggplot2","wordcloud","RCurl","RWeka","topicmodels","data.table")) {
  if (!require(libreria, character.only=T)) {
    install.packages(libreria)
    library(libreria, character.only=T)
  }
}

#Por reproducibilidad, se recomienda correr el código desde la sección de limpieza y preprocesamiento
#Sin embargo, se muestra el código utilizado para obtener los tweets a continuación:

#------------------------ Autenticación en Twitter -----------------------------------#

#Se colocan las claves, tokens y llaves proporcionadas por Twitter en variables
consumerKey <- "ck"
consumerSecret <- "cs"

accessToken <- "at"
accessTokenSecret <-	"ats"

#Se inicializa el proceso de autenticación
setup_twitter_oauth(consumerKey,consumerSecret,accessToken,accessTokenSecret)

#---------------------------- Lectura de Datos ---------------------------------------#

#Se buscan los tweets del usuario designado
Tweets <- searchTwitteR("traficogt",n=150,lang = "es")

#Se pasan a un dataframe
Datos <- twListToDF(Tweets)

#Se agrega la fecha y la hora de cada tweet, en la zona horaria de Guatemala
dif_UTC_gt <- 6*60*60
Datos$created2 <- Datos$created-dif_UTC_gt

#Creación del csv para guardar los datos
write.csv(Datos,"Tweets.csv")

#-------------------------- Limpieza y preprocesamiento ------------------------------#

#Por reproducibilidad, se coloca el csv de tweets originales en una variable para no sobreescribir los tweets con el código
Datos <- read.csv("Tweets.csv")

#Se eliminan las columnas de datos no relevantes
Datos <- subset(Datos, select=-c(X,id,replyToUID,replyToSID,statusSource))

#Se realiza un vector de los datos y se convierten en volátiles para cambiar su contenido
VectorDatos <- VectorSource(Datos$text)
DatosLimpios <- VCorpus(VectorDatos)

#Preparar transformación
DatosLimpios <- tm_map(DatosLimpios, content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')))

#Se transforman los caracteres a minúsculas
DatosLimpios <- tm_map(DatosLimpios, content_transformer(tolower))

#Se eliminan los espacios en blanco adicionales
DatosLimpios <- tm_map(DatosLimpios, content_transformer(stripWhitespace))

#Se eliminan los URLs, se detiene al encontrar un espacio
RemoverURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
DatosLimpios <- tm_map(DatosLimpios, RemoverURL)
Espacio <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
DatosLimpios <- tm_map(DatosLimpios, Espacio, "/")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "@")
DatosLimpios <- tm_map(DatosLimpios, Espacio, "\\|")

#Se eliminan las puntuaciones y símbolos
DatosLimpios <- tm_map(DatosLimpios, content_transformer(removePunctuation))

#Se eliminan los números para que no interfieran con la predicción
#DatosLimpios<-tm_map(DatosLimpios, content_transformer(removeNumbers)) #Depende del tema

#Se eliminan artículos, preposiciones y conjunciones (stopwords)
DatosLimpios <- tm_map(DatosLimpios, removeWords, stopwords('spanish'))

#------------------------ Análisis Exploratorio -----------------------------------#

#Tokenización de Unigramas
UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))

#Tokenización de Bigramas
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))

#Tokenización de Trigramas
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))


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

#Remoción de bigramas que reaparecen menos de una vez
Bigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = BigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los bigramas
FrecBigrama <- Frecuencia(Bigrama)

#Remoción de trigramas que reaparecen menos de una vez
Trigrama <- removeSparseTerms(TermDocumentMatrix(DatosLimpios, control = list(tokenize = TrigramTokenizer)), 0.9999)

#Se obtienen las frecuencias de los trigramas
FrecTrigrama <- Frecuencia(Trigrama)

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

#Bigramas
PlotFrec(FrecBigrama, "Bigramas más comúnes (Top 25)")

#Trigramas
PlotFrec(FrecTrigrama, "Trigramas más comúnes (Top 25)")

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


#Encontrar palabras asociadas a trafico, pmt, accidentes y carretera.
findAssocs(Unigrama,"trafico",0.2)
findAssocs(Unigrama,"pmt",0.2)
findAssocs(Unigrama,"accidente",0.2)
findAssocs(Unigrama,"carretera",0.2)


#topic modelling, se crean 8 topicos de acuerdo a datos en comun.
dtm <- as.DocumentTermMatrix(Unigrama)
lda <- LDA(dtm, k=8)
term <- terms(lda,7)
(term <- apply(term, MARGIN = 2, paste, collapse = ","))

#se visualizan estos topicos, como todos los tweets fueron tomados el mismo dia, sale solo una columna en la grafica, pero si 
#se hubiesen tomado en un periodo de tiempo si se podria visualizar. Esto se conoce como stream graph.
topics <- topics(lda)
topics <- data.frame(date = as.IDate(Datos$created)  ,topic=topics)
ggplot(topics,aes(date,fill=term[topic]))+geom_density(position = "stack")


#graficar los tweets mas retweeteados, eje y es para la cantidad de veces retweeteados y el eje x es para la hora en la que fueron tweeteados.
table(Datos$retweetCount)
selected<-which(Datos$retweetCount >= 10)

dates<-strptime(Datos$created, format = "%Y - %m - %d")
plot(x = dates, y=Datos$retweetCount, type="l",col="grey",
     xlab="Fecha",ylab = "Veces Retweeteados")
colors <- rainbow(10)[1:length(selected)]
points(dates[selected],Datos$retweetCount[selected],pch=19,col=colors)
text(dates[selected],Datos$retweetCount[selected],Datos$text[selected],col=colors,
     cex=.9)

