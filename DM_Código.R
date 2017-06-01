##########################################################
############## CARGAR PAQUETES Y LIBRERÍAS ###############
##########################################################
install.packages("twitteR")
install.packages("tm")
install.packages("wordcloud")
install.packages("NLP")
install.packages("RColorBrewer")
install.packages(c("devtools", "rjson", "bit64", "httr"))

library(twitteR)
library(tm)
library(wordcloud)
library(NLP)
library(RColorBrewer)
library(devtools)
########################################################################################################################################



#################################################################
############## TWITTER API PROCESO DE AUTORIZACIÓN ##############
#################################################################
consumer_key <-"FM6pxe9ZWgp3HbMrNdFhqybWz"
consumer_secret <-"BsYDCXbGGMdsYj9ATviKxRTDIBW5yiB6sFbxVvl0Gq1HjNCiGm"
access_token <-"836873536874688514-Bk0nSryswLvnQTUHsyBCW55a67HBsFk"
access_secret <-"3FUFzEp2iRIhgSylljeGSwgOq4VVcc5Si0SePr5xNtyDs"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
########################################################################################################################################



#######################################################
############## RECOLECTAR TWEETS CUENTAS ##############
#######################################################
#Opción 1: QUE DICE LA COMPETENCIA. Recolectar tweets desde cuenta de usuario en Twitter (aún no se que voy a pedirle hasta que no tenga el objetivo)
#@VideojuegosGAME
rdmTweets1 <- userTimeline("VideojuegosGAME", n=3200)
(nDocs <- length(rdmTweets1))
#@MeriStation
rdmTweets2 <- userTimeline("MeriStation", n=3200)
(nDocs <- length(rdmTweets2))
#@Hobby_Consolas
rdmTweets3 <- userTimeline("Hobby_Consolas", n=3200)
(nDocs <- length(rdmTweets3))
#@PlayStationES
rdmTweets4 <- userTimeline("PlayStationES", n=3200)
(nDocs <- length(rdmTweets4))
#@Xbox_Spain
rdmTweets5 <- userTimeline("Xbox_Spain", n=3200)
(nDocs <- length(rdmTweets5))
#@Ubisoft_Spain
rdmTweets6 <- userTimeline("Ubisoft_Spain", n=3200)
(nDocs <- length(rdmTweets6))
#@VideoJuegos4
rdmTweets7 <- userTimeline("VideoJuegos4", n=3200)
(nDocs <- length(rdmTweets7))
#@NintendoES
rdmTweets8 <- userTimeline("NintendoES", n=3200)
(nDocs <- length(rdmTweets8))
#@VayaVideojuegos
rdmTweets9 <- userTimeline("VayaVideojuegos", n=3200)
(nDocs <- length(rdmTweets9))
#@Añadir más

rdmTweetsTotal <- c(rdmTweets1,
                    rdmTweets2,
                    rdmTweets3,
                    rdmTweets4,
                    rdmTweets5,
                    rdmTweets6,
                    rdmTweets7,
                    rdmTweets8,
                    rdmTweets9)
                  # Añadir más


#Opción 2:
#Opción 3:
#Opción X:
########################################################################################################################################



#############################################
############## CREAR DATAFRAME ##############
#############################################
df = twListToDF(rdmTweetsTotal)
df
dim(df) #16 variables
df$text
########################################################################################################################################



###############################################
############## LIMPIAR DATAFRAME ##############
###############################################
##ELIMINAR DUPLICADOS de un dataframe en R
anyDuplicated(df$text) #número de fila donde se encuentran los duplicados
unique(df$text)  #eliminamos los duplicados
########################################################################################################################################



##########################################
############## CREAR CORPUS ##############
##########################################
#Construir el corpus y especificar la fuente es un vector de caracteres (Desde dataframe cojo unicamente "text")
myCorpus <- Corpus(VectorSource(df$text))


#Modificar, no se aun para que sirve exactamente
# convert to lower case
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# tm v0.5-10
myCorpus <- tm_map(myCorpus, tolower)
########################################################################################################################################


#################################################
############## LIMPIEZA DEL CORPUS ##############
#################################################

#REMOVER URLs
removerURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removerURL))

#REMOVER cualquier otra cosa que tenga letras en ingles o espacios
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))

#PASAR A UTF-8 ASCII

#REMOVER signos de puntuacion #usar este codigo con todos los demas
removeSigPunt <- function(x) gsub("[[:punct:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeSigPunt)

#REMOVER PALABRAS QUE NO QUEREMOS QUE SALGAN
myStopwords <- c(stopwords(kind = "es"), "VideojuegosGAME",
                 "MeriStation",
                 "Hobby_Consolas",
                 "PlayStationES",
                 "Xbox_Spain",
                 "Ubisoft_Spain",
                 "VideoJuegos4",
                 "NintendoES",
                 "VayaVideojuegos")
                 #Añadir más"",
myStopwords #se nos añadirán al final


#REMOVER determinantes y preposiciones
myStopwords2 <- c("el","la","los","las","les","un","uno","una","unos","unas","al","del","todo","toda","todos","todas","mio","mios","mia","mias","tuyo","tuyos","tuya","tuyas","suyo","suyos","suya","suyas","nuestro",
"nuestros","nuestra","nuestras","vuestro","vuestros","vuestra","vuestras","suyo","suyos","suya","suyas","cuyo","cuya","cuyos","cuyas",
"este","ese","aquel","estos","esos","aquellos","esta","esa","aquella","estas","esas","aquellas","este","ese","aquel","estos","esos","aquellos","esta","esa","aquella","estas","esas","aquellas","esto","eso","aquello",
"dos","tres",
"the",
"a","ante","bajo","cabe","con","contra","de","desde","durante","en","entre","hacia","hasta","mediante","para","por","segun","sin","so","sobre","tras","versus","via")
#es igual a lo de antes
myStopwords2 <- read.table (file = "Determ_Prep_SW.txt", header = TRUE, row.names = 1, sep = "\t", dec = ".")


#REMOVER mis stopwords EN Corpus
myCorpus <- tm_map(myCorpus, removeWords, c(myStopwords,myStopwords2))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
myCorpus <- tm_map(myCorpus, removeWords, myStopwords2)
tm_map(myCorpus, removeWords(the))




        ################modificar                                         
#Stemming Words
# keep a copy of corpus to use later as a dictionary for stem completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument)
# inspect documents (tweets) numbered 11 to 15
# inspect(myCorpus[11:15])
# The code below is used for to make text fit for paper width
for (i in 11:15) {
    + cat(paste("[[", i, "]] ", sep=""))
    + writeLines(strwrap(myCorpus[[i]], width=73))
    }
# count frequency of "mining"
miningCases <- lapply(myCorpusCopy,
                          + function(x) { grep(as.character(x), pattern = "\\<mining")} )
sum(unlist(miningCases))

# count frequency of "miner"
minerCases <- lapply(myCorpusCopy,
                         + function(x) {grep(as.character(x), pattern = "\\<miner")} )
sum(unlist(minerCases))

# reemplazar "miner" con "mining"
myCorpus <- tm_map(myCorpus, content_transformer(gsub),
                       + pattern = "miners", replacement = "mining")

########################################################################################################################################



###########################################################################
############## CONSTRUCCIÓN MATRIZ DE DOCUMENTOS DE TERMINOS ##############
###########################################################################

#Construyendo Term-Document Matrix
tdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
tdm
idx <- which(dimnames(tdm)$Terms == "r") #no se
inspect(tdm[idx+(0:5),101:110]) #no se

tdm <- TermDocumentMatrix(myCorpus, control=list(minWordLength=1))

#Terminos frecuentes y asociaciones
# palabras frecuentes
findFreqTerms(tdm, lowfreq=100)

termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency>=100)
install.packages("ggplot2")
library(ggplot2)
df <- data.frame(term=names(termFrequency), freq=termFrequency)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") + xlab("Terms") + ylab("Count") + coord_flip()

#Encontrar asociaciones###################no funciona
library(tm)
#Con r
findAssocs(tdm, r, 0.25)
########################################################################################################################################



#######################################
############## WORDCLOUD ############## 
#######################################
library(wordcloud)
m <- as.matrix(tdm)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(rowSums(m), decreasing=TRUE)
# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]
# word cloud
set.seed(1000) 
# to make it reproducible
grayLevels <- gray( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=3, random.order=F, colors=pal)

#otro wordcloud
require(devtools)
install_github("lchiffon/wordcloud2")
library(wordcloud2)
wordcloud2(data = df, color = 'random-dark', shape ='pentagon')
wordcloud2
########################################################################################################################################



###########################################################
############## ANALISIS CLUSTER Y DENDOGRAMA ##############
###########################################################
# transpose the matrix to cluster documents (tweets)
m_cluster <- t(m)
# set a fixed random seed
set.seed(122)
# k-means clustering of tweets
k <- 8
kmeansResult <- kmeans(m_cluster, k)
# cluster centers
round(kmeansResult$centers, digits=3)

####MEJORAR

library(fpc)
# partitioning around medoids with estimation of number of clusters
pamResult <- pamk(m3, metric="manhattan")
# number of clusters identified
(k <- pamResult$nc)

pamResult <- pamResult$pamobject
# BUCLE   que muestra todos los metodos
for (i in 1:k) { 
  cat(paste("cluster", i, ": ")) cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)], "\n")
    }
########################################################################################################################################



##################################################
############## REGLAS DE ASOCIACIÓN ##############
##################################################

#FALTA

