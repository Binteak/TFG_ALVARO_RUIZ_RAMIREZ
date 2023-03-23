####################################################################
#                                                                  #
# Title: "Data Mining in Social Networks for SMEs Using Rstudio."  #
#                                                                  #
#   Author: Álvaro Ruiz Ramírez                                    #
#                                                                  #
#   Creation Date:      2017-10-17                                 #
#   Last Update:        2018-01-23                                 #
#                                                                  #
#   R version: 1.1.383                                             #
####################################################################
#PAQUETES Y LIBRERIAS--------------------------------------------------------------------------------------------------------------------
install.packages("ROAuth")
install.packages("RCurl")
install.packages("twitteR")
install.packages("sentiment")
install.packages("plyr")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("SnowballC")
install.packages("tm")
install.packages("httpuv")
install.packages("base64enc")
install.packages("qdap")

library(ROAuth)
library(RCurl)
library(twitteR)
library(sentiment)
library(plyr)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(SnowballC)
library(tm)
library(httpuv)
library(base64enc)
library(qdap)

#PROCESO DE AUTENTICACION Y CONEXION CON TWITTER API------------------------------------------------------------------------------------


#Certificaciones SSL
options(RCurlOptions = list(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl')))

#Urls
reqURL = 'https://api.twitter.com/oauth/request_token'
accessURL = 'https://api.twitter.com/oauth/access_token'
authURL = 'https://api.twitter.com/oauth/authorize'

#Credenciales
cred_twitter = OAuthFactory$new(consumerKey = api_key, consumerSecret = api_secret, requestURL = reqURL, accessURL = accessURL, authURL = authURL)
cred_twitter$handshake(cainfo = system.file('CurlSSL', 'cacert.pem', package = 'RCurl'))

#Tokens
api_key = "idOcb1eJaACX9fL175f0xQrsx"
api_secret = "Op2u8I5ODrrExzhbznaDE3ayrxTWQoMiIc0LDsZXz6dMSxJSfw"
access_token = "209639864-JiSUHNtyiPwmPstNbj3509ddIlN8nMUx7BOYzzGk"
access_token_secret = "mn1i4yZrU9EBPuuuLqqaCIg1YWDiWjHmD0kwUM8N4NVtK"

setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret) #Realizar autenticación

#DIRECTORIO DE TRABAJO-------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/Asus/Google Drive/TRABAJO FIN DE GRADO") #Todos los archivos estarán en esta dirección
getwd()

#RECOLECTAR TWEETS CUENTAS---------------------------------------------------------------------------------------------------------------
# Cuentas_Twitter <- read.csv("Cuentas_Twitter.csv",header=T)
# Cuentas_Twitter <- as.character(Cuentas_Twitter[,1])
# Problema al realizar el bucle "formato S4"para todas las cuentas en una sola linea

#VIÑAROCK
X1 <- userTimeline("VinaRockOficial", n=3200)
(nDocs <- length(X1))
#PRIMAVERA SOUND
X2 <- userTimeline("Primavera_Sound", n=3200)
(nDocs <- length(X2))
#SONAR
X3 <- userTimeline("SonarFestival", n=3200)
(nDocs <- length(X3))
#BILBAO BBK LIVE
X4 <- userTimeline("bilbaobbklive", n=3200)
(nDocs <- length(X4))
#CRUILLA FEST
X5 <- userTimeline("cruillabcn", n=3200)
(nDocs <- length(X5))
#AL RUMBO
X6 <- userTimeline("AlrumboFestival", n=3200)
(nDocs <- length(X6))
#FIB
X7 <- userTimeline("fiberfib", n=3200)
(nDocs <- length(X7))
#LOW FESTIVAL
X8 <- userTimeline("LowFestival", n=3200)
(nDocs <- length(X8))
#ARENAL SOUND
X9 <- userTimeline("ArenalSound", n=3200)
(nDocs <- length(X9))
#DREAM BEACH
X10 <- userTimeline("DreambeachFest", n=3200)
(nDocs <- length(X10))
#MADCOOL FESTIVAL
X11 <- userTimeline("madcoolfestival", n=3200)
(nDocs <- length(X11))
#AZKENA ROCK FESTIVAL
X12 <- userTimeline("AzkenaRockFest", n=3200)
(nDocs <- length(X12))
#CONTEMPOPRANEA
X13 <- userTimeline("contempopranea", n=3200)
(nDocs <- length(X13))
#SANTANDER MUSIC
X14 <- userTimeline("santandermusic", n=3200)
(nDocs <- length(X14))
#DCODE
X15 <- userTimeline("dcodefest", n=3200)
(nDocs <- length(X15))
#DOWNLOAD
X16 <- userTimeline("DownloadFestMad", n=3200)
(nDocs <- length(X16))
#TOMAVISTAS
X17 <- userTimeline("TomavistasFest", n=3200)
(nDocs <- length(X17))
#SAN SAN FESTIVAL
X18 <- userTimeline("SanSanFestival", n=3200)
(nDocs <- length(X18))
#FESTIVAL DE LES ARTS
X19 <- userTimeline("lesartsfest", n=3200)
(nDocs <- length(X19))
#VIDAFESTIVAL
X20 <- userTimeline("vidafestival", n=3200)
(nDocs <- length(X20))
#SONORAMA RIBERA
X21 <- userTimeline("sonoramaribera", n=3200)
(nDocs <- length(X21))

#ROCKSINSUBTITULOS
X22 <- userTimeline("RockSinUK", n=3200)
(nDocs <- length(X22))
#CULTURA ROCK MUSICA
X23 <- userTimeline("CulturaRock_M", n=3200)
(nDocs <- length(X23))
#DOD MAGAZINE
X24 <- userTimeline("dodmagazine", n=3200)
(nDocs <- length(X24))
#MONDOSONORO
X25 <- userTimeline("mondo_sonoro", n=3200)
(nDocs <- length(X25))
#ELQUINTOBEATLE
X26 <- userTimeline("quintoBeatle_es", n=3200)
(nDocs <- length(X26))
#ROCKING NEWS
X27 <- userTimeline("RockingNws", n=3200)
(nDocs <- length(X27))
#ELMUNDODETULSA
X28 <- userTimeline("elmundodetulsa", n=3200)
(nDocs <- length(X28))
#MUZIKALIA
X29 <- userTimeline("Muzikalia", n=3200)
(nDocs <- length(X29))
#INDIENAUTA
X30 <- userTimeline("Indienauta", n=3200)
(nDocs <- length(X30))
#31 cuentas

tweets = c(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,X22,X23,X24,X25,X26,X27,X28,X29,X30)
class(tweets)

#CREAR DATAFRAME INICIAL------------------------------------------------------------------------------------------

    #Longitud
    length_tweets <- length(tweets)
    length_tweets

    #Guardar tweets sin limpiar (iniciales)
    tweets_df <- ldply(tweets, function(t) t$toDataFrame())
    write.csv(tweets_df, "Tweets_Sin_Transformar.csv")  
                       
    #Obtener el texto
    tweets_txt = sapply(tweets, function(x) x$getText()) #obtenemos el texto
    tweets_txt

#---------------------------------------------------------TEXTO @'S---------------------------------------------------------   
#                                                                                                                          #
#----Guardar los perfiles más activos--------------------------------------------------------------------------------------#
#----Contar el número de veces que aparecen--------------------------------------------------------------------------------#
#----Crear tabla-----------------------------------------------------------------------------------------------------------#    
   
    arrobas = regmatches(tweets_txt,gregexpr("@(\\d|\\w)+",tweets_txt))
    arrobas = as.character(arrobas)
    df_arrobas = c(as.data.frame(arrobas)) 
    df_arrobas = ordered(table(df_arrobas))
    length(df_arrobas)
    
    require("qdap")
    arrobas_frec <- freq_terms(arrobas, 150)
    arrobas_frec = arrobas_frec[3:nrow(arrobas_frec),]
    arrobas_frec = arrobas_frec[which(nchar(arrobas_frec$WORD) > 3 & arrobas_frec$FREQ[] >= 10),]
                        
    for(i in 1:length(arrobas_frec$WORD)){
      arrobas_frec$WORD[i] = paste0("@",arrobas_frec$WORD[i])
    }
    
    Palabras_arr = arrobas_frec$WORD
    Frecuencia_arr = arrobas_frec$FREQ
    Frecuencia = Frecuencia_arr
    
    ggplot(arrobas_frec, aes(Frecuencia_arr, Palabras_arr)) + 
      geom_line(linetype = 4) +
      labs(y="Cuentas") + 
      labs(x="Frecuencia de Aparición") + 
      labs(title=paste0("@'s FRECUENTES: ",nrow(arrobas_frec)))+
      labs(caption="                     De: Álvaro Ruiz Ramírez")  +
      geom_point(aes(colour = Frecuencia)) 
 
    arrobas_imp= arrobas_frec[which(arrobas_frec$FREQ[] >= 10),] #aquellos que al menos aparezcan 10 veces
         
#---------------------------------------------------------TEXTO #'S---------------------------------------------------------   
#                                                                                                                          #
#----Guardar los hastags más usados----------------------------------------------------------------------------------------#
#----Contar el número de veces que aparecen--------------------------------------------------------------------------------#
#----Crear tabla-----------------------------------------------------------------------------------------------------------#  
    
    hastags = regmatches(tweets_txt,gregexpr("#(\\d|\\w)+",tweets_txt))
    hastags = as.character(hastags)
    
    library("qdap")
    hastags_frec = freq_terms(hastags, 150)
    hastags_frec = hastags_frec[2:nrow(hastags_frec),]
    hastags_frec = hastags_frec[which(nchar(hastags_frec$WORD) > 3 & hastags_frec$FREQ[] >= 10),]
    hastags_frec = hastags_frec[-grep("http", hastags_frec$WORD), ]
    
    for(i in 1:length(hastags_frec$WORD)){
      hastags_frec$WORD[i] = paste0("#",hastags_frec$WORD[i])
    }
    
   hastags_frec$FREQ = sort(hastags_frec$FREQ, decreasing=TRUE)
    
    Palabras_h = hastags_frec$WORD
    Frecuencia_h = hastags_frec$FREQ
    Frecuencia = Frecuencia_h
    
    ggplot(hastags_frec, aes(Frecuencia_h, Palabras_h)) + 
      geom_line(linetype = 4) +
      labs(y="Hastags") + 
      labs(x="Frecuencia de Aparición") + 
      labs(title=paste0("#HASTAGS FRECUENTES: ",nrow(hastags_frec)))+
      labs(caption="                     De: Álvaro Ruiz Ramírez")  +
      geom_point(aes(colour = Frecuencia)) 
      geom_smooth(method="lm", se=F)
    
      hastags_factor = factor(hastags_frec$WORD)
    
    hastags_imp = hastags_frec[which(hastags_frec$FREQ[] >= 10),] #aquellos que aparezcan al menos 10 veces
       
#---------------------------------------------------------TEXTO PALABRAS-----------------------------------------------------------------    
#LIMPIAR DATAFRAME PALABRAS--------------------------------------------------------------------------------------------------------------
require(tmap)
some_txt1 = tweets_txt
#remover RTs
some_txt1 = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
#remover dos puntos
some_txt2 = gsub(":", "", some_txt1)
#remover links html
some_txt3 = gsub("http[^[:blank:]]+", "", some_txt2)
#remover otros
some_txt4 = gsub("\n\n", "", some_txt3)
#remover menciones y hastags
some_txt5 =gsub("@(\\d|\\w)+", "", some_txt4)
some_txt5 = gsub("#(\\d|\\w)+", some_txt5)
#remover caracteres especiales
some_txt6 = gsub("[[:punct:]]", "", some_txt5)
#remover tildes
install.packages("stringi")
library(stringi)
some_txt7 <- stri_trans_general(some_txt6,"Latin-ASCII")
#remover digitos
some_txt8 =gsub("[[:digit:]]", "", some_txt7)
#eliminar filas duplicadas
duplicated(some_txt8) #vemos si hay duplicados
some_txt9 = unique(some_txt8) #eliminamos duplicados
duplicated(some_txt9) #vemos si se han quitado
#remover otros 2
some_txt10 =gsub("\n", "", some_txt9)
#remover tabs
some_txt11 =gsub("[ |\t]{2,}", "", some_txt10)
#quitar espacios en blanco al principio
some_txt12 =gsub("^ ", "", some_txt11)
#quitar espacios en blanco al final
some_txt13 =gsub(" $", "", some_txt12)
#cambiar de mayusculas a minúsculas
some_txt14 = gsub("/", "", some_txt13)
some_txt14 = as.list(tolower(some_txt14)) 
#remover letras chinas
some_txt15 = as.character(some_txt14)
some_txt15 = iconv(some_txt15, "latin1", "ASCII", sub="")
#volver a quitar espacios en blanco al principio
some_txt16 =gsub("^ ", "", some_txt15)
#quitar espacios en blanco al final
some_txt17 =gsub(" $", "", some_txt16)
#remover filas que estén vacías
l = length(some_txt17)
l
                        
write.csv(some_txt17, "Tweets_Transformados.csv") #guardamos tweets limpiados

#CREAR CORPUS---------------------------------------------------------------------------------------------------------------------------
require(tm)
                        
some_txt18 = Corpus(VectorSource(some_txt17))
head(some_txt18)

#LIMPIEZA DE STOPWORDS------------------------------------------------------------------------------------------------------------------
some_txt19 = tm_map(some_txt18, removeWords, (stopwords("english")))
swspa = (stopwords("spanish"))
swspa_sin = chartr('áéíóúñ','aeioun',swspa)
some_txt20 = tm_map(some_txt19, removeWords, swspa_sin)
some_txt20 = tm_map(some_txt20, removeWords, stopwords("catala"))
some_txt21 = tm_map(some_txt20, stripWhitespace)

lon = length(some_txt21)
for(i in 1:lon) { 
  print(strwrap(some_txt21[[i]]))
}
strwrap(some_txt21[[60]])

#crear y cargar el archivo desde csv
SW_Det_Prep <- read.csv("SW_Determinantes_Preposiciones.csv",header=T)
SW_Det_Prep <- as.character(SW_Det_Prep[,1])

SW_Otras <- read.csv("SW_Otras.csv",header=T)
SW_Otras <- as.character(SW_Otras[,1])

#Remover mis stopwords EN Corpus
some_txt22 <- tm_map(some_txt21, removeWords, SW_Det_Prep)
some_txt22 <- tm_map(some_txt22, removewords, SW_Otras)

#TERMINOS FRECUENTES PARA PALABRAS------------------------------------------------------------------------------------------------------
install.packages("qdap")
library("qdap")
term_count <- freq_terms(some_txt22, 20)

plot(term_count)
term_count = ordered(term_count)
Palabras_p = term_count$WORD
Frecuencia_p = term_count$FREQ
Frecuencia = Frecuencia_p

ggplot(term_count, aes(Frecuencia_p, Palabras_p)) + 
  geom_line(linetype = 4) +
  labs(y="Cuentas") + 
  labs(x="Frecuencia de Aparición") + 
  labs(title=paste0("Palabras FRECUENTES: ",length(Frecuencia_p)))+
  labs(caption="                     De: Álvaro Ruiz Ramírez")  +
  geom_point(aes(colour = Frecuencia)) 

term_count_imp= term_count[which(term_count$FREQ[] >= 10),] #aquellos que aparezcan al menos 10 veces

#TERMINOS FRECUENTES CON TM-------------------------------------------------------------------------------------------------------------
some_txt_tdm = TermDocumentMatrix(some_txt22)
some_txt_tdm
some_txt_m = as.matrix(some_txt_tdm)
dim(some_txt_m)
some_txt_m[100:140, 200]

#frecuencia de terminos por veces que aparecen fila a fila
term_frec = rowSums(some_txt_m)

#en orden descendente(de más a menos)
term_frec = sort(term_frec, decreasing = T)

#palabras más comunes
term_frec [1:200]

str(term_frec)
table(term_frec)
l = length(term_frec)
Cantidad = as.numeric(term_frec)
Palabras = names(term_frec)

#WORDCLOUD------------------------------------------------------------------------------------------------------------------------------
require(ggplot2)
wordcloud(some_txt22, min.freq = 5, max.words = 150, width = 1000, height = 1000, random.order = FALSE, rot.per=.15, colors = brewer.pal(4, "Dark2")) #color = pal
+ labs(caption="                     De: Álvaro Ruiz Ramírez")
