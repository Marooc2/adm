library(dplyr)
library(lubridate)

#Dashboard Palabras Claves
serializado<-TermDocumentMatrix(x = corpus)
matriz<-as.matrix(serializado)

frec_palabra<-rowSums(matriz)
View(frec_palabra)
df<-data.frame(frec_palabra)
#Palabras Claves
View(df)

#//////////////////////
xx<-chartr("","",ds_dt$Full.Name)
xx<-iconv(x = xx, from = "UTF-8",to = "UTF-8",sub = '')
df2<-data.frame(xx)
df2 <-df2%>%distinct_()
#usuarios
View(df2)

#Cantidad de Tweets
C_Tweets = ds_dt %>% summarise(nro = n_distinct(Tweet.ID))
View(C_Tweets)

#Total de Retweets 
matrizRT<-sum(ds_dt$Retweets)
matrizRT
View(as.matrix(ds_dt$Retweets))
#Tweets por Usuario
View(ds_dt)
prueba<-count(ds_dt,Screen.Name)
View(prueba)
write.csv(x = prueba, file = "G:/TF_u201515749_u201510971 _u201518514/Proyecto/Reportes/TweetsporUsuario.csv")
# Top tweets
Top<-ds_dt %>% select(Full.Name,Screen.Name,Retweets,Tweet.Text,Favorites,Date) 


TopUsers<-ds_dt %>% select(Full.Name,Screen.Name,Bio,Followers,Follows,Favorites)
Rp <-head(TopUsers,n = 5)

write.csv(x = TopUsers, file = "G:/TF_u201515749_u201510971 _u201518514/Proyecto/Reportes/TopUsers.csv")

ds_dt %>% summarise(nro = n_distinct(Location))

View(Rp)

#Palabras que aparecen 100 veces en un tweet
findFreqTerms(serializado, lowfreq=100)
# Palabras escasas que se repiten 0.5%  o mas en un tweet

sparse = removeSparseTerms(serializado, 0.995)

tweetsSparse = as.matrix(sparse)
tweetsSparse<-rowSums(matriz)
Sp<-data.frame(tweetsSparse)
View(Sp)

TopTwitter<-ds_dt%>%select(Screen.Name,App) %>% filter(App == "Facebook")
na.omit(TopTwitter)
TT<-TopTwitter %>% group_by(Screen.Name)%>% summarise(count = c(Screen.Name))
View(TT)

#
View(ds_dt)
s<-ds_dt%>% select(Screen.Name,Full.Name,App,Location)%>% filter(Location == "Perú")
#usuarios
write.csv(x = s, file = "G:/TF_u201515749_u201510971 _u201518514/Proyecto/Reportes/UsuariosPeru.csv")
