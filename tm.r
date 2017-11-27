#text mining
dataset<-"D:/Descargas mozilla/TF_u201515749_u201510971 _u201518514/Proyecto/Datos"
Datos<-read.csv(paste(dataset,"datasetTwitters.csv",sep = "/"))

View(Datos)
ds_dt<-na.omit(Datos)

View(ds_dt)
#Limpieza
ds_dt$Tweet.Text<-iconv(x = ds_dt$Tweet.Text, from = "UTF-8",to = "UTF-8",sub = '')
ds_dt$Full.Name<-iconv(x = ds_dt$Full.Name, from = "UTF-8",to = "UTF-8",sub = '')
#ds_dt$Tweet.Text<-chartr("áéíóú","aeiou",ds_dt$Tweet.Text)
ds_dt$Tweet.Text<-chartr("#+:)@-","      ",ds_dt$Tweet.Text)
ds_dt$Screen.Name<-chartr("#+:)@","     ",ds_dt$Screen.Name)
ds_dt$Full.Name<-chartr("#+:)@?","      ",ds_dt$Full.Name)
ds_dt$Location<-chartr(".,-","   ",ds_dt$Location)

corpus<-Corpus(VectorSource(ds_dt$Tweet.Text))%>% 
  tm_map(PlainTextDocument)%>% 
  tm_map(removePunctuation)%>% 
  tm_map(removeWords,stopwords("Spanish"))%>%
  tm_map(removeWords,c("Hola","Adios", "cgt","https"))%>%
  tm_map(content_transformer(tolower))%>%
  tm_map(removeNumbers)
