library(wordcloud)
library(ggplot2)

datos<-ds_dt

plot(ds_dt$Followers, main = "Seguidores")


gg<-ds_dt %>%select(Screen.Name,Followers)%>% filter(Followers > 0)

View(gg)
g2<-occurences<-table(unlist(gg)) %>%  filter(rank(desc(profits))<=50)
View(g2)

hist(ds_dt$Favorites,xlim = c(1,500), ylim = c(0,10),main = "Frequencia vs Favoritos",xlab ="Favorites")

fr<-ds_dt %>% filter(Screen.Name == Full.Name)
View(fr)

##Graph
wordcloud(corpus, max.words = 20)
class(ds_dt$Date)
######### TOP 5 App usados
t_app <- datos %>% count(App) %>% arrange(desc(n)) %>% top_n(5)
ggplot(t_app, aes(x=App,y=n))+
 geom_histogram()

ggplot(t_app, aes(App, n))+
  geom_bar(stat = "identity")

######## TOP 5 Location
t_loc <- datos %>% count(Location) %>% arrange(desc(n)) %>% top_n(5)
View(t_loc)
ggplot(t_loc, aes(Location, n,fill=n))+
  geom_bar(stat = "identity")+
  coord_flip()
ggplot(t_loc, aes(x = Location, fill = n)) +
  geom_bar(width = 1)+
  coord_polar(theta = "x")

######## TOP 5 Dates
t_dat <- datos %>% count(Date) %>% arrange(desc(n)) %>% top_n(5)
###datos %>% count(day(Date) %>% arrange(desc(n)) %>% top_n(5)

theme_set(theme_classic())
ggplot(t_dat, aes(Date, n))+
  geom_bar(stat="identity", width = 0.5, fill="tomato2") + 
  labs(title="Top 5 Dates") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
  
######## AVG Followers
avg_foller <- datos %>% summarise(promedio=mean(Followers))
avg_follow <- datos %>% summarise(promedio=mean(Follows))
avg_retweet <- datos %>% summarise(promedio=mean(Retweets))
avg_fav <- datos %>% summarise(promedio=mean(Favorites))

View(datos)
####### Top 5 Users
t_users <- datos %>% count(Full.Name) %>% arrange(desc(n)) %>% top_n(5)
theme_set(theme_bw())

ggplot(t_users, aes(x=Full.Name, y=n)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=Full.Name, 
                   xend=Full.Name, 
                   y=0, 
                   yend=n)) + 
  labs(title="Top 5 Users",x="Users") + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6))



