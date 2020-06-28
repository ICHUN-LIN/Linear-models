library(readxl)
library(ggplot2)
library(tidyverse)
library(magrittr)

spotify <- read_excel("spotify.xlsx") #read data from spotify.xlsx
spotify <- select(spotify,popularity, danceability, energy,key,loudness,mode,duration_ms,time_signature,decade)

convert_EX  <- function(x)
{ 
  x[x=="EX"]  <- NA
  x  <-as.numeric(x)
  return(x)
}

spotify <- spotify%>%mutate_at(c("danceability","energy","loudness","duration_ms"), convert_EX)

# danceability,
# energy,
# key,
# loudness,
# mode,
# duration_ms,
# time_signature, and
# decade.

summary(spotify$popularity) #check missing value
class(spotify$popularity) #check type

summary(spotify$danceability) #check missing value
class(spotify$danceability) #check type

summary(spotify$energy) #check missing value of coulm energy
class(spotify$energy) #check type

spotify$key<-factor(spotify$key,levels =c("A","A#","B","C","C#","D","D#","E","F","F#","G","G#"))
summary(spotify$key) #check missing value
class(spotify$key) #check type

summary(spotify$loudness) #check missing value
class(spotify$loudness) #check type

spotify$mode<-factor(spotify$mode,levels =c("major","minor"))
class(spotify$mode) #check type
summary(spotify$mode) #check missing value

summary(spotify$duration_ms) #check missing value
class(spotify$duration_ms) #check type

summary(spotify$time_signature) #check missing value
class(spotify$time_signature) #check type

spotify$decade<-factor(spotify$decade,levels =c("00s","50s","60s","70s","80s","90s"))
class(spotify$decade) #check type
summary(spotify$decade)

summary(spotify)

#bivarity anaylize

get_prop  <-function(x){return(x/ max(x, na.rm = TRUE))}
spotify <-spotify%>%mutate_if(is.numeric, get_prop)

# danceability, energy,loudness,duration_ms
# test  <- spotify%>%select(popularity, danceability, energy,loudness,duration_ms)
# cc <- test %>% gather(name, value = "data", 2:5)
# ggplot(cc,aes(data, popularity))+geom_point()+facet_wrap(~name)+geom_smooth(method="lm")

get_prop  <-function(x){return(x/ max(x, na.rm = TRUE))}
spotify <-spotify%>%mutate_if(is.numeric, get_prop)
spotify%>%select(popularity, danceability, energy,loudness,duration_ms) %>% gather(name, value = "data", 2:5)%>%ggplot(aes(data, popularity))+geom_point()+facet_wrap(~name)+geom_smooth(method="lm")

spotify%>%select(-popularity)%>%map(~lm(popularity~.x, data = spotify))%>%map_df(broom::glance, .id = "predictor")%>%select(predictor, p.value)


#t1  <- spotify%>%select(popularity, danceability)
#ggplot(t1,aes(danceability, popularity))+geom_point()+geom_smooth(method="lm")

#t2  <- spotify%>%select(popularity, energy)
#ggplot(t2,aes(energy, popularity))+geom_point()+geom_smooth(method="lm")

spotify%>%select(-popularity)%>%map(~lm(popularity~.x, data = spotify))%>%map_df(broom::glance, .id = "predictor")%>%select(predictor, p.value)
#m1 <- lm(popularity~key, data = spotify)
#broom::glance(m1)


# decade
# key
# mode
# time_signature

spotify$time_signature<-factor(spotify$time_signature,levels =c("0","1","2","3","4","5"))
spotify%>%ggplot(aes(time_signature, popularity ))+stat_boxplot(geom = "errorbar")+geom_boxplot()

spotify%>%ggplot(aes(decade, popularity ))+stat_boxplot(geom = "errorbar")+geom_boxplot()

spotify%>%ggplot(aes(key, popularity ))+stat_boxplot(geom = "errorbar")+geom_boxplot()

spotify%>%ggplot(aes(mode, popularity ))+stat_boxplot(geom = "errorbar")+geom_boxplot()



spotify%>%group_by(time_signature, mode)%>%summarise(mean =mean(popularity))%>%ggplot(aes(time_signature, mean, col = mode))+geom_point()+geom_line(aes(group = mode))

spotify%>%group_by(time_signature, key)%>%summarise(mean =mean(popularity))%>%ggplot(aes(key, mean, col = time_signature))+geom_point()+geom_line(aes(group = time_signature))

spotify%>%group_by(mode, key)%>%summarise(mean =mean(popularity))%>%ggplot(aes(key, mean, col = mode))+geom_point()+geom_line(aes(group = mode))

spotify%>%group_by(decade, key)%>%summarise(mean =mean(popularity))%>%ggplot(aes(decade, mean, col = key))+geom_point()+geom_line(aes(group = key))

spotify%>%group_by(decade, mode)%>%summarise(mean =mean(popularity))%>%ggplot(aes(decade, mean, col = mode))+geom_point()+geom_line(aes(group = mode))

spotify%>%group_by(decade, time_signature)%>%summarise(mean =mean(popularity))%>%ggplot(aes(decade, mean, col = time_signature))+geom_point()+geom_line(aes(group = time_signature))