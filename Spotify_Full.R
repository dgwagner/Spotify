library(rjson)
library(tidyverse)
`%notin%` <- Negate(`%in%`)

data1 <- fromJSON(file = "/Users/DerekWagner/Documents/Spotify Data/MyData_Full_Feb2022/endsong_0.json")
data1.1 <- fromJSON(file = "/Users/DerekWagner/Documents/Spotify Data/MyData_Full_Feb2022/endsong_1.json")
data1.2 <- fromJSON(file = "/Users/DerekWagner/Documents/Spotify Data/MyData_Full_Feb2022/endsong_2.json")

data1 <- c(data1, data1.1, data1.2)

data2 <- data.frame(t(sapply(data1,c)))
data2$ts <- as.POSIXct(strptime(data2$ts, format="%Y-%m-%dT%H:%M:%SZ"))
data2 <- data2[order(data2$ts),]

data2 <- unique(data2)

data2$msPlayed <- as.numeric(data2$ms_played)
data2$shortDate <- substr(data2$ts, 1, 16)

#Remove skips
data3 <- data2%>%
  filter(msPlayed > 30000)%>%
  mutate(sPlayed = msPlayed/1000,
         mPlayed = sPlayed/60)%>%
  filter(ts >= "2018-06-01")

data3 <- data3%>%
  rename(endTime = ts)%>%
  rename(trackName = master_metadata_track_name)%>%
  rename(artistName = master_metadata_album_artist_name)%>%
  select(c(endTime, shortDate, trackName, artistName, msPlayed, sPlayed, mPlayed, episode_show_name, episode_name))

data_pods <- data3%>%
  filter(episode_name!="NULL")
data_music <- data3%>%
  filter(episode_name == "NULL")%>%
  select(-c(episode_show_name, episode_name))

saveRDS(data_music, file = "/Users/DerekWagner/Documents/Spotify Data/songs_data_asof_20220317.rds")
saveRDS(data_pods, file = "/Users/DerekWagner/Documents/Spotify Data/pods_data_asof_20220317.rds")

### Consolidate paused tracks
song_maxes <- data_songs%>%
  group_by(artistName,trackName)%>%
  summarize(max_length = max(mPlayed))%>%ungroup()
data_songs <- data_songs%>%merge(song_maxes, by=c('artistName','trackName'))
data_songs$endTime = as.POSIXct(as.character(data_songs$endTime), format="%Y-%m-%d %H:%M")
data_songs <- data_songs%>%arrange(endTime)
data_songs['dummy'] <- NA
data_songs['todelete'] <- NA
for (i in 1:nrow(data_songs)){
  row_1 = data_songs[i,]
  row_2 = data_songs[i+1,]
  if ((as.character(row_1$artistName)==as.character(row_2$artistName))
      &(as.character(row_1$trackName)==as.character(row_2$trackName))){
    data_songs$dummy[i] = 'SAME'
    data_songs$dummy[i+1] = 'SAME'
    total_time = as.numeric(row_1$mPlayed) + as.numeric(row_2$mPlayed)
    #data_songs$diff[i+1] = difftime(row_2$endTime, row_1$endTime, units='mins')
    if (total_time < row_1$max_length+.75){
      data_songs$mPlayed[i+1] = total_time
      data_songs$todelete[i] = "deleteme"
    }
  }
}
data_songs <- data_songs%>%filter(is.na(todelete)==TRUE)
data_songs <- data_songs%>%select(-c(dummy,todelete))

#Aggregation
data_songs <- unique(data_music%>%select(-c(endTime)))

song_plays <- data_songs%>%
  group_by(artistName,trackName)%>%
  summarize(n = n())%>%
  arrange(desc(n))
artist_plays <- data_songs%>%
  group_by(artistName)%>%
  summarize(n = n())%>%
  arrange(desc(n))
artist_time <- data_songs%>%
  group_by(artistName)%>%
  summarize(tot_min = sum(mPlayed),
            n = n(),
            unique_songs = n_distinct(trackName))%>%ungroup()%>%
  mutate(per_play = tot_min/n,
         pct = (tot_min/sum(tot_min))*100)%>%
  arrange(desc(tot_min))
song_time <- data_songs%>%
  group_by(artistName, trackName)%>%
  summarize(tot_min = sum(mPlayed),
            n = n())%>%ungroup()%>%
  mutate(per_play = tot_min/n,
         pct = (tot_min/sum(tot_min))*100)%>%
  arrange(desc(tot_min))

artist_time_2022 <- data_songs%>%
  filter(shortDate>="2022-01-01")%>%
  group_by(artistName)%>%
  summarize(tot_min = sum(mPlayed),
            n = n(),
            unique_songs = n_distinct(trackName))%>%ungroup()%>%
  mutate(per_play = tot_min/n,
         pct = (tot_min/sum(tot_min))*100)%>%
  arrange(desc(tot_min))
song_time_2022 <- data_songs%>%
  filter(shortDate>="2022-01-01")%>%
  group_by(artistName, trackName)%>%
  summarize(tot_min = sum(mPlayed),
            n = n())%>%ungroup()%>%
  mutate(per_play = tot_min/n,
         pct = (tot_min/sum(tot_min))*100)%>%
  arrange(desc(tot_min))
# Viz

data_time <- data_songs
data_time$endTime <- as.Date(data_time$shortDate)
library(lubridate)
data_time$my <- floor_date(data_time$endTime, "month")
data_time_2 <- data_time%>%
  group_by(my)%>%
  filter(artistName=="Post Malone")%>%
  filter(trackName=='Wow.')%>%
  summarize(tot_min=sum(mPlayed))

ggplot(data=data_time_2, aes(x=my, y=tot_min))+
  geom_bar(stat='identity')+
  xlim(as.Date("2018-11-01"), as.Date("2022-04-01"))
