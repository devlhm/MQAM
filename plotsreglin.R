install.packages("pacman") # gerenciador de pacotes
library(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, 
               ggvis, httr, lubridate, plotly, rio, rmarkdown, shiny,                       
               stringr, tidyr, magrittr, hrbrthemes, car, plotly, plyr) #pacotes basicos


create_hist <- function(data, col, graph_title) {
  ggplot(data=data, aes(x=col)) +
    geom_histogram(fill="purple") +
    theme_ipsum() +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y")+
    ggtitle(graph_title)
}

create_boxplot <- function(data, col, graph_title) {
  ggplot(data, aes(y=col)) +
  geom_boxplot() +
  theme_ipsum() +
  ggtitle(graph_title)
}

create_dplot <- function(data, col, graph_title) {
  ggplot(data=data, aes(x=col)) +
    geom_density() +
    theme_ipsum() +
    stat_summary(aes(xintercept = after_stat(x), y = 0), fun = mean, geom = "vline", orientation = "y")+
    ggtitle(graph_title)
}

create_barplot <- function(data, col, graph_title) {
  ggplot(data, aes(x=col))+
    geom_bar(stat="count", width=0.7, fill="purple")+
    ggtitle(graph_title) +
    theme_ipsum()
}

remove_outliers <- function(data, col) {
  quartiles <- quantile(col, probs=c(.25, .75), na.rm = FALSE)
  IQR <- IQR(col)
  
  Lower <- quartiles[1] - 1.5*IQR
  Upper <- quartiles[2] + 1.5*IQR 
  
  return(subset(data, col > Lower & col < Upper))
}

df <- dataset
df$duration_min = df$duration_ms / 60000

df <- remove_outliers(df, df$duration_ms)
df <- remove_outliers(df, df$speechiness)
df <- remove_outliers(df, df$instrumentalness)
df <- remove_outliers(df, df$loudness)

#Retirados as entradas com valor 0 na popularidade
df <- df %>% filter(popularity > 0)

old_keys <- c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
keys <- c("Unknown", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B")

df$key <- mapvalues(df$key, old_keys, keys)
df$mode <- mapvalues(df$mode, c(0, 1), c("Minor", "Major"))
df$time_signature <- mapvalues(df$time_signature, c(4, 3, 1, 5), c("4/4", "3/4", "7/4", "5/4"))

create_boxplot(df, df$popularity, "Popularidade")
create_boxplot(df, df$duration_min, "Duração em minutos")
create_boxplot(df, df$danceability, "Dançabilidade")
create_boxplot(df, df$energy, "Energia")
create_boxplot(df, df$loudness, "Altura")
create_boxplot(df, df$speechiness, "Vocalidade")
create_boxplot(df, df$acousticness, "Acusticidade")
create_boxplot(df, df$instrumentalness, "Instrumentalidade")
create_boxplot(df, df$valence, "Positividade")
create_boxplot(df, df$tempo, "Tempo")

create_dplot(df, df$popularity, "Popularidade (sem entradas com valor 0)")
create_boxplot(df, df$popularity, "Popularidade (sem entradas com valor 0)")

create_barplot(df, df$explicit, "Explícita")

create_barplot(df, df$mode, "Modo")

create_barplot(df, df$key, "Tom")

create_barplot(df, df$time_signature, "Compasso")

create_dplot(df, df$duration_min, "Duração em minutos")
create_boxplot(df, df$duration_min, "Duração em minutos")

create_dplot(df, df$danceability, "Dançabilidade")
create_boxplot(df, df$danceability, "Dançabilidade")

create_dplot(df, df$energy, "Energia")
create_boxplot(df, df$energy, "Energia")

create_dplot(df, df$loudness, "Altura")
create_boxplot(df, df$loudness, "Altura")

create_dplot(df, df$speechiness, "Vocalidade")
create_boxplot(df, df$speechiness, "Vocalidade")

create_dplot(df, df$acousticness, "Acusticidade")
create_boxplot(df, df$acousticness, "Acusticidade")

create_dplot(df, df$instrumentalness, "Instrumentalidade")
create_boxplot(df, df$instrumentalness, "Instrumentalidade")

create_dplot(df, df$valence, "Positividade")
create_boxplot(df, df$valence, "Positividade")

create_dplot(df, df$tempo, "Tempo")
create_boxplot(df, df$tempo, "Tempo")
