install.packages("pacman") # gerenciador de pacotes

library(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr,
               hrbrthemes, car, DescTools, factoextra, cluster) #pacotes basicos

dataset <- read.csv(file.choose())
dataset <- dataset %>% distinct() #remove linhas duplicadas

df <- dataset %>% select(popularity, danceability,instrumentalness, acousticness, ) #seleção das variáveis relevantes

# Normalização das variáveis


df_scaled <- scale(df, center = medias, scale = dps) # Variáveis normalizadas

# Clusterização K Means

set.seed(123)

clusters <- kmeans(df_scaled, 4, nstart = 25)
clusters

aggregate(df, by=list(cluster=clusters$cluster), mean) # Médias dos clusters

# Gráfico de clusters
fviz_cluster(clusters, df,
             repel = FALSE,
             geom = "point",
             show.clust.cent = TRUE,
             ellipse.type = "norm",
             ggtheme = theme_minimal(),
             main = "Factor map",
             alpha = 0.2,
             shape = 19)
