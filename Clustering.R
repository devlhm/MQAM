install.packages("pacman") # gerenciador de pacotes

library(pacman)

pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr, magrittr,
               hrbrthemes, car, DescTools, factoextra, cluster) #pacotes basicos

dataset <- read.csv("dataset.csv")
dataset <- dataset %>% distinct() #remove linhas duplicadas

# Selecionando os gêneros musicais da análise
selected_genres <- c("classical", "metal", "hip-hop", "reggae")

df <- dataset %>% filter(track_genre %in% selected_genres)

#seleção das variáveis relevantes
df <- df %>% select(danceability, acousticness, energy, tempo, valence, track_genre, speechiness)

# ===================================================================

# Gráficos

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

create_boxplot(df, df$danceability, "Dançabilidade")
create_boxplot(df, df$instrumentalness, "Instrumentalidade")
create_boxplot(df, df$acousticness, "Acusticidade")
speech_bp <- create_boxplot(df, df$speechiness, "Fala")

create_boxplot(df, df$energy, "Energia")
create_boxplot(df, df$tempo, "Tempo")
create_boxplot(df, df$valence, "Valência")

# ===================================================================

# Normalização das variáveis

df_scaled <- df %>% select(-track_genre) %>% scale

# ===================================================================

# Clusterização K Means (não hierárquica)

set.seed(123)

kmeans_res <- kmeans(df_scaled, 4)

table(df$cluster) # Tamanho dos clusters
df$cluster = as.factor(kmeans_res$cluster) # Inserindo o número do cluster no dataset original

# Média das variáveis de cada cluster
aggregate(df %>% select(-track_genre, -cluster), by=list(cluster=kmeans_res$cluster), mean)

table(df$cluster, df$track_genre) # Frequência dos gêneros em cada cluster

ggplot(df, aes(group=cluster, fill=track_genre, x=cluster, y=after_stat(prop)))+
  geom_bar(position="fill", stat="prop", width=0.7) +
  ggtitle("Proporção dos gêneros em cada cluster") +
  theme_ipsum()

# Gráfico de clusters (gerado usando as duas variáveis que mais explicam a variabilidade)

fviz_cluster(kmeans_res, df %>% select(-track_genre, -cluster),
             repel = FALSE,
             geom = "point",
             show.clust.cent = TRUE,
             ellipse.type = "norm",
             ggtheme = theme_ipsum(),
             main = "Factor map",
             alpha = 0.2,
             shape = 19)

#ver se vale a pena colocar (e não sei o que são esses pontos pretos)
plot(df %>% select(-track_genre, -cluster), pch=20, col = df$cluster)

df <- df %>% select(-cluster) # Removendo a coluna cluster do dataframe original

# ===================================================================
# Clusterização hierárquica

# Utilizado distância manhattan e procedimento de agrupamento de Ward
h_clusters <- df_scaled %>% dist(method="manhattan") %>% hclust(method="ward.D2")

cluster_cut <- cutree(h_clusters, 4) # Selecionar recorte com 4 clusters

table(cluster_cut) # Tamanho de cada cluster
table(cluster_cut, df$track_genre) # Quantidade de gêneros em cada cluster

df$cluster <- cluster_cut # Inclusão do número do cluster no dataframe original

# Média das variáveis de cada cluster

aggregate(df %>% select(-track_genre), by=list(cluster=cluster_cut), mean)

ggplot(df, aes(group=cluster, fill=track_genre, x=cluster, y=after_stat(prop)))+
  geom_bar(position="fill", stat="prop", width=0.7)+
  ggtitle("Proporção dos gêneros em cada cluster") +
  theme_ipsum()

# Gráfico de clusters

#ver se vale a pena colocar (e não sei o que são esses pontos pretos)
plot(df %>% select(-track_genre, -cluster), pch=20, col = cluster_cut)
