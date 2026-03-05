#------------------------------------------------------------------
# Proyecto: Análisis de emociones de la discografía de Taylor Swift
# Script: Emotions.R
# Propósito: Crear gráficos y extraer insights con los resultados del análisis 
# de emociones de las letras de las canciones de Taylor Swift realizado en 
# Python con un modelo preentrenado
# Autor: Alicia esperesate
# Fecha: 2022-02-04
#------------------------------------------------------------------

# =========================================
# 1. CONFIGURACIÓN
# =========================================

# Cargamos las librerías
library(tidyverse)
library(stringr)
library(forcats)
library(scales)
library(ggradar)

# Orden de los álbumes
album_levels <- c(
  "taylor_swift","fearless","speak_now","red","1989",
  "reputation","lover","folklore","evermore",
  "midnights","the_tortured_poets_department",
  "the_life_of_a_showgirl"
)

# Orden semántico de emociones (de positivas a negativas)
emotion_levels <- c(
  "joy","surprise","neutral",
  "sadness","anger","fear","disgust"
  )

# Color de cada álbum
album_colors <- c(
  "taylor_swift" = "#1B9E77",
  "fearless" = "#F1C40F",
  "speak_now" = "#7570B3",
  "red" = "#E74C3C",
  "1989" = "#5DADE2",
  "reputation" = "#000000",
  "lover" = "#E84393",
  "folklore" = "#7F8C8D",
  "evermore" = "#8E5A2B",
  "midnights" = "#0B3C5D",
  "the_tortured_poets_department" = "#FFFFFF",
  "the_life_of_a_showgirl" = "#E67E22"
)

# Colores de las emociones
emotion_colors <- c(
  "joy"="#F7DC6F",
  "sadness"="#5DADE2",
  "anger"="#E74C3C",
  "fear"="#8E44AD",
  "disgust"="#58D68D",
  "surprise"="#F39C12",
  "neutral"="#95A5A6"
)


# =========================================
# 2. CARGA DE DATOS
# =========================================

# Resultados del análisis de emociones y el csv. con los metadatos extraídos de Wikipedia
base_path <- "C:/Users/Alicia/OneDrive/Escritorio/Proyecto TS"

emotions_album <- read.csv(file.path(base_path, "data/processed/results/emotions_by_album.csv"))
emotions_song  <- read.csv(file.path(base_path, "data/processed/results/emotions_by_song.csv"))
emotions_song_metadata <- read.csv(file.path(base_path, "data/csv/songs_data.csv"), sep = ";")


# ========================================= 
# 3. LIMPIEZA 
# ========================================= 

# Reordenamos las columnas según el orden semántico definido
emotions_song <- emotions_song %>%
  select(song_id, song, album, all_of(emotion_levels))

emotions_album <- emotions_album %>%
  select(album, all_of(emotion_levels))

# Factores ordenados 
emotions_album <- emotions_album %>% 
  mutate(album = factor(album, levels = album_levels)) 

emotions_song <- emotions_song %>%
  mutate( album = factor(album, levels = album_levels), 
          song = factor(song) ) 

# Join metadatos con el dataset sin pivotar

songs_full_no_pivot <- emotions_song_metadata %>% 
  left_join(emotions_song, by = "song_id") %>%
  rename(song = song.x, album = album.x)

# Pivotado 
emotions_album_pivot <- emotions_album %>%
  pivot_longer( 
    cols = -album, 
    names_to = "emotion", 
    values_to = "value" 
    ) %>%
  mutate(emotion = factor(emotion, levels = emotion_levels))

emotions_song_pivot <- emotions_song %>%
  pivot_longer(
    cols = all_of(emotion_levels),
    names_to = "emotion",
    values_to = "value"
  ) %>%
  mutate(emotion = factor(emotion, levels = emotion_levels))

# Extraemos nombre de las canciones y número de pista 
emotions_song_pivot <- emotions_song_pivot %>%
  mutate( 
    track = as.integer(str_extract(song, "^\\d+")), 
    song_clean = str_remove(song, "^\\d+_") 
    ) 

# Dataset con metadatos + pivotado 
songs_full <- songs_full_no_pivot %>%
  pivot_longer(
    cols = all_of(emotion_levels),
    names_to = "emotion",
    values_to = "value"
  ) %>%
  mutate(emotion = factor(emotion, levels = emotion_levels),
         track = as.integer(str_extract(song, "^\\d+")),
         song_clean = str_remove(song, "^\\d+_"))


# ========================================= 
# 4. VARIABLES DERIVADAS 
# ========================================= 

# Media por disco (media de canciones) 
emotions_song_mean <- emotions_song %>% 
  group_by(album) %>% 
  summarise(across(joy:disgust, \(x) mean(x, na.rm = TRUE))) 
head(emotions_song)
emotions_song_mean <- emotions_song_mean %>% 
  mutate(album = factor(album,levels = album_levels)) 

emotions_song_mean_pivot <- emotions_song_mean %>% 
  pivot_longer( 
    cols = -album, 
    names_to = "emotion", 
    values_to = "value"
  ) %>% 
  mutate(emotion = factor(emotion, levels = emotion_levels)) 

# Canciones con el nivel más alto de una de las emociones 
top_songs <- songs_full_no_pivot %>% 
  select(song, album, joy, surprise, neutral, sadness, anger, fear, disgust) %>%
  pivot_longer( 
    cols = joy:disgust, 
    names_to = "emotion", 
    values_to = "value" 
  ) %>% 
  group_by(emotion) %>% 
  slice_max(value, n = 1, with_ties = FALSE) %>% 
  ungroup() %>% 
  mutate(emotion = factor(emotion, levels = emotion_levels)) 

#Canciones con el nivel más bajo de una de las emociones 
low_songs <- songs_full_no_pivot %>% 
  select(song, album, joy, surprise, neutral, sadness, anger, fear, disgust) %>%
  pivot_longer( 
    cols = joy:disgust, 
    names_to = "emotion", 
    values_to = "value" 
  ) %>% 
  group_by(emotion) %>% 
  slice_min(value, n = 1, with_ties = FALSE) %>% 
  ungroup() %>%
  mutate(emotion = factor(emotion, levels = emotion_levels)) 

#Convertimos a número la columna de duración y creamos tramos de duración 
songs_full_dur <- songs_full %>% 
  separate(length, into = c("min", "sec"), sep = ":", convert = TRUE) %>%
  mutate(
    length = min * 60 + sec,
    length_min = length / 60
  ) %>% 
  select(-min, -sec) 

songs_full_dur_bins <- songs_full_dur %>% 
  mutate( 
    length_bin = cut( 
      length, 
      breaks = c(0, 120, 180, 240, 300, Inf), 
      labels = c("<2 min", "2–3", "3–4", "4–5", ">5") 
    ) 
  ) 

# Top 12 escritores 
top12_writers <- songs_full %>% 
  distinct(writers, song_id) %>% 
  count(writers, sort = TRUE) %>% 
  slice_head(n = 12) %>% 
  pull(writers) 

songs_top12 <- songs_full %>% 
  filter(writers %in% top12_writers) 

songs_means <- songs_top12 %>% 
  group_by(writers, emotion) %>% 
  summarise(value = mean(value), .groups = "drop") %>% 
  mutate(emotion = fct_relevel(emotion, !!!emotion_levels)) 
songs_top12 <- songs_top12 %>% 
  mutate(emotion = fct_relevel(emotion, !!!emotion_levels)) 

songs_top12_dur <- songs_full_dur %>% 
  filter(writers %in% top12_writers)

writers_length_mean <- songs_full_dur %>% 
  filter(writers %in% top12_writers) %>% 
  group_by(writers) %>% 
  summarise(mean_length_min = mean(length_min, na.rm = TRUE),
            .groups = "drop")

# Dataset para mapa de calor por canción 
songs_heatmap <- songs_full_no_pivot %>% 
  select(song, album, track, joy, surprise, neutral, sadness, anger, fear, disgust) %>%
  pivot_longer(cols = joy:disgust, names_to = "emotion", values_to = "value")

songs_heatmap <- songs_heatmap %>%
  mutate(
    album = factor(album, levels = album_levels),
    track = factor(track, levels = sort(unique(track))),
    emotion = factor(emotion, levels = emotion_levels)
  )

# Para saber el número máximo de pista
max_track <- max(emotions_song_pivot$track, na.rm = TRUE)

# =========================================
# 5. VISUALIZACIONES
# =========================================

# 5.1 ANÁLISIS POR ÁLBUM

# Gráficos de barras del nivel de cada emoción de los álbumes 
ggplot(
  emotions_song_mean_pivot, 
  aes(x= album, y= value, fill = album)
       ) + 
  geom_col(color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = album_colors) + 
  facet_wrap(~emotion) + 
  coord_cartesian(ylim = c(0, 1))+
  labs(
    title = "Intensidad emocional media por álbum (media canciones)",
    x = NULL,
    y = "Intensidad media",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(), 
    plot.title = element_text(hjust = 0.5)  
  ) 

ggplot(
  emotions_album_pivot, 
  aes(x= album, y= value, fill = album)
       ) + 
  geom_col(color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = album_colors) + 
  facet_wrap(~emotion) + 
  coord_cartesian(ylim = c(0, 1))+
  labs(
    title = "Intensidad emocional por álbum (análisis global)",
    x = NULL,
    y = "Intensidad",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  )

# Mismos gráficos anteriores, pero con la escala de y distinta según los valores
ggplot(
  emotions_song_mean_pivot, 
  aes(x= album, y= value, fill = album)
  ) + 
  geom_col(color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = album_colors) + 
  facet_wrap(~emotion, scales = "free_y")  + 
  labs(
    title = "Intensidad emocional media por álbum (media canciones)",
    x = NULL,
    y = "Intensidad media",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  )

ggplot(
  emotions_album_pivot, 
  aes(x= album, y= value, fill = album)
  ) + 
  geom_col(color = "black", linewidth = 0.2) +  
  scale_fill_manual(values = album_colors) + 
  facet_wrap(~emotion, scales = "free_y")  + 
  labs(
    title = "Intensidad emocional por álbum (análisis global)",
    x = NULL,
    y = "Intensidad",
    fill = "Álbum"
  )  +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  )

# Mapa de calor emociones por álbum
ggplot(
  emotions_song_mean_pivot, 
  aes(x = album, y = emotion, fill = value)
  ) +
  geom_tile() +
  scale_fill_gradientn(colours=c("navy", "purple", "violet", "pink", "white")) +
  labs(
    title = "Intensidad emocional media por álbum",
    x = "Álbum",
    y = "Emoción",
    fill = "Intensidad media"
    ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

#Gráficos radar de cada álbum
for(a in album_levels){
  data_album <- emotions_song_mean %>%
    filter(album == a)
  
  print(
    ggradar(data_album,
            group.colours = album_colors[a],
            grid.min = 0,
            grid.mid = 0.5,
            grid.max = 1,
            background.circle.colour = "gray20",
            plot.title = paste("Perfil emocional:", a))
  )
}

#Gráficos de violín distribución de las emociones en los álbumes
ggplot(
  emotions_song_pivot, 
  aes(x = album, y = value, fill = album)
  ) +
  geom_violin(alpha = 0.6, color= "black", linewidth = 0.05) +
  scale_fill_manual(values = album_colors) +
  facet_wrap(~ emotion, scales = "free_y") +
  labs(
    title = "Distribución de la intensidad emocional por álbum",
    x = NULL,
    y = "Intensidad",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    plot.title = element_text(hjust = 0.5) 
  )

#------------------------------------------------------------------

# 5.2. ANÁLISIS POR CANCIONES


# Gráfico de líneas variación emociones a lo largo de las canciones de los discos
ggplot(
  emotions_song_pivot, 
  aes(x = track, y = value, color = emotion)
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(~ album, scales= "free_x") +
  scale_color_manual(values = emotion_colors) +
  scale_x_continuous(breaks = 1:max_track) +
  labs(
    title = "Variación de la intensidad emocional a lo largo del álbum",
    x = "Número de pista",
    y = "Intensidad",
    color = "Emoción"
  ) +
  theme_minimal()+
  theme(
    plot.title = element_text(hjust = 0.5) 
  )

# Mismo gráfico, pero con las líneas suavizadas para apreciar mejor las tendencias y cambios.
ggplot(
  emotions_song_pivot, 
  aes(x = track, y = value, color = emotion)
  ) +
  geom_smooth(se = FALSE, method = "loess", linewidth = 1.2) +
  facet_wrap(~ album, scales= "free_x") +
  scale_color_manual(values = emotion_colors) +
  scale_x_continuous(breaks = 1:max_track) +
  labs(
    title = "Tendencia suavizada de la intensidad emocional por álbum",
    x = "Número de pista",
    y = "Intensidad",
    color = "Emoción"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Distribución emoción por canción en cada álbum
ggplot(
  emotions_song_pivot, 
  aes(x = emotion, y = value, color = emotion)
  ) +
  geom_boxplot(alpha = 0.3, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.6, size = 1.5)+
  facet_wrap(~ album) +
  scale_color_manual(values = emotion_colors) +
  labs(
    title = "Distribución y dispersión de las emociones por álbum",
    x = NULL,
    y = "Intensidad",
    color = "Emoción"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_blank()
    )

# Mapa de calor emoción por canción
ggplot(
  songs_heatmap, 
  aes(x = emotion, y = factor(track), fill = value)
  ) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colours=c("navy", "purple", "violet", "pink", "white")) +
  facet_wrap(~ album, scales = "free_y") +
  labs(
    title = "Mapa de calor: emociones por canción y álbum",
    x = "Emoción",
    y = "Número de pista",
    fill = "Intensidad"
    ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 7),
    plot.title = element_text(hjust = 0.5)
  ) 

#Canciones con emociones más altas
ggplot(
  top_songs, 
  aes(x = emotion, y = value, fill = album)
  ) +
  geom_col(color = "black") +
  geom_text(aes(label = song),
            vjust = -0.5,
            size = 1.8,
            angle= 8,
            hjust=0) +
  scale_fill_manual(values = album_colors) +
  labs(
    title = "Canción con mayor intensidad por emoción",
    x = "Emoción",
    y = "Intensidad",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        )

# Canciones con las emociones más bajas
ggplot(
  low_songs, 
  aes(x = emotion, y = value, fill = album)
  ) +
  geom_col(color = "black") +
  geom_text(aes(label = song),
            vjust = -0.5,
            size = 1.8,
            angle= 8,
            hjust=0) +
  scale_fill_manual(values = album_colors) +
  labs(
    title = "Canción con menor intensidad por emoción",
    x = "Emoción",
    y = "Intensidad",
    fill = "Álbum"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)
        )


#------------------------------------------------------------------

# 5.3. ANÁLISIS POR AUTOR

# Gráfico boxplot y de puntos con los niveles de emociones por escritor
ggplot(
  songs_top12, 
  aes(x = emotion, y = value, fill = emotion)
  ) +
  geom_boxplot(color = NA) +  
  geom_point(data = songs_means, aes(x = emotion, y = value),
             color = "black", size = 2, inherit.aes = FALSE) +  
  facet_wrap(~ writers, scales = "fixed") +  
  scale_fill_manual(values = emotion_colors) +
  labs(
    title = "Distribución de emociones por escritor",
    x = NULL,
    y = "Intensidad media",
    fill = "Emoción"
  ) +
  theme_minimal() +
  theme(  axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5) 
  )

# Gráfico columnas con el nivel promedio de cada emoción por escritor
ggplot(
  songs_means, 
  aes(x = emotion, y = value, fill = emotion)
  ) +
  geom_col() +  
  facet_wrap(~ writers, scales = "fixed") +  
  scale_fill_manual(values = emotion_colors) +
  labs(
    title = "Nivel promedio de cada emoción por escritor",
    x = NULL,
    y = "Intensidad media",
    fill = "Emoción"
  ) +
  theme_minimal() +
  theme(  axis.text.x = element_blank(),
          plot.title = element_text(hjust = 0.5) 
  )

# Gráfico de columnas con la duración media de las canciones de cada escritor
ggplot(
  writers_length_mean,
  aes(x = reorder(writers, mean_length_min), 
      y = mean_length_min)
) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Duración media de las canciones por escritor",
    x = "Escritor",
    y = "Duración media (minutos)"
  ) +
  scale_y_continuous(
    breaks = seq(0, 6, by = 0.25)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )


#------------------------------------------------------------------

# 5.4. ANÁLISIS EXPLORATORIO

# Intensidad de las canciones según la duración de la canción
ggplot(
  songs_full_dur_bins, 
  aes(x = length, y = value, color = emotion)
  ) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ emotion) +
  scale_color_manual(values = emotion_colors)+
  labs(
    title = "Relación entre duración de la canción y la intensidad emocional",
    x = "Duración (segundos)",
    y = "Intensidad",
    color = "Emoción"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Tendencias de las emociones según la duración de la canción
ggplot(
  songs_full_dur_bins, 
  aes(x = length_bin, y = value, color = emotion, group = emotion)
  ) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun = mean, geom = "point", size = 2) +
  facet_wrap(~ emotion) +
  scale_color_manual(values = emotion_colors)+
  labs(
    title = "Tendencia de la intensidad emocional según la duración de la canción",
    x = "Duración (minutos)",
    y = "Intensidad media",
    color = "Emoción"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Emociones por año
ggplot(
  songs_full, 
  aes(x = year, y = value, color=emotion)
  ) +
  facet_wrap(~ emotion)+
  scale_color_manual(values = emotion_colors)+
  geom_line() +
  geom_smooth()+
  labs(title = "Tendencia de las emociones a lo largo de los años",
       x = "Año",
       y = "Intensidad",
       color = "Emoción") +
  theme_minimal() +
  theme(axis.text.x = element_text(),
          plot.title = element_text(hjust = 0.5)
          )




