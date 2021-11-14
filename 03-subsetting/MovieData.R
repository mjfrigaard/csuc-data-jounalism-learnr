library(ggplot2movies)
library(tidyverse)


movies <- ggplot2movies::movies
MovieData <- movies %>% 
    filter(year > 2000 & mpaa != "") %>% 
    pivot_longer(cols = c(Action:Short), 
                 names_to = "genre_key", 
                 values_to = "genre_value") %>% 
    select(title:rating, mpaa, genre_key, genre_value) %>% 
    filter(genre_value == 1) %>% 
    mutate(genre_value = case_when(
        genre_key == 'Action' ~ "action",
        genre_key == 'Animation' ~ "animation",
        genre_key == 'Comedy' ~ "comedy",
        genre_key == 'Drama' ~ "drama",
        genre_key == 'Documentary' ~ "documentary",
        genre_key == 'Romance' ~ "romance",
        genre_key == 'Short' ~ "short",
        TRUE ~ NA_character_)) %>% 
    pivot_wider(names_from = genre_key, 
                values_from = genre_value) %>% 
    unite(col = "genres", Action:Short, sep = ", ") %>% 
    mutate(genres = str_remove_all(string = genres, pattern = "NA, "),
           genres = str_remove_all(string = genres, pattern = ", NA"))
# export
# write_csv(x = MovieData, file = "03-subsetting/MovieData.csv")