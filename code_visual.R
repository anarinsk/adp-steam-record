# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Load ----

source(here::here('code_munge.R'))

dfa %>% count(title, sort=T) %>% slice(1:20) %>% mutate(rank=row_number()) %>% select(-n) -> title_top

dfa %>% 
  filter(title %in% title_top$title) %>% 
  left_join(title_top, by = c('title')) %>% 
  ggplot() +
  aes(x = log10(time+1), y = fct_reorder(title, 1/rank)) +
  geom_density_ridges(jittered_points = T, point_alpha=0.1) +
  theme_bw(base_size = 11) + 
  scale_color_viridis() + 
  labs(y = "Title by rank of sales") + 
  labs(title = "Play Time Pattern of Top-adopted STEAM Titles")

