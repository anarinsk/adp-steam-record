# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Load ----
librarian::shelf(tidyverse, here, ggridges, viridis)
df0 <- read_csv(here::here('data/steam-200k.csv'), col_names = F) %>% 
  select(-X5) 
names(df0) <- c('id', 'title', 'behavior', 'time')

# Munge ----


df0 %>% 
  group_by(id, title) %>% 
  summarise(
    is_purchased = if_else("purchase" %in% behavior, T, F)
  ) -> df1 

df0 %>% 
  group_by(id,title) %>% 
  summarise(
    purchase_count = sum(behavior == "purchase"),
    play_count = sum(behavior == "play"), 
    time = sum(time[time != 1]) 
  ) -> df2

df2 %>% 
  mutate(
    is_double_purchased = if_else(purchase_count >= 2, T, F)
  ) %>% 
  select(-purchase_count) %>% 
  ungroup() -> dfa