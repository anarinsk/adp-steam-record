# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Load ----
librarian::shelf(tidyverse)
df0 <- read_csv(here::here('data/steam-200k.csv'), col_names = F) %>% 
  select(-X5) 
names(df0) <- c('id', 'title', 'behavior', 'time')

# Munge ----
df0 %>% 
  filter(behavior == "play") %>% 
  count(title, sort = T)

df0 %>% 
  filter(behavior == "play") %>% 
  group_by(title) %>% 
  summarise(
    total_time = sum(time), 
    total_run = n()
  ) %>% 
  arrange(desc(total_time), desc(total_run)) 

df0 %>% 
  filter(behavior == "purchase") %>% 
  count(title, sort = T)



## Two dist 

## 
