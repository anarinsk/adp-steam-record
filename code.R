# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Load ----
librarian::shelf(tidyverse, here)
df0 <- read_csv(here::here('data/steam-200k.csv'), col_names = F) %>% 
  select(-X5) 
names(df0) <- c('id', 'title', 'behavior', 'time')

# Munge ----


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
  ) -> df3

df3 %>% 
  mutate(
    is_double_purchased = if_else(purchase_count >= 2, T, F)
  ) %>% 
  select(-purchase_count) %>% 
  ungroup() -> dfa

### Prepare df for vis ----
### 1. by title 
### 2. by id 

dfa %>% 
  count(is_double_purchased)

dfa %>% 
  group_by(title) %>% 
  summarise(
    n_purchased = n(), 
    time = sum(time) 
  ) -> vdf_title 

# 상위에 위치한 게임들에 대해서 개별 게임 분포 정도는 그려주자. 


dfa %>% 
  group_by(id) %>% 
  summarise(
    n_purchased = n(), 
    n_played = sum(time >= 0.5), 
    total_time = sum(time), 
    mean_time = total_time / n_played, 
    sh_play = n_played / n_purchased
  ) -> vdf_id 


### 


