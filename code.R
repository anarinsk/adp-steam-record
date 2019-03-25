# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Load ----
librarian::shelf(tidyverse, here, ggridges, viridis)
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
    n_played = sum(play_count > 0), 
    time = sum(time)
  ) %>% 
  ungroup() %>%  
  mutate(
    sh_time = time/ sum(time)
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


### Visualization by id players 
### Total Histogram 
### Exclude 0, 1 
### 

vdf_id %>% 
  ggplot() + 
  aes(x = sh_play) + 
  geom_histogram()

vdf_id %>% 
  filter(between(sh_play, 0.001, 0.999)) %>% 
  ggplot() + 
  aes(x = sh_play) + 
  geom_density(adjust=1, color = "green", size=2, alpha=0.5) + 
  geom_histogram(aes(y=..density..* 0.4), binwidth = 0.025) +
  theme_bw(base_size = 11) + 
  scale_color_viridis() + 
  labs(x = "share of playing title ordered") + 
  labs(title = "Distribution of playing/buying in STEAM")

vdf_id %>% 
  filter(sh_play == 1) %>% 
  ggplot() + 
  aes(x = n_purchased) + 
  geom_histogram() 

vdf_id %>% 
  filter(sh_play == 0 & n_purchased <= 20) %>% 
  ggplot() + 
  aes(x = n_purchased) + 
  geom_histogram() 


vdf_id %>% 
  filter(n_purchased >= 10) %>% 
  ggplot() + 
  aes(x = sh_play) + 
  geom_histogram() 

#### vdf_title

vdf_title %>% 
  ggplot() + 
  aes(x = log10(time + 1)) + 
  geom_histogram()

vdf_title %>% 
  mutate(avg_time = time / n_purchased) %>% 
  ggplot() + 
  aes(x = log10(avg_time+1)) + 
  geom_histogram()



vdf_title %>%  
  arrange(desc(n_purchased)) %>% 
  top_n(10, n_purchased) %>% 
  mutate(rank = row_number()) %>% 
  select(title, rank) -> title_top


title_top

dfa %>% 
  filter(title %in% title_600 & time <= 50) %>% 
  ggplot() +
  aes(x = time, y = title) +
  geom_density_ridges()


dfa %>% 
  filter(title %in% title_top$title) %>% 
  left_join(title_top, by = c('title')) %>% 
  ggplot() +
  aes(x = log10(time+1), y = fct_reorder(title, 1/rank)) +
  geom_density_ridges(jittered_points = T, point_alpha=0.2) +
  theme_bw(base_size = 11) + 
  scale_color_viridis() + 
  labs(y = "Title by rank of sales") + 
  labs(title = "Distribution of STEAM play time")
  

 
### Some stuff 

max(vdf_id$total_time)

vdf_id %>% 
  mutate(
    cat_share = case_when(
      sh_play == 0 ~ "No playing", 
      sh_play == 1 ~ "Complete playing", 
      TRUE ~ "In-between"
    )
  ) %>% 
  count(cat_share)


