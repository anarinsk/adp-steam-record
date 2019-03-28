# From ----
# https://www.kaggle.com/tamber/steam-video-games

# Do munging & save rds 

librarian::shelf(rlang, qwraps2, janitor, stringr, 
                 knitr, kableExtra, scales, thomasp85/patchwork,
                 ggridges)
source(here::here('code_munge.R'))
saveRDS(dfa, here::here("data/df4rmd.rds")) 

### Prepare df for basic stat ----
### 1. by title 
### 2. by id 

dfa %>% 
  group_by(title) %>% 
  summarise(
    n_adopted = n(), 
    n_played = sum(play_count > 0), 
    time = sum(time)
  ) %>% 
  ungroup() %>%  
  mutate(
    sh_time = time/ sum(time)
  ) -> df_title 

dfa %>% 
  group_by(id) %>% 
  summarise(
    n_adopted = n(), 
    n_played = sum(time >= 0.5), 
    total_time = sum(time), 
    mean_time = total_time / n_played, 
    sh_play = n_played / n_adopted
  ) -> df_id 

### Make summary table 

get_unitsummary <- function(df, var){
#  
  #enquo_var <- enquo(var)
  #var_name <- deparse(substitute(var)) 
  var_name <- var 
  
  df %>% 
    summarise(
      n = n(), 
      max = max(get(var)), 
      min = min(get(var)), 
      mean = mean(get(var)), 
      med = median(get(var)), 
      sd = sd(get(var))
    ) -> res
  
  res %>% 
    mutate(var = var_name) %>% 
    select(var, everything())
}

c("n_adopted", "time") %>% map_df(function(x){get_unitsummary(df_title, x)}) %>% kable() %>% kable_styling()

c("n_adopted", "n_played", "total_time", "sh_play") %>% 
  map_df(function(x){get_unitsummary(df_id, x)}) %>% kable() %>% kable_styling()


### Basic Histogram 

plot_hist <- function(var, df, logten=T, title = "TITLE"){
#  
  enquo_var <- enquo(var)
  
  df %>% 
    ggplot() + 
    geom_histogram() + 
    labs(title = title) + 
    theme_bw() -> basic 
  
  if (logten==T) {basic + aes(x = log10(!!enquo_var+1))} 
  else {basic + aes(x = !!enquo_var)}
#
}

# With thomas85/patchwork
p1 <- plot_hist(n_adopted, df_id, logten=T, "Dist. of Adoption by Individual Players")
p2 <- plot_hist(total_time, df_id, logten=T, "Dist. of Playing Time by Individual Players")

p1 + p2

p1 <- plot_hist(n_adopted, df_title, logten=T, "Dist. of Adoption by Title")
p2 <- plot_hist(time, df_title, logten=T, "Dist. of Playing Time by Title")

p1 + p2

plot_hist(sh_play, df_id, logten=F, "Dist. of Playing over Purchasing")
plot_hist(sh_play, df_id %>% filter(between(sh_play, 0.01, 0.99)), logten=F, "Dist. of Playing over Purchasing")

## top_20 by ID and Title time 

df_id %>% arrange(desc(total_time)) %>% top_n(20) %>% 
  kable() %>% kable_styling()

df_id %>% arrange(desc(n_adopted)) %>% top_n(20) %>% 
  kable() %>% kable_styling()

df_title %>% arrange(desc(time)) %>% top_n(20) %>% 
  mutate(sh_time = percent(sh_time)) %>% 
  kable() %>% kable_styling()

### ridges ---

dfa %>% count(title, sort=T) %>% slice(1:20) %>% mutate(rank=row_number()) %>% select(-n) -> title_top

dfa %>% 
  filter(title %in% title_top$title) %>% 
  left_join(title_top, by = c('title')) %>% 
  ggplot() +
  aes(x = log10(time+1), y = fct_reorder(title, 1/rank)) +
  geom_density_ridges(jittered_points = T, point_alpha=0.1) +
  theme_bw(base_size = 11) +
  labs(y = "Title by rank of sales") + 
  labs(title = "Play Time Pattern of Top-adopted STEAM Titles")


df_id %>% arrange(desc(n_adopted)) %>% 
  slice(1:20) %>% mutate(rank=row_number()) %>% select(rank, id) -> id_top_adopted

df_id %>% arrange(desc(total_time)) %>% 
  slice(1:20) %>% mutate(rank=row_number()) %>% select(rank, id) -> id_top_time

dfa %>% 
  filter(id %in% id_top_adopted$id) %>% 
  left_join(id_top_adopted, by = c('id')) %>% 
  group_by(id, title) %>% 
  summarise(
    time = mean(time), 
    rank = head(rank)
  ) %>% 
  ggplot() +
  aes(x = log10(time+1), y = fct_reorder(as.factor(rank), 1/rank)) +
  geom_density_ridges(jittered_points = T, point_alpha=0.1) +
  theme_bw(base_size = 11) +
  labs(y = "ID by adoption level") + 
  labs(title = "Play Time Pattern of STEAM users I")

dfa %>% 
  filter(id %in% id_top_time$id) %>% 
  left_join(id_top_time, by = c('id')) %>% 
  group_by(id, title) %>% 
  summarise(
    time = mean(time), 
    rank = head(rank)
  ) %>% 
  ggplot() +
  aes(x = log10(time+1), y = fct_reorder(as.factor(rank), 1/rank)) +
  geom_density_ridges(jittered_points = T, point_alpha=0.1) +
  theme_bw(base_size = 11) +
  labs(y = "ID by total play time") + 
  labs(title = "Play Time Pattern of STEAM users II")

### Simple Query ----

denom <- 12393

df_id %>% filter(total_time >= 100) %>% nrow() -> num
percent(num / denom)

df_id %>% filter(sh_play >= 0.5) %>% nrow() -> num
percent(num / denom)

10^0.5

 