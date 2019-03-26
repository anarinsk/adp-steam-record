librarian::shelf(here, ggridges, rlang, tidyverse, magrittr, stringr, knitr, kableExtra, scales, thomasp85/patchwork)


### Load df 

dfa <- readRDS(here::here("data", "df4rmd.rds"))

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
  ) -> df_title 

dfa %>% 
  group_by(id) %>% 
  summarise(
    n_purchased = n(), 
    n_played = sum(time >= 0.5), 
    total_time = sum(time), 
    mean_time = total_time / n_played, 
    sh_play = n_played / n_purchased
  ) -> df_id 

### Functions 

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

