#Bob Ross elements

bob_ross_content <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

# clean up the data
bob_ross_content <- bob_ross_content %>% 
  janitor::clean_names() %>% 
  separate(episode, into = c("season", "episode"), sep = "E") %>% 
  mutate(season = str_extract(season, "[:digit:]+")) %>% 
  mutate_at(vars(season, episode), as.integer) 

sum_elements <- bob_ross_content %>% 
  # remove all variables that contain "frame"
  select(!contains("frame")) %>% 
  mutate(num_elements = rowSums(across(aurora_borealis:winter)))


sum_elements %>% 
  count(num_elements)


  
  
  
  
  
  
  