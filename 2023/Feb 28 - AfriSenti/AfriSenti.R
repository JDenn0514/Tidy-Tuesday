afrisenti <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/afrisenti.csv')
languages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/languages.csv')
language_scripts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_scripts.csv')
language_countries <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/language_countries.csv')
country_regions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-28/country_regions.csv')



library(tidyverse)

# add language scripts to the df -----------------------------------------------
afrisenti <- left_join(afrisenti, language_scripts) 

# remove the ary variables from the afrisenti df and make it a new df
scripts <- afrisenti %>% 
  filter(language_iso_code != "ary") 
  
# fix the ary script variable
ary <- afrisenti %>% 
  # only get the tweets from this language
  filter(language_iso_code == "ary") %>% 
  # determine if its an arabic or latin script
  mutate(script = case_when(str_detect(tweet, "[aeiou]") == TRUE ~ "Latin",
                         .default = "Arabic"))

# join the two dfs so its correct
afrisenti <- bind_rows(ary, scripts)


# add the language names to the afrisenti df
afrisenti <- left_join(afrisenti, languages)

















