# Percentage each color was used per season

# Load packages -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(stringr)
library(showtext)
font_add_google("Alfa Slab One", "ASO", regular.wt = 400)
font_add_google("Passion One", "P", regular.wt = 400)
showtext_opts(dpi = 500) 
showtext_auto(TRUE)
library(htmlwidgets)
library(highcharter)
library(purrr)



# Read in the data ------------------------------------
bob_ross <- read_csv(
  "https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv",
) 

# The first column doesn't contain data that we need, so we can remove it

bob_ross <- bob_ross %>% 
  select(-1) %>% 
  clean_names


# Frequency of Color usage per season ------------------------------------------

# create new DF that calculates the usage of each color per season
season_colors <- bob_ross %>% 
  # split up the list of 
  separate_longer_delim(cols = c(color_hex, colors), ", ") %>% 
  # fix the regular expressions for color_hex and colors
  mutate(color_hex = str_remove_all(color_hex, "\\[|\\]|'"),
         # removes the "[" and "]" and "'" and "\\r\\n"
         colors = str_remove_all(colors, "\\[|\\]|'|\\\\r\\\\n"),
         # to remove \\r\\n here's an alternative without regular expressions
         colors = 
           case_when(
             colors == "Phthalo Green\\r\\n" ~ "Phthalo Green",
             colors %in% c("Liquid Clear", "Titanium White") ~ "White",
             colors %in% c("Black Gesso", "Midnight Black", "Liquid Black") ~ "Black",
             .default = paste(colors))
         ) %>% 
  # perform groupwise transformations based on season
  group_by(season) %>% 
  # get frequency of each color's usage
  count(colors) %>% 
  # create new variables 
         # numbe of colors used per season
  mutate(color_sum = sum(n),
         # n/color_sum for percent, then multiply by 100 and round it two digits
         pct = round(((n/sum(n))*100), 2),
         # use percent() function to add % symbol
         pct_lab = percent(pct, scale = 1),
         season = as.factor(season)) %>% 
  ungroup() %>% 
  group_by(colors) %>% 
  mutate(avg_pct = sum(pct)/31)


pal <- c(
  `Alizarin Crimson` = "#4E1500",  
  `Bright Red` = "#DB0000",  
  `Cadmium Yellow` = "#FFEC00",  
  `Phthalo Green` = "#102E3C",  
  `Prussian Blue` = "#021E44",  
  `Sap Green` = "#0A3410",  
  `White` = "#FFFFFF",  
  `Van Dyke Brown` = "#221B15",  
  `Black` = "#000000",  
  `Burnt Umber` = "#8A3324",  
  `Indian Yellow` = "#FFB800",  
  `Phthalo Blue` = "#0C0040",  
  `Yellow Ochre` = "#C79B00",  
  `Dark Sienna` = "#5F2E1F",  
  `Indian Red` = "#CD5C5C")


plot <- season_colors %>% 
  ggplot(., aes(x = season, y = pct, fill = colors)) +
  geom_col(width = 0.8, color = "white",
           position = position_stack(vjust = .5, reverse = TRUE)) +
  geom_label(aes(label = pct_lab), 
             fill = "white", color = "black", size = 2,
             position = position_stack(vjust = .5, reverse = TRUE)) +
  theme_minimal() +
  theme(text = element_text(family = "P"),
        plot.background = element_rect(fill = "#F5F5F5")) +
  scale_fill_manual(values = pal)

ggsave("plot.png", width = 12, height = 10, units = "in")


# stacked graph doesn't look good so let's do a drill down graph with highcharts


# So first graph is going to show average pct used across all seasons
# drill down will show pct for each season

# end goal is to create a dynamic visual that shows average percent each  
# paint color was used over the whole show, and a drilldown feature to 
# show percent of paint used across different seasons.



# initial data cleaning and prep- use this as the base
colors_df <- bob_ross %>% 
  # split up the list of 
  separate_longer_delim(cols = c(color_hex, colors), ", ") %>% 
  # fix the regular expressions for color_hex and colors
  mutate(color_hex = str_remove_all(color_hex, "\\[|\\]|'"),
         # removes the "[" and "]" and "'" and "\\r\\n"
         colors = str_remove_all(colors, "\\[|\\]|'|\\\\r\\\\n"),
         # to remove \\r\\n here's an alternative without regular expressions
         colors = 
           case_when(
             colors == "Phthalo Green\\r\\n" ~ "Phthalo Green",
             colors %in% c("Liquid Clear", "Titanium White") ~ "White",
             colors %in% c("Black Gesso", "Midnight Black", "Liquid Black") ~ "Black",
             .default = paste(colors))
  ) 


# building a highchart with drill down at once --------------------------------
# new df that has the frequency of each color usage for base plot
avg_freq <- colors_df %>% 
  # get the total freq of each color
  count(colors) %>% 
  # get the overall percentage of each color's usage
  mutate(pct = (n/sum(n))*100,
         pct_lab = percent(pct, accuracy = 0.1, scale = 1)) %>% 
  arrange(desc(pct))

# nested data for drill down
by_season_nest <- colors_df %>% 
  # group by colors then season
  group_by(colors, season) %>% 
  # get the number of paintings for each color by season
  summarize(paintings = n()) %>% 
  # add overall pct usage of each color
  left_join(avg_freq) %>% 
  # ungrup the data to perform new analysis
  ungroup() %>% 
  # perform group-wise calculations by seaso
  group_by(season) %>% 
  # get pct each color was used for each season
  mutate(pct_season = round((paintings/sum(paintings))*100, 2),
         pct_season_lab = percent(pct_season, scale = 1,  accuracy = 0.1)) %>% 
  # ungroup the data
  ungroup() %>% 
  # group the data by colors
  group_by(colors, .add = TRUE) %>% 
  #create nested data at parent level - colors
  group_nest() %>% 
  mutate(
    #id should be set to parent level
    id = colors,
    #type specifies chart type
    type = "column",
    #drilldown data should contain arguments for chart - use purrr to map
    data = purrr::map(data, mutate, name = season, y = pct_season),
    data = purrr::map(data, list_parse)
  )

# plot the results
hchart(avg_freq, 
       type = "bar", 
       hcaes(x = colors, y = pct, color = colors, drilldown = colors)) %>% 
  hc_title(text = "Overall Usage Percentage of Each Color") %>% 
  hc_drilldown(activeAxisLabelStyle = list(textDecoration = "none"),
               allowPointDrilldown = TRUE,
               series = list_parse(by_season_nest))



# getting a donut chart ---------------------------------------------------
# new df that has the frequency of each color usage
avg_freq <- colors_df %>% 
  # get the total freq of each color
  count(colors) %>% 
  # get the overall percentage of each color's usage
  mutate(pct = (n/sum(n))*100,
         pct_lab = percent(pct, accuracy = 0.1, scale = 1))
  
# create a basic donut chart
donut_chart <- avg_freq %>% 
  #set up highchart object
  hchart("pie", 
         #mapping for pie chart
         hcaes(x = colors, y = pct, drilldown = colors), 
         name="Colors") %>% 
  #add title
  hc_title(text="By Color") %>% 
  # this make it a donut chart
  hc_plotOptions(pie = list(innerSize="70%"))

donut_chart
  
# we need to create a new data set aggregated that contains our drilldown 
# information in several lists with the suitable mappings and arguments for 
# a new highcharter object

by_season_nest <- colors_df %>% 
  # group by colors then season
  group_by(colors, season) %>% 
  # get the number of paintings for each color by season
  summarize(paintings = n()) %>% 
  # add overall pct usage of each color
  left_join(avg_freq) %>% 
  # ungrup the data to perform new analysis
  ungroup() %>% 
  # perform group-wise calculations by seaso
  group_by(season) %>% 
  # get pct each color was used for each season
  mutate(pct_season = round((paintings/sum(paintings))*100, 2),
         pct_season_lab = percent(pct_season, scale = 1,  accuracy = 0.1)) %>% 
  # ungroup the data
  ungroup() %>% 
  # group the data by colors
  group_by(colors, .add = TRUE) %>% 
  #create nested data at parent level - colors
  group_nest() %>% 
  mutate(
    #id should be set to parent level
    id = colors,
    #type specifies chart type
    type = "column",
    #drilldown data should contain arguments for chart - use purrr to map
    data = purrr::map(data, mutate, name = season, y = pct_season),
    data = purrr::map(data, list_parse)
  )


drilldown_chart <- donut_chart %>% 
  hc_drilldown(
    #map to data
    series = list_parse(by_season),
    allowPointDrilldown = TRUE,
    #set stylings of data labels that offer drill down views
    activeDataLabelStyle = list(
      textDecoration="none",
      color="black"
    )
  )

drilldown_chart



final_chart<-drilldown_chart|>
  #relabel x Axis
  hc_xAxis(title = list(text="Season"))|>
  #relabel y Axis
  hc_yAxis(title = list(text="Percent"))|>
  #reorder column charts by y Axis
  hc_plotOptions(column = list(
    dataSorting = list(enabled = FALSE)
  )
  )|>
  #customize drilldown & drillup events
  hc_chart(
    events = list(
      drilldown = JS(
        "function(){
               this.title.update({text: 'Usage per Season'})
               this.update({
                  xAxis:{visible:true},
                  yAxis:{visible:true}
               })
               }"
      ),
      drillup =  JS("function() {
              this.title.update({text: 'Average Usage Of Each Color in One Season'})
              this.update({
                xAxis:{visible:false},
                yAxis:{visible:false}
               })
             }")
    ))

final_chart


pal <- c(
  "#4E1500", 
  "#000000",  
  "#DB0000",  
  "#8A3324",  
  "#FFEC00",  
  "#5F2E1F",  
  "#CD5C5C",
  "#FFB800",  
  "#0C0040",
  "#102E3C",  
  "#021E44",  
  "#0A3410", 
  "#221B15", 
  "#FFFFFF",  
  "#C79B00")


#create and save theme as new variable
custom_theme <- hc_theme(
  colors = pal,
  chart = list(
    backgroundColor = NULL
  ),
  title = list(
    style = list(
      color = "#333333",
      fontFamily = "Archivo",
      fontWeight="bold"
    )
  ),
  xAxis = list(
    labels=list(style = list(
      color = "#666666",
      fontFamily = "Archivo"
    ))
  ),
  yAxis = list(
    labels=list(style = list(
      color = "#666666",
      fontFamily = "Archivo"
    ))
  ),
  tooltip = list(
    style = list(
      fontFamily = "Archivo"
    )
  ),
  plotOptions = list(
    series = list(
      dataLabels = list(style=list(fontFamily = "Archivo")
      ))
  )
)


final_chart %>% 
  #add theme
  hc_add_theme(custom_theme)


# building a highchart with drill down at once --------------------------------
# new df that has the frequency of each color usage for base plot
avg_freq <- colors_df %>% 
  # get the total freq of each color
  count(colors) %>% 
  # get the overall percentage of each color's usage
  mutate(pct = (n/sum(n))*100,
         pct_lab = percent(pct, accuracy = 0.1, scale = 1)) %>% 
  arrange(desc(pct))

# nested data for drill down
by_season_nest <- colors_df %>% 
  # group by colors then season
  group_by(colors, season) %>% 
  # get the number of paintings for each color by season
  summarize(paintings = n()) %>% 
  # add overall pct usage of each color
  left_join(avg_freq) %>% 
  # ungrup the data to perform new analysis
  ungroup() %>% 
  # perform group-wise calculations by seaso
  group_by(season) %>% 
  # get pct each color was used for each season
  mutate(pct_season = round((paintings/sum(paintings))*100, 2),
         pct_season_lab = percent(pct_season, scale = 1,  accuracy = 0.1)) %>% 
  # ungroup the data
  ungroup() %>% 
  # group the data by colors
  group_by(colors, .add = TRUE) %>% 
  #create nested data at parent level - colors
  group_nest() %>% 
  mutate(
    #id should be set to parent level
    id = colors,
    #type specifies chart type
    type = "column",
    #drilldown data should contain arguments for chart - use purrr to map
    data = purrr::map(data, mutate, name = season, y = pct_season),
    data = purrr::map(data, list_parse)
  )

# plot the results
hchart(avg_freq, 
       type = "bar", 
       hcaes(x = colors, y = pct, color = colors, drilldown = colors)) %>% 
  hc_title(text = "Overall Usage Percentage of Each Color") %>% 
  hc_drilldown(activeAxisLabelStyle = list(textDecoration = "none"),
               allowPointDrilldown = TRUE,
               series = list_parse(by_season_nest))
  





















