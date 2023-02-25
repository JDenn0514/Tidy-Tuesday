

# get the color hexes
color_hex <- print(unique_bob_ross_colors)


# sum up the frequency of each color's appearance
color_sum <- bob_ross %>% 
  # sum up the number of times each color appears
  summarize(across(black_gesso:alizarin_crimson, sum)) %>% 
  # pivot table
  pivot_longer(cols = everything(),
               names_to = "color",
               values_to = "count") %>% 
  arrange(desc(count)) %>% 
  # fix color labels so we can use left_join to get hex codes
  # separate the color labels so we can remove the "_"
  separate_wider_delim(color, "_", names = c("first", "second"), too_many = "drop") %>% 
         # use paste0 so there's no space, use str_to_title to capitalize first letter
  mutate(color = paste0(str_to_title(first), str_to_title(second)),
         # fix the VanDykeBrown color
         color = case_when(color == "VanDyke" ~ "VanDykeBrown",
                          .default = paste(color))) %>% 
  # add color_hex values
  left_join(color_hex) %>% 
  # Adjust the labels for axis
  mutate(name = str_to_title(paste(first, second)),
         name = case_when(name == "Van Dyke" ~ "Van Dyke Brown",
                                .default = paste(name)),
         white = case_when(count == 400 ~ "Paintings Used In: 400",
                           count == 51 ~ "51",
                           .default = NA),
         inside = case_when(count %in% c(55:380) ~ paste(count),
                            .default = NA),
         outside = case_when(count %in% c(1, 14) ~ paste(count),
                             .default = NA))

color_sum %>% select(color_hex)
  
pal <- c("#FFFFFF",  
         "#4E1500",  
         "#221B15",  
         "#FFEC00",  
         "#C79B00",  
         "#0C0040",  
         "#DB0000",  
         "#000000",  
         "#0A3410",  
         "#FFB800",  
         "#5F2E1F",  
         "#021E44",  
         "#102E3C",  
         "#000000",  
         "#8A3324",
         "#FFFFFF",  
         "#000000",  
         "#CD5C5C")
  
color_sum %>% 
  ggplot(., aes(x = count, y = fct_reorder(name, count), fill = fct_reorder(name, -count))) +
  geom_col(width = 0.8, color = "gray20") +
  # labels that appear inside the bar
  geom_label(aes(label = inside, color = name), 
             color = pal, fill = "white", hjust = 1, nudge_x = -2, family = "P") +
  # labels that appear outside the bar
  geom_label(aes(label = outside, color = name), 
             color = pal, fill = "white", hjust = 0, nudge_x = 2, family = "P") +
  # white color labels
  geom_label(aes(label = white), 
             color = "gray20", fill = "white", hjust = 1, nudge_x = -2, family = "P") +
  annotate("label", x = 370, y = "Prussian Blue",
           family = "P", size = 3.5, lineheight = 0.9, color = "gray20",
           label = str_wrap("Liquid Black and Midnight Black are have the same hex code", 15)) +
  theme_minimal() +
  # adjust the theme of the graphs
  # convert all text elements to font "P" and size 14
  theme(text = element_text(family = "P", size = 14),
        # convert plot title font to ASO, center it, move it lower, and size to 18
        plot.title = element_text(family = "ASO", hjust = 0.5, vjust = 0.3, size = 18),
        plot.title.position = "plot",
        # change the plot background color to a very light gray/off-white
        plot.background = element_rect(fill = "#F5F5F4"),
        # adjust the plot margins
        plot.margin = margin(15, 30, 15, 15),
        # remove x,y grid lines
        panel.grid = element_blank(),
        # remove legend
        legend.position = "none",
        # remove x-axis
        axis.text.x = element_blank()) +
  scale_fill_manual(values = pal) + 
  labs(x = NULL,
       y = NULL,
       title = str_wrap("The Frequency of Bob Ross's Usage of Each Color in his Paintings", 38),
       subtitle = str_wrap("Liquid Clear was the most commonly used paint color, appearing in 400 out of 403 paintings Bob Ross made.", 55),
       caption = "Visualization: @JDenn0514\nData: Jared Wilber's data on Bob Ross Paintings via @frankiethull {BobRossColors}")

  





