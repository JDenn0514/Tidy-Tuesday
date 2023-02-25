# Number of Colors used in his paintings

# Load packages -------------------------------------------------------------
library(tidyverse)
library(janitor)
library(stringr)
library(BobRossColors)
library(showtext)
font_add_google("Alfa Slab One", "ASO", regular.wt = 400)
font_add_google("Passion One", "P", regular.wt = 400)
showtext_opts(dpi = 500) 
showtext_auto(TRUE)



# Read in the data ------------------------------------
bob_ross <- read_csv(
  "https://raw.githubusercontent.com/jwilber/Bob_Ross_Paintings/master/data/bob_ross_paintings.csv",
) 

# The first column doesn't contain data that we need, so we can remove it

bob_ross <- bob_ross %>% 
  select(-1) %>% 
  clean_names
  

# Frequency of Number of Colors ------------------------------------------

# create new DF that calculates the freq of num_colors
freq_num_color <- bob_ross %>% 
  # get frequencies
  count(num_colors) %>% 
  # since 2 colors is never used add it as a row
  add_row(num_colors = 2, n = 0) %>% 
  # order the rows by num_colors
  arrange(num_colors) %>% 
  # adjust the num_colors variables
  mutate(num_colors = as.factor(num_colors), # convert to a factor
         # create new var with the n for only num_colors 1-5 and 15
         small = case_when(num_colors %in% c(1:5, 15) ~ paste(n),
                           .default = NA),
         # create new var with the n for only num_colors 6-11 and 13-14
         large = case_when(num_colors %in% c(6:11, 13, 14) ~ paste(n),
                           .default = NA),
         # convert num_colors so it says "100 Paintings"
         most = case_when(num_colors == 12 ~ paste(n, "Paintings"),
                          .default = NA)) 
  
# plot the graph  
num_color <- freq_num_color %>% 
  ggplot(.,aes(y = num_colors, x = n, fill = num_colors)) +
  geom_col(width = 0.8, color = "gray20", fill = "") +
  # add labels outside the bars
  geom_label(aes(label = small), 
             fill = "white",color = "black", hjust = 0, nudge_x = 1, family = "P") +
  # add labels inside the bars
  geom_label(aes(label = large), 
             fill = "white",color = "black", hjust = 1, nudge_x = -1, family = "P") +
  # add labels inside the bars with added text
  geom_label(aes(label = most), 
             fill = "white",color = "black", hjust = 1, nudge_x = -1, family = "P") +
  #annotate(geom_label, )
  theme_minimal() +
  # adjust the theme of the graphs
        # convert all text elements to font "P" and size 14
  theme(text = element_text(family = "P", size = 14),
        # convert plot title font to ASO, center it, move it lower, and size to 18
        plot.title = element_text(family = "ASO", hjust = 0.5, vjust = 0.3, size = 18),
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
  # using the BobRossColors package with the valley_waterfall colors
  scale_fill_bob_ross(painting = "valley_waterfall") +
  scale_x_continuous(expand = expansion(add = c(0.01, 0.05))) +
  # adjust labels
  labs(y = "Number of colors in a painting",
       x = NULL,
       title = str_wrap("The Frequency of the Number of Colors Bob Ross Used in his Paintings", 37),
       subtitle = str_wrap('Bob Ross most frequently used twelve colors, which he used in 100 of his paintings. The fewest he used was one color, which he used in only one painting, the "Contemplative Lady."', 65),
       caption = "Visualization: @JDenn0514\nData: Jared Wilber's data on Bob Ross Paintings via @frankiethull {BobRossColors}")
#print image
print(num_color)

# save image
ggsave("num_color.png", num_color, height = 10, width = 8.25, units = "in", dpi = 400)

# dev.size is set to height = 10 and width = 8.25


