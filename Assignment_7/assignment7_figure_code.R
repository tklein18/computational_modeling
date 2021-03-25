# this script will create the figure for assingment 7 of my computational modeling course



setwd(
  "/Users/tklein/Desktop/JHU_Classes/computational_modeling/Assignment_7"
)



# libs
library(tidyverse)




# reading in data =================



experiment_data <- read.csv(
  'experiment_output/Klein_assignment7_code experiment-table.csv', skip = 6
)








# graphing data =========================


experiment_data %>%
  mutate(
    ticks = as.numeric(X.step.), 
    X..similar.wanted = as.character(X..similar.wanted)
  ) %>% 
  group_by(X..similar.wanted, ticks) %>% 
  summarize(count = mean(count.turtles.with..not.happy..)) %>% 
  ggplot(aes(ticks, count))+
  geom_line(aes(color = X..similar.wanted), size = 1.25)+
  facet_grid(X..similar.wanted ~ .)+
  ylab("Number Unhappy")+
  scale_x_continuous(
    name = "Time",
    breaks = c(0, 10, 20, 40, 80, 160, 320, 500), 
    expand = c(.01,.01)
  )+
  scale_color_viridis_d(
    name = "% Similar \n Wanted"
  )+
  theme_bw()+
  theme(
    panel.grid.minor.x = element_blank()
  )+
  ggtitle(
    "Segreation Model Experiment Results - Average of Unhappy Agents Over Time - Density = 95%"
  )+
  ggsave('Klein_Assignment7_experiment_figure.png', width = 13, height = 6)
