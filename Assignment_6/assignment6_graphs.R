# this script will create the graphs for 
# assignment 6 of my computational modeling cours



setwd(
  "/Users/tklein/Desktop/JHU_Classes/computational_modeling/Assignment_6"
)




# libs
library(tidyverse)


# reading in data ==================================


original_experiment <- read.csv(paste0(
 "experiment_data/", 
"Klein_Assignment6_code blue_fertility_effect_on_red_extinction-table.csv"
  ), skip = 6
)



extended_experiment <- read.csv(
  paste0(
    "experiment_data/", 
    "Klein_Assignment6_code blue_fertility_and_carrying_capacity-table.csv"
  ), skip = 6
)



# data manipulations ======================================


original_experiment_summary <- original_experiment %>% 
  group_by(blue.fertility) %>% 
  summarize(
    red_ext_ticks_mean = mean(ticks), 
    red_ext_ticks_stdv = sd(ticks)
  )



extended_experiment_summary <- extended_experiment %>% 
  group_by(blue.fertility, carrying.capacity) %>% 
  summarize(
    red_ext_ticks_mean = mean(ticks), 
    red_ext_ticks_stdv = sd(ticks)
  )






# graphs ==============================


original_experiment_summary %>% 
  ggplot(aes(blue.fertility, red_ext_ticks_mean))+
  geom_point()+
  geom_errorbar(
    aes(
      ymin = red_ext_ticks_mean - red_ext_ticks_stdv, 
      ymax = red_ext_ticks_mean + red_ext_ticks_stdv
    )
  )+
  scale_x_continuous(
    name = "Blue Fertility", labels = seq(2, 5, .5),
    breaks = seq(2, 5, .5)
  )+
  ylab("Ticks Until Red Extinction")+
  theme_bw()+
  ggtitle(
    paste(
      "Simple Birth Rate Model Results - Red Fertility = 2", 
      "& Carrying Capacity = 1000"
      )
  )+
  ggsave(
    "Klein_Assignment6_original_figure.png", width = 13, height = 6
  )






extended_experiment_summary %>% 
  ggplot(aes(blue.fertility, red_ext_ticks_mean))+
  geom_point(aes(color = as.factor(carrying.capacity)))+
  geom_errorbar(
    aes(
      ymin = red_ext_ticks_mean - red_ext_ticks_stdv, 
      ymax = red_ext_ticks_mean + red_ext_ticks_stdv, 
      color = as.factor(carrying.capacity)
    )
  )+
  scale_x_continuous(
    name = "Blue Fertility", labels = seq(2, 5, .5),
    breaks = seq(2, 5, .5)
  )+
  ylab("Ticks Until Red Extinction")+
  scale_color_viridis_d(
    name = "Carrying Capacity"
  )+
  facet_grid(rows = vars(carrying.capacity))+
  theme_bw()+
  #theme(legend.position = c(.9, .75))+
  ggtitle(
    paste(
      "Simple Birth Rate Model Results - Red Fertility = 2"
    )
  )+
  ggsave(
    "Klein_Assignment6_exercise1_figure.png", width = 13, height = 6
  )












