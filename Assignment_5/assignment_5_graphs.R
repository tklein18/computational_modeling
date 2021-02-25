# this script will create two graphs
# for assignment 5 of my computational modeling class
# the graphs will be the results of simulations
# from two models of butterfly movements


setwd(
  paste(
    '/Users/tklein/Desktop/JHU_Classes/computational_modeling', 
    '/Assignment_5', sep = ''
    )
)


# libs
library(tidyverse)


# reading in the data =====================================

# the data is stored in two different folders
# and there are multiple files
# each file has data from a model run with a certain value of q
# and that value is stored above the data in the csv file
# this for loop will go to each folder
# find all the files in that folder
# and then read in each file
# add the value of q and model type as a variable to the data
# and then bind the data to data frame of all the data

model_results <- data.frame()

for(i in c('artificial_results', 'real_results')){
  
  data_files <- list.files(path = i)
  
  for(f in data_files){
    
    
    temp_data <- read.csv(
      paste(
        i, f, sep = '/'
      ), 
      skip = 16
    )
    
    temp_q <- read.csv(
      paste(
        i, f, sep = '/'
      ), 
      skip = 5, nrows = 1
    )
    
    temp_q_value <- temp_q %>% pull(selected_q_value)
    
    temp_data <- temp_data %>% 
      mutate(
        q_value = temp_q_value, 
        model_type = i
      )
    
    
    model_results <- bind_rows(model_results, temp_data)
    
    
  }
  
  
}





# creating graphs ===========================================




graphing_data <- model_results %>% 
  mutate(
    q_value_factor = factor(q_value, levels = as.character(seq(0, 1, .1)))
  ) %>% 
  filter(
    q_value_factor %in% c('0.1', '0.3', '0.5', '0.7', '0.9')
  )




# creating the artificial results first

graphing_data %>% 
  filter(
    model_type == 'artificial_results'
  ) %>% 
  ggplot(aes(x, y))+
  geom_line(aes(color = q_value_factor))+
  labs(
    y = 'Corridor Width',
    x = 'Tick', 
    color = 'q'
    )+
  scale_color_viridis_d(option = 'D')+
  theme_bw()+
  theme(legend.position = c(.9, .75))+
  ggtitle(
    'Corridor Width - Artificial Landscape Models'
  )+
  ggsave(
    'Klein_assignment5_artificial_graph.png', width = 13, height = 6
  )




# creating the real results 

graphing_data %>% 
  filter(
    model_type == 'real_results'
  ) %>% 
  ggplot(aes(x, y))+
  geom_line(aes(color = q_value_factor))+
  labs(
    y = 'Corridor Width',
    x = 'Tick', 
    color = 'q'
  )+
  scale_color_viridis_d(option = 'D')+
  theme_bw()+
  theme(legend.position = c(.9, .75))+
  ggtitle(
    'Corridor Width - Real Landscape Models'
  )+
  ggsave(
    'Klein_assignment5_real_graph.png', width = 13, height = 6
  )




