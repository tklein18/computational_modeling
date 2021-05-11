# this script will analyze the results of 
# my simulated stock market 


setwd(file.path(
  "", "Users", "tklein", "Desktop", "JHU_Classes",
  "computational_modeling", "Research_Assignment"
))




# libraries
library(tidyverse)
library(scales)



# reading in the data ===========================================


simulation_results <- read_csv(
  file.path("simulation_output", "artificial_stock_market_v3 experiment-table.csv"), 
  skip = 6
)



# data exploration ====================================================



summary(simulation_results$`current-price`)


simulation_results %>% 
  group_by(`sd-ni-growth`) %>% 
  summarize(
    min_price = min(`current-price`), 
    max_price = max(`current-price`), 
    mean_price = mean(`current-price`)
  )


simulation_results %>% 
  group_by(`mean-ni-growth`) %>% 
  summarize(
    min_price = min(`current-price`), 
    max_price = max(`current-price`), 
    mean_price = mean(`current-price`)
  )


simulation_results %>% 
  group_by(`num-rational`) %>% 
  summarize(
    min_price = min(`current-price`), 
    max_price = max(`current-price`), 
    mean_price = mean(`current-price`)
  )




simulation_results %>% 
  ggplot(aes(`current-price`))+ 
  geom_density(aes(fill = as.character(`num-rational`)), alpha = .3)+
  scale_x_continuous(limits = c(0, 20))+
  scale_fill_discrete(name = "num-rational")+
  theme_bw()+
  ggtitle("Distributions of Market Price by Number of Rational Agents")+
  ggsave(file.path("R_output", "num_rational_density_plot.png"), width = 7, height = 3.2)



simulation_results %>% 
  mutate(
    mean_ni_growth = percent(`mean-ni-growth`), 
    sd_ni_growth = percent(`sd-ni-growth`), 
    sd_ni_growth = factor(sd_ni_growth, levels = c("12.20%", "7.30%", "2.70%"))
  ) %>% 
  ggplot(aes(`current-price`))+ 
  geom_density(aes(fill = as.character(`num-rational`)), alpha = .3)+
  scale_x_continuous(limits = c(0, 20))+
  scale_fill_discrete(name = "num-rational")+
  theme_bw()+
  facet_grid(mean_ni_growth ~ sd_ni_growth, scales = "free_y")+
  ggsave(file.path("R_output", "density_plot_grid.png"), width = 7, height = 3.2)








# defining bubbles ===========================================

# going to create two columns that calculate the 
# fair market price at every point in time for every simulation
# based on a two sigma and three sigma net income growth 
# if the price is above these levels, then the market price
# is clearly in a state of exuberance 


sim_results_with_bubs <- simulation_results %>% 
  mutate(
    two_sig_expected_price = (`current-ni` *
                                (1 + (`mean-ni-growth` + 2*(`sd-ni-growth`)))^4) / 1000 * 20 / 1.05, 
    three_sig_expected_price = `current-ni` *
      (1 + (`mean-ni-growth` + 3*(`sd-ni-growth`)))^4 / 1000 * 20 / 1.05, 
    bubble_two = if_else(`current-price` > two_sig_expected_price, 1, 0), 
    bubble_three = if_else(`current-price` > three_sig_expected_price, 1, 0)
  )



sim_results_with_bubs %>% 
  mutate(
    mean_ni_growth = percent(`mean-ni-growth`), 
    sd_ni_growth = percent(`sd-ni-growth`), 
    sd_ni_growth = factor(sd_ni_growth, levels = c("12.20%", "7.30%", "2.70%")),
    num_rational = factor(`num-rational`, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_ni_growth, sd_ni_growth, `[run number]`) %>% 
  summarize(
    number_days_bubble = sum(bubble_two)
  ) %>% 
  group_by(num_rational, mean_ni_growth, sd_ni_growth) %>% 
  summarize(mean_days_bubble = mean(number_days_bubble)) %>% 
  ggplot(aes(num_rational, mean_days_bubble))+ 
  geom_col()+
  theme_bw()+
  facet_grid(mean_ni_growth ~ sd_ni_growth, scales = "fixed")+
  ggsave(file.path("R_output", "two_bubble_days.png"), width = 7, height = 3.2)





sim_results_with_bubs %>% 
  mutate(
    mean_ni_growth = percent(`mean-ni-growth`), 
    sd_ni_growth = percent(`sd-ni-growth`), 
    sd_ni_growth = factor(sd_ni_growth, levels = c("12.20%", "7.30%", "2.70%")),
    num_rational = factor(`num-rational`, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_ni_growth, sd_ni_growth, `[run number]`) %>% 
  summarize(
    number_days_bubble = sum(bubble_three)
  ) %>% 
  group_by(num_rational, mean_ni_growth, sd_ni_growth) %>% 
  summarize(mean_days_bubble = mean(number_days_bubble)) %>% 
  ggplot(aes(num_rational, mean_days_bubble))+ 
  geom_col()+
  theme_bw()+
  facet_grid(mean_ni_growth ~ sd_ni_growth, scales = "fixed")+
  ggsave(file.path("R_output", "three_bubble_days.png"), width = 7, height = 3.2)




sim_results_with_bubs %>% 
  filter(
    `mean-ni-growth` == .0390 &
    `sd-ni-growth` == .1220 &
    `num-rational` == 10 &
      `[run number]` == 201
  ) %>% 
  ggplot(aes(x = `[step]`, y = `current-price`))+
  geom_line(aes(group = 1))+
  geom_line(aes(`[step]`, three_sig_expected_price))





