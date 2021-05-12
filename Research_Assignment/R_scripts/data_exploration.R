# this script will analyze the results of 
# my simulated stock market 


setwd(file.path(
  "", "Users", "tklein", "Desktop", "JHU_Classes",
  "computational_modeling", "Research_Assignment"
))




# libraries
library(tidyverse)
library(scales)
library(rebus)


# reading in the data ===========================================


simulation_results <- read_csv(
  file.path("simulation_output", "artificial_stock_market_v3 experiment-table.csv"), 
  skip = 6
)


new_names <- names(simulation_results) %>%
  str_replace_all(pattern = "-", replacement = "_") %>% 
  str_replace_all(pattern = OPEN_BRACKET, replacement = "") %>% 
  str_replace_all(pattern = CLOSE_BRACKET, replacement = "") %>% 
  str_replace_all(pattern = " ", replacement = "_")

new_names <- c(new_names[1:8], "irrational_cash", "irrational_shares")

names(simulation_results) <- new_names






# data exploration ====================================================



summary(simulation_results$current_price)


simulation_results %>% 
  group_by(sd_ni_growth) %>% 
  summarize(
    min_price = min(current_price), 
    max_price = max(current_price), 
    mean_price = mean(current_price)
  )


simulation_results %>% 
  group_by(mean_ni_growth) %>% 
  summarize(
    min_price = min(current_price), 
    max_price = max(current_price), 
    mean_price = mean(current_price)
  )


simulation_results %>% 
  group_by(num_rational) %>% 
  summarize(
    min_price = min(current_price), 
    max_price = max(current_price), 
    mean_price = mean(current_price)
  )




simulation_results %>% 
  ggplot(aes(current_price))+ 
  geom_density(aes(fill = as.character(num_rational)), alpha = .3)+
  scale_x_continuous(limits = c(0, 20))+
  scale_fill_discrete(name = "num-rational")+
  theme_bw()+
  ggtitle("Distributions of Market Price by Number of Rational Agents")+
  ggsave(file.path("R_output", "num_rational_density_plot.png"), width = 7, height = 3.2)



simulation_results %>% 
  mutate(
    mean_ni_growth = percent(mean_ni_growth), 
    sd_ni_growth = percent(sd_ni_growth), 
    sd_ni_growth = factor(sd_ni_growth, levels = c("12.20%", "7.30%", "2.70%"))
  ) %>% 
  ggplot(aes(current_price))+ 
  geom_density(aes(fill = as.character(num_rational)), alpha = .3)+
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
    two_sig_expected_price = (current_ni *
                                (1 + (mean_ni_growth + 2*(sd_ni_growth)))^4) / 1000 * 20 / 1.05, 
    three_sig_expected_price = current_ni *
      (1 + (mean_ni_growth + 3*(sd_ni_growth)))^4 / 1000 * 20 / 1.05,
    two_sig_low_price = (current_ni *
                                (1 - (mean_ni_growth + 2*(sd_ni_growth)))^4) / 1000 * 20 / 1.05, 
    three_sig_low_price = current_ni *
      (1 - (mean_ni_growth + 3*(sd_ni_growth)))^4 / 1000 * 20 / 1.05, 
    bubble_two = if_else(current_price > two_sig_expected_price, 1, 0), 
    bubble_three = if_else(current_price > three_sig_expected_price, 1, 0)
  ) %>% 
  group_by(run_number) %>% 
  arrange(step) %>% 
  mutate(
    two_bubble_start = if_else(
      bubble_two == 1 & lag(bubble_two) == 0 & !is.na(lag(bubble_two)), 1, 0
      ), 
    three_bubble_start = if_else(
      bubble_three == 1 & lag(bubble_three) == 0 & !is.na(lag(bubble_three)), 1, 0
    ), 
    sd_desc = case_when(
      sd_ni_growth > .12 ~ "High SD", 
      sd_ni_growth < .03 ~ "Low SD", 
      T ~ "Average SD"
    ), 
    sd_desc = factor(sd_desc, levels = c("High SD", "Average SD", "Low SD")),
    mean_desc = case_when(
      mean_ni_growth > .03 ~ "High Mean" ,
      mean_ni_growth < .01 ~ "Low Mean", 
      T ~ "Average Mean"
    ), 
    mean_desc = factor(mean_desc, levels = c("High Mean", "Average Mean", "Low Mean"))
  ) %>% 
  ungroup()



sim_results_with_bubs %>% 
  mutate(
    num_rational = factor(num_rational, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc, run_number) %>% 
  summarize(
    number_days_bubble = sum(bubble_two)
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc) %>% 
  summarize(mean_days_bubble = mean(number_days_bubble)) %>% 
  ggplot(aes(num_rational, mean_days_bubble))+ 
  geom_col()+
  theme_bw()+
  facet_grid(mean_desc ~ sd_desc, scales = "fixed")+
  ggsave(file.path("R_output", "two_bubble_days.png"), width = 7, height = 3.2)





sim_results_with_bubs %>% 
  mutate(
    num_rational = factor(num_rational, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc, run_number) %>% 
  summarize(
    number_days_bubble = sum(bubble_three)
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc) %>% 
  summarize(mean_days_bubble = mean(number_days_bubble)) %>% 
  ggplot(aes(num_rational, mean_days_bubble))+ 
  geom_col(fill = "lightblue")+
  ylab("Days")+
  xlab("Number of Rational Agents")+
  theme_bw()+
  facet_grid(mean_desc ~ sd_desc, scales = "fixed")+
  ggtitle("Figure 7: Average Number of Days in Bubble Territory")+
  ggsave(file.path("R_output", "three_bubble_days.png"), width = 7, height = 4)




sim_results_with_bubs %>% 
  mutate(
    num_rational = factor(num_rational, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc, run_number) %>% 
  summarize(
    number_of_bubbles = sum(two_bubble_start)
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc) %>% 
  summarize(mean_bubbles = mean(number_of_bubbles)) %>% 
  ggplot(aes(num_rational, mean_bubbles))+ 
  geom_col()+
  theme_bw()+
  facet_grid(mean_desc ~ sd_desc, scales = "fixed")+
  ggsave(file.path("R_output", "two_bubble_count.png"), width = 7, height = 4)




sim_results_with_bubs %>% 
  mutate(
    num_rational = factor(num_rational, levels = c("10", "50", "90"))
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc, run_number) %>% 
  summarize(
    number_of_bubbles = sum(three_bubble_start)
  ) %>% 
  group_by(num_rational, mean_desc, sd_desc) %>% 
  summarize(mean_bubbles = mean(number_of_bubbles)) %>% 
  ggplot(aes(num_rational, mean_bubbles))+ 
  geom_col(fill = "lightblue")+
  ylab("Number of Bubbles")+
  xlab("Number of Rational Agents")+
  theme_bw()+
  facet_grid(mean_desc ~ sd_desc, scales = "fixed")+
  ggtitle("Figure 6: Average Number of Bubbles")+
  ggsave(file.path("R_output", "three_bubble_count.png"), width = 7, height = 4)









sim_results_with_bubs %>% 
  filter(
  run_number == 231
  ) %>% 
  ggplot(aes(x = step, y = current_price))+
  geom_line(aes(group = 1, color = "Current Market Price"))+
  geom_line(aes(step, three_sig_expected_price))+
  geom_line(aes(step, three_sig_low_price))+
  geom_ribbon(aes(
    x = step, ymin = three_sig_low_price, 
    ymax = three_sig_expected_price,
    fill = "Fundamental Price Range"
  ), alpha = .25)+
  scale_fill_manual(
    values = c("Fundamental Price Range" = "blue"), 
    name = ""
  )+
  scale_color_manual(
    values = c("Current Market Price" = "forestgreen"), 
    name = ""
  )+
  scale_y_continuous(name = "Price", labels = dollar)+
  theme_bw()+
  theme(legend.position = "top")+
  ggtitle("Figure 5: Simulation Price Example")+
  ggsave(file.path("R_output", "sim_price_example.png"), width = 7, height = 3.2)
  







sim_results_with_bubs %>% 
  filter(
    run_number == 233
  ) %>% 
  ggplot(aes(x = step, y = current_price))+
  geom_line(aes(group = 1))+
  geom_line(aes(step, two_sig_expected_price))+
  geom_line(aes(step, two_sig_low_price))




sim_results_with_bubs %>% 
  filter(
    num_rational == 50 & 
      sd_desc == "High SD" & 
      mean_desc == "High Mean"
  ) %>% 
  group_by(run_number) %>% 
  summarize(
    three_bubbles = sum(three_bubble_start), 
    two_bubbles = sum(two_bubble_start), 
    three_bubble_days = sum(bubble_three), 
    two_bubble_days = sum(bubble_two)
  )












