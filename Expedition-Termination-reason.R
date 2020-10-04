#Tidy Tuesday week 39 Himalayan expeditions
#Investigating the Reasons for terminations of different expeditions
#load packages
library(tidyverse)
#load data
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
view(expeditions)

#Plot
expeditions %>% group_by(termination_reason) %>% count() %>% 
  arrange(desc(n)) %>% filter(n >=126) %>% 
  ggplot(aes(n, fct_reorder(termination_reason, n), fill = termination_reason)) + geom_col() +
  geom_text(aes(x=n, hjust =1.2, label = n))+
  labs(title = "Top Termination Reasons For Different Expeditions",
       y = "Termantion reason", x = "Count-Frequency")+
  theme(legend.position = "none",
        plot.title = element_text(hjust =.5)) +
  ggsave("Termination.png")
