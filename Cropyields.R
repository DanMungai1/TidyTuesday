#Load Packages
library(tidyverse)
library(ggthemes)

##Loading Data
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)
key_crop_yields <- tuesdata$key_crop_yields
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application

Kenya <- key_crop_yields %>%
  pivot_longer(cols = 4:14, names_to = "produce", values_to = "production") %>% 
  filter(Entity == "Kenya",
         !produce %in% c("Soybeans (tonnes per hectare)",
                         "Cocoa beans (tonnes per hectare)",
                         "Peas (tonnes per hectare)"),
         !Year <= 1999)

Kenya1 <- key_crop_yields %>%
  janitor::clean_names() %>%
  rename_with(~ str_remove(., "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = 4:14, names_to = "produce", values_to = "production") %>% 
    filter(entity == "Kenya",
         produce %in% c("bananas", "potatoes"),
         !year <= 1999)

KenyaUganda <- key_crop_yields %>%
  janitor::clean_names() %>%
  rename_with(~ str_remove(., "_tonnes_per_hectare")) %>% 
  pivot_longer(cols = 4:14, names_to = "produce", values_to = "production") %>% 
  filter(entity %in% c("Kenya", "Uganda"),
         produce %in% c("bananas", "potatoes"),
         !year <= 1999)

ggplot(filter(KenyaUganda, produce == "bananas"), aes(factor(year), production, fill = entity)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Banana Production in Kenya and Uganda",
       x = "Year",
       y= "Production Per Hectare") + theme_fivethirtyeight() +
  ggsave("KenyaUgandabanana.png")

ggplot(filter(KenyaUganda, produce == "potatoes"), aes(factor(year), production, fill = entity)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Potatoes Production in Kenya and Uganda",
       x = "Year",
       y= "Production Per Hectare") + theme_fivethirtyeight() +
  ggsave("KenyaUgandapotatoes.png")




ggplot(filter(Kenya1, produce == "bananas"), aes(factor(year), production)) + geom_point()+
  geom_line(group = 2)+
  labs(title = "Banana Production in Kenya",
       x = "Year",
       y= "Production Per Hectare") + theme_fivethirtyeight() +
  ggsave("Banana.png")

ggplot(filter(Kenya1, produce == "potatoes"), aes(factor(year), production)) + geom_point()+
  geom_line(group = 2)+
  labs(title = "Potatoes Production in Kenya",
       x = "Year",
       y= "Production Per Hectare") + theme_fivethirtyeight()+
  ggsave("potatoes.png")

ggplot(Kenya, aes(factor(Year), production, fill = produce)) +
geom_bar(stat = "identity") + coord_flip() +
facet_wrap(~produce, scales = "free") +
labs(title = "Crop production in Kenya",
       subtitle = "Crop Production over the last 20 years",
     caption = "courtesy of Dan Mungai")+
guides(fill = "none") +
theme_fivethirtyeight() +
theme(plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)) +
  ggsave("Different-crops.png")

ggplot(Kenya, aes(factor(Year), production, fill = produce)) +
  geom_bar(stat = "identity") + coord_flip() +
   labs(title = "Crop production in Kenya",
       subtitle = "Crop Production over the last 20 years")+
  guides(fill = "none") +
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggsave("General-trend.png")

###Fertilizer
KenyaFert <- fertilizer %>% janitor::clean_names() %>% 
  filter(entity == "Kenya",
         year >= "2000") %>% 
  rename(fertilizer = nitrogen_fertilizer_use_kilograms_per_hectare,
         cereal_yield = cereal_yield_tonnes_per_hectare) %>% 
  mutate(fertilizer = fertilizer/1000,
         ratio = fertilizer/cereal_yield) %>% 
  ggplot(aes(factor(year), ratio)) + geom_bar(stat = "identity") +
  labs(title = "The fertilizer to cereal yield Ratio",
       x = "year", y= "fertilizer:cereal yield ratio")+
  theme_fivethirtyeight() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) + 
  ggsave("fertilizercereal-yield-ratio.png")
