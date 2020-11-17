library(tidyverse)
library(tidytext)
library(wordcloud2)
library(htmlwidgets)
library(wesanderson)
library(webshot)
black_in_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-17/black_in_data.csv')
tokens <- black_in_data %>% unnest_tokens(word, purpose) %>% 
  count(word, sort = T) %>% anti_join(stop_words)
word_plot <- wordcloud2(tokens, shape='circle', size=0.75, 
                        color=rep(wes_palettes$Rushmore1[3:5], nrow(tokens)/length(wes_palettes$Rushmore1[3:5])))

# save widget in html
saveWidget(word_plot,"word_plot.html",selfcontained = F)

# and in png
webshot("word_plot.html","blackindata.png", delay =5)
