# Tidy Tuesday 25 Aug 2020
# Chopped TV Show
library(ggplot2)
library(dplyr)
library(tidytext) #  for text mining
library(wordcloud2)
library(wesanderson)
library(webshot)
# webshot::install_phantomjs()
library("htmlwidgets")
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

# find each individual word used in entree and count how many time it was in the show
judges <- chopped %>% select(judge1, judge2, judge3) %>%
  unite(judges,judge1, judge2, judge3) %>% 
  unnest_tokens(word, judges) %>% count(word, sort=TRUE)

# remove any stop words using the stopwords dataset
cleaned_judges <- judges %>%
  anti_join(get_stopwords())

# Create wordcloud plot
judges_word_plot <- wordcloud2(cleaned_judges, shape='pentagon', size=0.75, 
                                  color=rep(wes_palettes$Rushmore1[3:5], nrow(cleaned_judges)/length(wes_palettes$Rushmore1[3:5])))

# save widget in html
saveWidget(judges_word_plot,"judges_word_plot.html",selfcontained = F)

# and in png
webshot("judges_word_plot.html","judges_word_plot.png", delay =5)
