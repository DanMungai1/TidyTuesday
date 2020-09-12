library(tidyverse)
library(friends)
library(tidytext)
library(ggthemes)
library(wordcloud2)
library(wesanderson)
library(webshot)
# webshot::install_phantomjs()
library("htmlwidgets")
friends <- friends

unnest_friends <- friends %>% 
  filter(speaker %in% c("Monica Geller", "Joey Tribbiani", "Chandler Bing",
"Phoebe Buffay", "Ross Geller")) %>% select(speaker, text) %>% 
  unnest_tokens(word, text) %>% group_by(speaker) %>% 
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>% top_n(10)

ggplot(unnest_friends, aes(x= n, y = reorder(word, n), fill = speaker)) + geom_col() +
  facet_wrap(~speaker, scales = "free") + theme_fivethirtyeight() +
  ggsave("friendsplot.png", dpi = 300)

wordscloud <- friends %>% 
  filter(speaker %in% c("Monica Geller", "Joey Tribbiani", "Chandler Bing",
                        "Phoebe Buffay", "Ross Geller")) %>% select(speaker, text) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE)

wordcloud <- wordcloud2(wordscloud, shape='circle', size=1.75, 
           color=rep(wes_palettes$Rushmore1[3:5], nrow(wordscloud)/length(wes_palettes$Rushmore1[3:5])))
wordcloud

saveWidget(wordcloud,"wordsplot.html",selfcontained = F)

# and in png
webshot("wordsplot.html","friends_word_plot.png", delay =5)
