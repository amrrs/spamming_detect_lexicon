library(tidyverse)

pred_lex <- read_csv("predatory_lexicon_updated.csv")


#input_text <- read_file('spam (1).txt')

input_text <- "acceptance is very critical"

input_text <- tolower(input_text)

pred_lex$match <- tolower(input_text) %>%
   str_detect(pred_lex$word)

  
ifelse(sum(pred_lex$match)>0,
  pred_lex %>%
  filter(match) %>%
  summarise(value),
  0)

pred_lex %>% filter(match)


