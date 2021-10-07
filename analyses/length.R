library(tidytext)
library(tidyverse)


##function for removing URLs
url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
sw <- get_stopwords()

sentiments_affin <- get_sentiments()
sentiments_bing <- get_sentiments("bing")
sentiments_nrc <- get_sentiments("nrc")
sentiments_loughran <- get_sentiments("loughran")

##################
#Reading datasets#
##################
guidance_1415 <- read.delim("OA1415.txt") %>% 
  mutate(year = "2014-15") 

guidance_1516 <- read.delim("OA1516.txt") %>% 
  mutate(year="2015-16")

guidance_1617 <- read.delim("OA1617.txt") %>% 
  mutate(year="2016-17")

guidance_1718 <- read.delim("OA1718.txt") %>% 
  mutate(year="2017-18") 
  
guidance_1819 <- read.delim("OA1819.txt") %>% 
  mutate(year="2018-19")
  
guidance_1920 <- read.delim("OA1920.txt") %>% 
  mutate(year = "2019-20") 

guidance_2021 <- read.delim("OA2021.txt") %>% 
  mutate(year="2020-21")

OIF_guidance <- read.delim("OIF_Guidance.txt") %>% 
  mutate(year="OIF Current")

##################
#Merging datasets#
##################
guidance_overall <- rbind(guidance_1415, 
                          guidance_1516,
                          guidance_1617,
                          guidance_1718,
                          guidance_1819,
                          guidance_1920,
                          guidance_2021,
                          OIF_guidance)


##################
#TIDYING FUNCTION#
##################
tidying <- function(.df){
  .df %>% 
    mutate(words_character = as.character(words),
           numberless = gsub('[0-9]+', '', words_character),
           individual = str_remove_all(numberless, url_regex)) %>% 
    select(individual, year) %>% 
    unnest_tokens(words_character_final, individual, token = "words") %>% 
    filter(!grepl("sfc.ac.uk", words_character_final)) %>%
    mutate(word = words_character_final)
}

################
################
#####Tidying####
################
################

#Two datasets - one including and one without stopwords
guidance_Stopwordsinc <- tidying(guidance_overall) %>% 
  mutate(classification = "OVERALL")

guidance_noStopwords <- tidying(guidance_overall) %>% 
  anti_join(sw, by="word") %>% 
  mutate(classification = "NO STOP WORDS*")

#merging
guidance_OVERALL_FINAL <- rbind(guidance_Stopwordsinc,
                                guidance_noStopwords)

################
################
#####VIZ########
################
################

guidance_OVERALL_FINAL %>% 
  group_by(year, classification) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=year, y=total, fill = classification)) +
  geom_col(position = "dodge") +
  labs(x="Academic Year for which guidance was issued", 
       y="Word count", 
       title = "Length of college OA guidance, 2014-15 to 2020-21", 
       caption = "*omits prepositions, numbers, URLs, etc") +
  scale_y_continuous(label = scales::comma) +
  theme_minimal() +
  theme(legend.title= element_blank()) +
  geom_text(position = position_dodge(1), aes(label = scales::comma(total)), vjust=-.1) +
  scale_fill_manual(values = c("#7b3294", "#3182bd")) 


################
################
####Wordcloud###
################
################
library(wordcloud)

wordcloud <- guidance_noStopwords %>% 
  count(words_character_final, sort = T)

wordcloud(words = wordcloud$words_character_final, freq = wordcloud$n, min.freq = 1,
          max.words = 150, random.order=F, rot.per = .35, scale=c(3.5,0.25),
          colors = brewer.pal(8, "BrBG"))
