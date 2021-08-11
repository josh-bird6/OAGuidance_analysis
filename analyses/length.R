library(tidytext)
library(tidyverse)


##function for removing URLs
url_regex <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
sw <- get_stopwords()

sentiments_affin <- get_sentiments()
sentiments_bing <- get_sentiments("bing")
sentiments_nrc <- get_sentiments("nrc")
sentiments_loughran <- get_sentiments("loughran")

#READING DATASETS###############

guidance_1415 <- read.delim("OA1415.txt")
guidance_1516 <- read.delim("OA1516.txt")
guidance_1617 <- read.delim("OA1617.txt")
guidance_1718 <- read.delim("OA1718.txt")
guidance_1819 <- read.delim("OA1819.txt")
guidance_1920 <- read.delim("OA1920.txt")
guidance_2021 <- read.delim("OA2021.txt")

OIF_guidance <- read.delim("OIF_Guidance.txt")


###########PREPARING FOR ANALYSIS

###############
###############
####2014-15####
###############
###############

guidance_1415_analysis <- guidance_1415 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2014-15") 

#REMOVINGSTOPWORDS
guidance_FINAL_1415 <- guidance_1415_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2015-16####
###############
###############

guidance_1516_analysis <- guidance_1516 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2015-16") 

#REMOVINGSTOPWORDS
guidance_FINAL_1516 <- guidance_1516_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2016-17####
###############
###############

guidance_1617_analysis <- guidance_1617 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2016-17") 

#REMOVINGSTOPWORDS
guidance_FINAL_1617 <- guidance_1617_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2017-18####
###############
###############

guidance_1718_analysis <- guidance_1718 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2017-18") 

#REMOVINGSTOPWORDS
guidance_FINAL_1718 <- guidance_1718_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2018-19####
###############
###############

guidance_1819_analysis <- guidance_1819 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2018-19") 

#REMOVINGSTOPWORDS
guidance_FINAL_1819 <- guidance_1819_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2019-20####
###############
###############

guidance_1920_analysis <- guidance_1920 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2019-20") 

#REMOVINGSTOPWORDS
guidance_FINAL_1920 <- guidance_1920_analysis %>% 
  anti_join(sw, by ="word") 

###############
###############
####2020-21####
###############
###############

guidance_2021_analysis <- guidance_2021 %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "2020-21") 

#REMOVINGSTOPWORDS
guidance_FINAL_2021 <- guidance_2021_analysis %>% 
  anti_join(sw, by ="word") 

################
################
####OIF GUIDANCE
################
################

OIF_guidance_analysis <- OIF_guidance %>% 
  mutate(words_character = as.character(words),
         numberless = gsub('[0-9]+', '', words_character),
         individual = str_remove_all(numberless, url_regex)) %>% 
  select(individual) %>% 
  unnest_tokens(words_character_final, individual, token = "words") %>% 
  filter(!grepl("sfc.ac.uk", words_character_final)) %>%
  mutate(word = words_character_final,
         year = "OIF Current") 

#REMOVINGSTOPWORDS
OIF_guidance_FINAL <- OIF_guidance_analysis %>% 
  anti_join(sw, by ="word") 





#################
#Merging datasets
#################
guidance_overall <- rbind(guidance_1415_analysis, 
                          guidance_1516_analysis,
                          guidance_1617_analysis,
                          guidance_1718_analysis,
                          guidance_1819_analysis,
                          guidance_1920_analysis,
                          guidance_2021_analysis,
                          OIF_guidance_analysis) %>% 
  mutate(classification = "OVERALL")

guidance_overall_swomitted <- rbind(guidance_FINAL_1415,
                                    guidance_FINAL_1516,
                                    guidance_FINAL_1617,
                                    guidance_FINAL_1718,
                                    guidance_FINAL_1819,
                                    guidance_FINAL_1920,
                                    guidance_FINAL_2021,
                                    OIF_guidance_FINAL) %>% 
  mutate(classification = "NO STOP WORDS*")



guidance_OVERALL_FINAL <- rbind(guidance_overall,
                                guidance_overall_swomitted)


guidance_OVERALL_FINAL %>% 
  group_by(year, classification) %>% 
  summarise(total = n()) %>% 
  ggplot(aes(x=year, y=total, fill = classification)) +
  geom_col(position = "dodge") +
  labs(x="Academic Year for which guidance was issued", y="Word count", title = "Length of college OA guidance, 2014-15 to 2020-21", caption = "*omits prepositions, numbers, URLs, etc") +
  scale_y_continuous(label = scales::comma) +
  theme_minimal() +
  theme(legend.title= element_blank()) +
  geom_text(position = position_dodge(1), aes(label = scales::comma(total)), vjust=-.1) +
  scale_fill_manual(values = c("#7b3294", "#3182bd")) 
  


##wordcloud
library(wordcloud)

wordcloud(words = test$words_character_final, freq = test$n, min.freq = 1,
          max.words = 150, random.order=F, rot.per = .35, scale=c(3.5,0.25),
          colors = brewer.pal(8, "BrBG"))



# test <- guidance_overall_swomitted %>%
#   count(words_character_final, sort = T)
