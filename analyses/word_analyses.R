#MORE ANALYSIS

DATA_WORDS <- guidance_overall_swomitted %>%
  filter(
    word %in% c(
      "analysis",
      "data",
      "fes",
      "proportion",
      "proportionate",
      "proportions",
      "statistics",
      "target",
      "targets")) %>% 
  select(word, year) %>% 
  mutate(words = ifelse(word %in% c("proportion", "proportionate", "proportions"), "proportion etc",
                        ifelse(word %in% c("target", "targets"), "target etc",
                               word))) %>% 
  group_by(words, year) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=year, y=Total, fill = Total)) +
  geom_col() +
  facet_wrap(~words) +
  theme_minimal() +
  theme(panel.border = element_rect("grey", fill = NA),
        strip.background = element_rect("grey", fill = NA),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(aes(label = Total, vjust = -.1)) +
  labs(x="Academic year", y="Occurrences", title = "Terms concerning data in college OA guidance, 2014-15 to 2020-21*", caption = "*Contains some word amalgamation, e.g. 'proportion', 'proportions', 'proportionate', etc")

###############################################################################################


ACCESS_WORDS <- guidance_overall_swomitted %>%
  filter(
    word %in% c("access",
                "cowa",
                "deprivation",
                "deprived",
                "fair",
                "fairer",
                "fairness",
                "inequalities",
                "inequality",
                "simd")) %>% 
    select(word, year) %>% 
    mutate(words = ifelse(word %in% c("deprivation", "deprived"), "deprivation",
                          ifelse(word %in% c("fair", "fairer", "fairness"), "fair etc",
                                 ifelse(word %in% c("inequality", "inequalities"), "inequality",
                                 word)))) %>% 
  group_by(words, year) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=year, y=Total, fill = Total)) +
  geom_col() +
  facet_wrap(~words) +
  theme_minimal() +
  theme(panel.border = element_rect("grey", fill = NA),
        strip.background = element_rect("grey", fill = NA),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(aes(label = Total, vjust = -.1)) +
  labs(x="Academic year", y="Occurrences", title = "Terms concerning access in college OA guidance, 2014-15 to 2020-21*", caption = "*Contains some word amalgamation, e.g. 'fair', 'fairer', 'fairness', etc")

###############################################################################################

PROCESS_WORDS <- guidance_overall_swomitted %>%
  filter(
    word %in% c(
      "collaborate",
      "collaborated",
      "collaborating",
      "collaboration",
      "collaborations",
      "collaborative",
      "collaboratively",
      "excellence",
      "excellent",
      "governance",
      "governing",
      "mainstream",
      "mainstreamed",
      "mainstreaming",
      "priorities",
      "priority",
      "prioritise",
      "prioritising",
      "prioritises",
      "streamlined",
      "intensification",
      "intensified",
      "negotiate",
      "negotiated",
      "negotiating",
      "negotiation",
      "negotiations",
      "recruit",
      "recruiting")) %>%
  select(word, year) %>% 
  mutate(words = ifelse(word %in% c("collaborate", "collaborated", "collaborating", "collaboration","collaborations", "collaborative","collaboratively"), "collaboration etc",
                        ifelse(word %in% c("excellence", "excellent"), "excellence",
                               ifelse(word %in% c("governance", "governing"), "governance",
                                      ifelse(word %in% c("mainstream", "mainstreamed", "mainstreaming"), "mainstreaming etc",
                                             ifelse(word %in% c("priorities", "priority", "prioritise", "prioritising","prioritises"), "priorities etc",
                                                    ifelse(word %in% c("intensification", "intensified"), "intensification",
                                                           ifelse(word %in% c("negotiate", "negotiated", "negotiating", "negotiation", "negotiations"), "negotiate etc",
                                                                  ifelse(word %in% c("recruit", "recruiting"), "recruitment",
                                                                         "streamlined"))))))))) %>% 
  group_by(words, year) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=year, y=Total, fill = Total)) +
  geom_col() +
  facet_wrap(~words) +
  theme_minimal() +
  theme(panel.border = element_rect("grey", fill = NA),
        strip.background = element_rect("grey", fill = NA),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(aes(label = Total, vjust = -.1)) +
  labs(x="Academic year", y="Occurrences", title = "Terms concerning process in college OA guidance, 2014-15 to 2020-21*", caption = "*Contains some word amalgamation")

###############################################################################################


PRIORITY_WORDS <- guidance_overall_swomitted %>%
  filter(
    word %in% c(
      "articulate",
      "articulated",
      "articulating",
      "articulation",
      "cld",
      "leaver",
      "leaver's",
      "leavers",
      "destination",
      "destinations",
      "care",
      "carer",
      "carer's",
      "carers",
      "cares",
      "disabilities",
      "disability",
      "disabled",
      "accessibility",
      "diversity",
      "ethnic",
      "ethnicity",
      "gaelic")) %>% 
  select(word, year) %>%
  mutate(words = ifelse(word %in% c("articulate","articulated","articulating","articulation"), "articulation etc",
                        ifelse(word %in% c("cld", "leaver", "leaver's", "leavers", "destination", "destinations"), "destinations",
                               ifelse(word %in% c("care", "carer", "carer's", "cares", "carers"), "care etc",
                                      ifelse(word %in% c("disabilities", "disability", "disabled", "accessibility"), "disability etc",
                                             ifelse(word %in% c("diversity", "ethnic", "ethnicity"), "ethnicity/diversity",
                                                    "gaelic")))))) %>% 
  group_by(words, year) %>% 
  summarise(Total = n()) %>% 
  ggplot(aes(x=year, y=Total, fill = Total)) +
  geom_col() +
  facet_wrap(~words) +
  theme_minimal() +
  theme(panel.border = element_rect("grey", fill = NA),
        strip.background = element_rect("grey", fill = NA),
        axis.text.x = element_text(angle = 45, hjust=1)) +
  geom_text(aes(label = Total, vjust = -.1)) +
  labs(x="Academic year", y="Occurrences", title = "Terms concerning OA priorities in college OA guidance, 2014-15 to 2020-21*", caption = "*Contains some word amalgamation")
  
