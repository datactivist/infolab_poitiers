## libs -------

library(tidyverse)
library(lubridate) # date

## data -----

setwd("~/Datactivist/GrandPoitiers/infolab_poitiers/hackaviz/bdd/")

mariages <- read_csv2("./data/citoyennete-mariages-a-poitiers.csv") %>% 
  set_names(c("annee","mois","contrat","valeur")) %>% 
  mutate(date = make_date(month = mois,year = annee),
         contrat = case_when(contrat == "AUTRES MENTIONS" ~ "autres",
                             contrat == "MENTIONS DE PACS" ~ "pacs",
                             contrat == "PUBLICATION POUR MARIAGES EXTERIEURS A LA COMMUNE" ~ "mariage (ext)",
                             contrat == "CONTRAT DE MARIAGE" ~ "mariage (n)",
                             contrat == "NOMBRE DE MARIAGES" ~ "mariage (c)",
                             contrat == "MENTIONS DE DIVORCE" ~ "divorce",
         TRUE ~ contrat))
                           
unique(mariages$contrat)  

head(mariages)

## continuité --
mariages %>% group_by(mois, annee, contrat) %>% summarise(n = n())
mariages %>% group_by(date,contrat) %>% summarise(n = n()) %>% filter(n != 1)

## evolution
mariages %>% ggplot(aes(x = mois, y = valeur, color = contrat)) +
  geom_line() +
  facet_wrap(~ annee) +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none")

# le poids des "autres mentions" est énorme : à expliciter
mariages %>% group_by(date) %>%
  mutate(part = valeur/sum(valeur)) %>% 
  ungroup() %>%
  ggplot(aes(x="",y=part,fill=contrat)) +
  geom_bar(stat="identity",width=1) +
  facet_wrap(~ annee) +
  coord_polar("y", start=0)

# celui des pacs aussi

mariages %>% filter(contrat %in% c("mariage (n)", "mariage (c)")) %>% 
  ggplot(aes(x = mois, y = valeur, color = contrat)) +
  geom_line() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank()) +
  facet_wrap(~ annee)
