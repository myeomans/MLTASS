################################################
#
# Machine Learning & Text Analysis for Social Science
#
#              Assignment 6
#
#
################################################

library(quanteda)
library(ggrepel)
library(textclean)
library(tidyverse)
library(glmnet)
library(gemini.R)

source("MLTASS_dfm.R")
source("kendall_acc.R")


# filter to only look at calls from fiscal quarter 3, just to start
ecMain<-readRDS("data/earningsDat_3Y.RDS") %>%
  filter(FQ==3)

# filter the Q&A to match the filtering on the main dataset
ecQA<-readRDS("data/earningsQandA_3Y.RDS")%>%
  mutate(wordcount=str_count(text,"[[:alpha:]]+")) %>%
  filter(callID%in%ecMain$callID)

ecQA %>%
  filter(callID=="100521") %>%
  as_tibble() %>%
  head(30) 

# how many questions per call are there?
ecQA %>%
  filter(asker==1) %>%
  group_by(callID) %>%
  summarize(qcount=n()) %>%
  ggplot(aes(x=qcount)) +
  geom_histogram()

# how many questions per questioner are there?
ecQA %>%
  filter(asker==1) %>%
  group_by(callID,askerID) %>%
  summarize(qcount=n()) %>%
  ggplot(aes(x=qcount)) +
  geom_histogram()


# how many questioners per call are there?
ecQA %>%
  filter(asker==1) %>%
  group_by(callID) %>%
  summarize(qcount=n_distinct(askerID)) %>%
  ggplot(aes(x=qcount)) +
  geom_histogram()


# This variable indicates the order of askers in each call
ecQA %>%
  filter(asker==1 & followup == 0) %>%
  with(table(as.numeric(askerID)))

# question has the order of questions
ecQA %>%
  with(table(question,asker))

####################################################################################
# calculate a turn-level feature and add it to the conversation-level data
####################################################################################

# Average word count of first questions in the Q&A data and join to the main data
ecMain_merged <- ecMain %>%
  left_join(ecQA %>%
              filter(asker==1 & question==1) %>%
              group_by(callID) %>%
              summarize_at(vars(wordcount),sum) %>%
            rename(wordcount_q1="wordcount"))

# compare average answer word count to earnings per share
kendall_acc(ecMain_merged$wordcount_q1,
            ecMain_merged$EPS_actual)

# Average word count of first answers in the Q&A data and join to the main data
ecMain_merged <- ecMain %>%
  left_join(ecQA %>%
              filter(asker==0 & question==1) %>%
              group_by(callID) %>%
              summarize_at(vars(wordcount),sum) %>%
              rename(wordcount_a1="wordcount")
            ) %>%
  mutate(wordcount_a1=replace_na(wordcount_a1,0))

# compare average answer word count to earnings per share
kendall_acc(ecMain_merged$wordcount_a1,
            ecMain_merged$EPS_actual)


############################################################
# Create a question-answer pair dataset
############################################################

QApairs=ecQA %>%
  filter(callID=="100002") %>%
  mutate(qnum=question,
         asker=ifelse(asker==1,"question","answer")) %>%
  group_by(callID,qnum,asker) %>%
  summarize(text=paste(text,collapse=" ")) %>%
  pivot_wider(names_from="asker",values_from="text")

head(QApairs)

############################################################
# Connect to the LLM API!
############################################################




gemini.R::setAPI("AIzaSyA1kcHjek4VXVDJQy_SSE2NZQkG-6rahOc")

xx<-gemini_chat("who is michael yeomans?",
                model = "2.0-flash-lite")

xx$outputs

