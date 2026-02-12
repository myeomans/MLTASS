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

revs<-read.csv("data/week1_answers.csv")

#source("geminiAPI.R")
gemini.R::setAPI(gKey)

revs$stars_guess<-NA
revs$price_guess<-NA
revs$gender_guess<-NA


stars_task<-"I am giving you a restaurant review. 
  I want you to read it and guess if the writer liked or hated the restaurant.. 
If they liked it, respond with a '4'. If they hated it, respond with a '2'.
Only respond with the number 4 or 2, don't write anything else. 

Here is the review:  "


price_task<-"I am giving you a restaurant review. 
  I want you to read it and guess if the restaurant is cheap or expensive. 
If it is cheap, respond with a '1'. If it is expensive, respond with a '2'.
Only respond with the number 1 or 2, don't write anything else. 
Here is the review:  "

gender_task<-"I am giving you a restaurant review. 
  I want you to read it and guess if the writer is male or female. 
Answer only with the word 'male' or 'female' and nothing else. 
Here is the review:  "

tpb<-txtProgressBar(1,nrow(revs))
for(x in 1:nrow(revs)){
  s_prompt=paste(stars_task,revs[x,]$text)
  xx<-gemini_chat(s_prompt,
                  model = "2.0-flash-lite")
  revs[x,]$stars_guess<-xx$outputs
  p_prompt=paste(price_task,revs[x,]$text)
  xx<-gemini_chat(p_prompt,
                  model = "2.0-flash-lite")
  revs[x,]$price_guess<-xx$outputs

  g_prompt=paste(gender_task,revs[x,]$text)
  xx<-gemini_chat(g_prompt,
                model = "2.0-flash-lite")
  revs[x,]$gender_guess<-xx$outputs
   setTxtProgressBar(tpb,x)
}

revs$stars_guess<-gsub("\n","",revs$stars_guess)
revs$price_guess<-gsub("\n","",revs$price_guess)
revs$gender_guess<-gsub("\n","",revs$gender_guess)

revs$gender_guess<-1*(revs$gender_guess=="male")

revs %>%
  with(table(gender,gender_guess))

revs %>%
  with(table(price,price_guess))

revs %>%
  with(table(stars,stars_guess))
