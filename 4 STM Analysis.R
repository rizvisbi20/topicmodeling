
##################################################################
##### Analysis with unsupervised techniques, EU speeches  ########
##################################################################

# OVERVIEW, what we do here:
# I. Load data
# II. After preparing the meta data and doing some robustness tests of cleaning procedures in other script ("Analysis_stm_preprobust") 
# we clean here the data and test a selection of models with different Ks before we determine the most suitable K for topic interpretation

# clean environment
rm(list = ls())

library(dplyr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(stringr)
library(Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(stmCorrViz)
library(beepr)
library(readtext)


# Set working directory 

# load the sorted supercorpus (as per dictionary results, prepared in precedent script)
#load("Our Data/final_dataframe.RData")
load("Our Data/data_final_merge_unprocessed.RData")
load("Our data/metacorpus.RData")
load("Our Data/meta_stm.RData")
# load("Our Data/toks.RData")

# Create Corpus, Tokens etc
# Do standard cleaning procedures: punctuation, numbers, lowercase, stopwords, steming
data_final_Corp <- corpus(data_final_merge_unprocessed)
toks <- tokens(data_final_Corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
toks <- tokens_tolower(toks)
toks <- tokens_select(toks,min_nchar=4L, selection = "remove")

## NEED TO SET UP SOME COMPOUND TOKENS
# identify tokens for removal but also analysis
toks <- tokens_compound(toks, pattern = phrase(c("credit unions", "credit union", "co-operative", "co-operatives",
     "capital market","capital markets", "return on assets","return on equity",
     "return on investment","trading business", "value proposition",
     "core market","corporate social responsibility","best interest",
     "give back","people before profit","profit sharing", "vice president",
     "chief executive officer", "first Ontario", "first west", 
     "interior savings", "northern credit union", "coast capital",
     "long term", "short term", "per cent", "british columbia",
     "royal bank","synergy credit union", "gulph and fraser",
     "crosstown civic", "salman arm", "caisses populaires",
     "caisses populaire", "uni financial", "your neighbourhood"))) 


#########################################

###### I. Preprocessing, Cleaning ######

#########################################


# for unsupervised techniques such as LDA and STM
# preprocessing is crucial and depending on the measurements chosen, can influence results significantly
# this is why we applied Denny and Spirling's (2018) method to evaluate our preprocessing in the precedent script before we do the cleainng here

########################################
########################################

# turn into dfm, more preprocessing (stopwords)
alldfm <- dfm(toks, remove = stopwords("english"))

# trim dfm
# Common practise to get rid of terms which are disturbing/not highly frequent in corpus
# We make this rather strict to get better model outcome (less rubbish in the topics)
# Keep only words occurring >= 20 times and in >= 3 documents
# try with 20 / 4
alldfm <- dfm_trim(alldfm, min_termfreq = 20, min_docfreq = 4)

# IMPORTANT: 
# Remove  words liable to confuse interpretation of machine learning 
# Compile a csv file with the 3000 most frequent words in one column 
# and manually mark in second column whether this is a irrelevant term which needs to be removed with (1) 
# For example entity names, names of executives, foreign words and letters, and other nonsense

# Careful, DO NOT USE THIS CODE, it only served the purpose to compile the list,
# using it again will overwrite the manual coding!!!
# store most frequent 1000 features as dataframe
#x <- topfeatures(alldfm, 3000) 
#y <- names(x) 
#remove_list <- as.data.frame(y) 
#x

# DON'T DO NEXT STEP -- it'll write over file
# write.csv(remove_list, file = "remove_list2.csv", row.names = FALSE)


# If longer list, use this...removes words coded as 1
ignorewords <- read.csv("remove_list2.csv", header=FALSE, sep=",", stringsAsFactors = FALSE)
ignorewords$V2[is.na(ignorewords$V2)] <- 0
ignorewords <- ignorewords[-which(ignorewords[,2]!=1),-2] 
ignorewords

# If there are other "rubbish" words/letters we can remove these as well
# Maerz calls this 'cleaning' datafile, i.e., load("cleaning.RData")
# in which case : ignorewords <- c(ignorewords, cleaning)

# remove misleading terms from dfm
# stemming words has to happen after remove words else remove words won't
# get rid of what I want
alldfm <- dfm_remove(alldfm, ignorewords)
alldfm <- dfm_wordstem(alldfm)

# test by showing 100 most frequent terms in dfm
x <- topfeatures(alldfm, n = 100, decreasing = TRUE, scheme = c("count",    "docfreq"), groups = NULL)
x <- as.data.frame(x) 
x
############################################

#### II. Apply unsupervised techniques ####

############################################


############################################
##### STM: Structural Topic Model ##########
############################################

# what we do here:
# FIRST: get statistically optimal number of topics
# SECOND: do detailed robustness tests, check several models with alternative numbers of topics 
# (continue analysis and visualization of results of best stm model in next script)

#########################------------------###################################
#########################------------------###################################

# generally, the stm package allows two ways of entering meta data: topical prevalence and topical content:

# PREVALENCE: allow the observed metadata to affect the frequency with which a topic is discussed
# for us this means, using ~name of the entity  
# as prevalence covariate controls for the varying size of subcorpii
# (wordfrequency scores are normalized based on relative size of subcorpus)

# CONTENT: allow the observed metadata to affect how a particular topic is discussed 
# for us this means that we compare how co-ops  and IOF 'speakers' discuss a certain topic


#########################-----------###############################
#########################-----------###############################

# before we start, we convert quanteda's dfm to stm object
# this is crucial because otherwise the algorithm does this for each substep again and again # and that can take a lot of time
stm_obj <- convert(alldfm, to = c("stm"), docvars = NULL)

# NOTE: we don't need docvars here, because we work with the above compiled meta data frame
# export stm object for main stm script
save(stm_obj, file = "Our Data/stm_object.RData")

# now, put number of topics to 0 for statistically optimized number of topics, here without meta data:
stat_model <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 0, 
                prevalence = ~ID, data = meta, 
                verbose = TRUE, max.em.its = 100, init.type = "Spectral", gamma.prior='L1')
stat_model

beep(5)

# 48 topics with convergence. Still too many for topic interpretation...


#################################################################

# II. Compile a selection of models (most robust) for validation

#################################################################

# narrow range of topics so something between 3, 5, 7, 9, 11
# seems the best amount of topics for a valid/intuitively interpretable stm model

# compile all models here with same settings, only different Ks, and write script for each of them 
# compile nice LaTeX table which shows that the major difference between this narrowed down selection of models
# is that if K is increased, there are more subtopics, if K is decreased, these subtopics are summarized to one!

# settings for all models:
# prevalence = ~ entity (to control for varying size of subcorpii)
# init.type = "spectral" (as recommended by Roberts et al.)
# max.em.its = 150 (should be enough)
# content = not needed here

# 3 topics
stm_obj_3 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 3, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_3, file = "Our Data/stm_obj_3.RData")

# 5 topics
stm_obj_5 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 5, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_5, file = "Our Data/stm_obj_5.RData")


# 7 topics
stm_obj_7 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 7, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_7, file = "Our Data/stm_obj_7.RData")


# 9 topics
stm_obj_9 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 9, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_9, file = "Our Data/stm_obj_9.RData")


# 11 topics
stm_obj_11 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 11, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_11, file = "Our Data/stm_obj_11.RData")

# 12 topics
stm_obj_12 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 12, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_12, file = "Our Data/stm_obj_12.RData")

# 13 topics
stm_obj_13 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 13, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_13, file = "Our Data/stm_obj_13.RData")

# 14 topics
stm_obj_14 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 14, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_14, file = "Our Data/stm_obj_14.RData")

# 17 topics
stm_obj_17 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 17, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_17, file = "Our Data/stm_obj_17.RData")

# 20 topics

stm_obj_20 <- stm(stm_obj$documents, vocab = stm_obj$vocab, K= 20, 
                  prevalence = ~ID, data = meta, gamma.prior='L1',
                  verbose = TRUE, max.em.its = 150, init.type = "Spectral")
save(stm_obj_20, file = "Our Data/stm_obj_20.RData")

beep(5)


# continue with separate scripts to analyze each of these models in more detail (available upon request) and show most valid topic selection of stm_14
# continue with next script 
