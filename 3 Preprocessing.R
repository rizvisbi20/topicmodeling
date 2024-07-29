
##########################################################################
##### Prepare analysis with unsupervised techniques, EU speeches  ########
##########################################################################

# Apply Danny and Spirlings diagnostics to find out about effects of text cleaning
# devtools::install_github("matthewjdenny/preText")
# install.packages("preText")

# clean environment
rm(list = ls())

# load packages
library(dplyr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(quanteda.textmodels)
library(stm)
library(stringr)
library(Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(stmCorrViz)
library(preText)
library(readtext)
library(beepr)

load("Our Data/data_final_merge_unprocessed.RData")
load("Our Data/metacorpus.RData")

##########################################
# Preprocessing and cleaning Checks ######
##########################################

# apply Denny and Spirling's techniques (preText package) to make sure that our cleaning procedures do not have huge effects on results
# code adopted from: http://www.mjdenny.com/getting_started_with_preText.html

# basic idea: take a sample of around 500 texts from corpus and test all kind of preprocessing techniques
# compare the pairwise distance between documents in sample for each of the combinations of preprocessing techniques
# assess results: if there is huge difference, double check if this particular preprocessing feature stands on valid theoretical ground
# or should be spared due to its comparatively huge effect on the results

# export also the vector with all documents for main stm script
# findtexts <- as.character(final_dataframe) # this should be findtexts <- as.character(final_dataframe$text) 
findtexts <- as.character(data_final_merge_unprocessed$text)
save(findtexts, file = "Our Data/all_documents.RData")

# check if all CUs are included in metacorpus and save as unique data file
name <- docvars(metacorpus, "Name")
name_unique <- unique(name)
name_unique
save(name_unique, file = "Our Data/name_unique.RData")

# first, take sample from speech corpus 
# can skip this for text run
sampling <- as.character(final_dataframe) # 'texts' is deprecated
# sample without replacement
sample_statements <- sample(sampling, replace=FALSE) # Initially set at 500; i use all

# we can now make use of the factorial_preprocessing() function, 
# which will preprocess the data 64 or 128 different ways (depending on whether n-grams are included).
preprocessed_documents <- factorial_preprocessing(
  findtexts,
  use_ngrams = FALSE,
  infrequent_term_threshold = 0.2,
  verbose = FALSE)
beep(5)

# this function will output a list object with three fields. 
# The first of these is $choices, a data.frame containing indicators for each of the preprocessing steps used. 
# The second is $dfm_list, which is a list with 64 or 128 entries, 
# each of which contains a quanteda::dfm object preprocessed according to the specification in the corresponding row in choices. 
# Each DFM in this list will be labeled to match the row names in choices, but you can also access these labels from the $labels field. We can look at the first few rows of choices below:
names(preprocessed_documents)

head(preprocessed_documents$choices)

# Now that we have our preprocessed documents, we can perform the preText procedure on the factorial preprocessed corpus using the preText() function. 
preText_results <- preText(
  preprocessed_documents,
  dataset_name = "Annual Report Messages",
  distance_method = "cosine",
  num_comparisons = 50,
  verbose = TRUE)
beep(5)

#The preText() function returns a list of result with four fields:
# preText_scores: A data.frame containing preText scores and preprocessing step labels for each preprocessing step as columns. Note that there is no preText score for the case of no prepprocessing steps.
# ranked_preText_scores: A data.frame that is identical to $preText_scores except that it is ordered by the magnitude of the preText score
# choices: A data.frame containing binary indicators of which preprocessing steps were applied to factorial preprocessed DFM.
# regression_results: A data.frame containing regression results where indicators for each preprocessing decision are regressed on the preText score for that specification.

#We can now feed these results to two functions that will help us make better sense of them.
# preText_score_plot() creates a dot plot of scores for each preprocessing specification:
preText_score_plot(preText_results)

# Here, the least risky specifications have the lowest preText score and are displayed at the top of the plot. 
# We can also see the conditional effects of each preprocessing step on the mean preText score for each specification that included that step. 
# Here again, a negative coefficient indicates that a step tends to reduce the unusualness of the results, 
# while a positive coefficient indicates that applying the step is likely to produce more unusual results for that corpus.
# P = punctuation; N = removing numbers; L = lowercase; S = Stemming; W = remove stopwords; I = infrequently used terms 

# here, we can get an even better overview on those techniques which might influences the results the most
regression_coefficient_plot(preText_results,
                            remove_intercept = TRUE)

# conclusions: our combination of preprocessing features is close to the mean of scores in the plot (even below), thus comparatively okay
# stopword removal is expected to have high effects on the results - yet, we still do this but make the list of stopwords transparent (see above)
# to show that this includes only word fragments or irrelevant terms

# overall, our preprocessing seems fine.

# report this in Appendix, refer to it in foodnote which explains cleaning procedures

############################################

#### IV. Apply unsupervised techniques ####

############################################

# see next script!
