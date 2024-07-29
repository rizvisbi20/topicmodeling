# Script for CU terminology assessment
# With thanks to Seraphine F. Maerz for making her script and dataset available
# And thanks to the developers of quanteda and R
# clean environment
rm(list = ls())
options(max.print=5000) #default is 99999 so this imposes limit

#Get packages with mysterious"options" command
library(quanteda)
library(ggrepel)
library(stringr)
library(dplyr)
library(XML)
library(RCurl)
library(xml2)
library(ellipse)
library(extrafont)
library(plotrix)
library(ggplot2)
library(plyr)
library(tidyr)
library(stargazer)
library(xtable)
library(tibble)
library(data.table)
library(readtext)
require(quanteda.textstats)
require(quanteda.textplots)
require(devtools)
library(beepr)

# require(quanteda.corpora)
require(lubridate)
require(tidyverse)
require(stopwords)
options(width = 110) #sets width of window for viewing output
library(dplyr)

#########################################################
# Set directory
setwd("")
setwd("")
#load("Our Data/data_final_merge.RData")

load ("Our Data/data_final_merge_unprocessed.RData")
name <- data_final_merge_unprocessed$ID
entity <- data_final_merge_unprocessed$Entity
rank <- data_final_merge_unprocessed$Rank
data_final_merge_unprocessed$Rank2 <- ifelse (entity=="Bank", rank, rank+10)
data_final_merge_unprocessed$Rank <- NULL

Corp <- corpus(data_final_merge_unprocessed)
docnames(Corp)<-data_final_merge_unprocessed$uid
BK_Corp <- corpus_subset(Corp, Entity == "Bank")
CU_Corp <- corpus_subset(Corp, Entity == "CU")

ndoc(Corp)
ndoc(BK_Corp)
ndoc(CU_Corp)


# Grab texts for possible later use...
texts <- as.character(Corp)
head(texts)


###########################################################################
# Explore Corpus by creating a variable that captures summary data on corpus ###########################################################################
# These summary data are treaked as tokens

sum <-summary(Corp, n = 764)
mean(sum$Tokens)
median(sum$Tokens)
min(sum$Tokens)
max(sum$Tokens)
sd(sum$Tokens)

#Bank Data
BK_sum <-summary(BK_Corp,n = 163)
mean(BK_sum$Tokens)
median(BK_sum$Tokens)
min(BK_sum$Tokens)
max(BK_sum$Tokens)
sd(BK_sum$Tokens)


#CU Data
CU_sum <-summary(CU_Corp, n = 601)
mean(CU_sum$Tokens)
median(CU_sum$Tokens)
min(CU_sum$Tokens)
max(CU_sum$Tokens)
sd(CU_sum$Tokens)

####################################################
# Tokenize and DFM Corpus
####################################################

toks <- tokens(Corp, remove_punct = TRUE, remove_symbols = TRUE, remove_numbers = TRUE)
# toks<- tokens_keep(toks,pattern = "^[a-zA-Z]+$", valuetype = "regex")
toks<-tokens_select(toks,min_nchar=2L, selection = "remove")
toks <- tokens_compound(toks, pattern = phrase(c("small business", "small businesses", "main street","core market","core markets",
                                                 "credit unions", "credit union", "capital market", "capital markets",
                                                 "better return","better returns","competitive advantage","return on assets",
                                                 "return on equity", "return on investment","trading business",
                                                 "value proposition","core market","corporate social responsibility",
                                                 "best interest","give back","people before profit","profit sharing", 
                                                 "share in the profit"))) # compound tokens
toks <- tokens_remove(toks, pattern = stopwords("en")) #Remove stopwords 


######## OPTIONAL ######## 
# toks <- tokens_wordstem((toks)) #note stemwords may affect results
# toks <- tokens_remove(toks, c('bank', 'credit union')) to get rid of top words and see what happens
# toks2 <- tokens(Corp) # use only if we don't want to do any preprocessing

# DFM 
mydfm <- dfm(toks, tolower = T)
head(mydfm)

# now group the dfm by name (= speaker)
# this will be useful when I have several years of data
name_dfm <- dfm_group(mydfm, groups = name)
ndoc(name_dfm) #Should be 63; use in data matrix
head(name_dfm)

############################################################
# DICTIONARY
############################################################
#
# Running list of words to consider adding: 'soul' and 'volunteer'
# 
dictionary_list <-list("Banks"=c(list("100: Patrons"=c("shareholder","stockholder","shareholders", "stockholders", "share", "shares", "stock", "stocks"), 
                                      "200: Economic"=c("ROA", "ROE", "ROI", "better returns","buffer", "capital market", "competitive advantage", "competitor", "competitors", "core business", "differentiated", "efficiency", "efficient", "market", "markets", "productivity","return on assets", "return on equity", "return on investment", "trading business","valuation", "valuations", "value proposition","winning"),
                                      "300: Geopraphic"=c("core market", "core markets", "diversification", "Europe", "footprint","multinational","North America","redeploy", "redeployed", "segment", "segments", "South America", "U.S.", "United States", "US$"),
                                      "400: Stakeholder"=c("client","clients", "customer","customers"),
                                      "500: Governance"=c("appoint","appointed","calibre", "expert","expertise","proxy","proxies","talent"),
                                      "600: Horizon"=c("quarter", "quarters","short term"),
                                      "700: Identity"=c("bank"),
                                      "800: Values"=c("CSR","corporate social responsibility", "performance", "philanthropy"))), 
                       
                       
                       "Credit Unions"=c(list("100: Patrons"=c("member","members","owner","owners","own", "owned", "our","ours","yours"), 
                                              "200: Economic"=c("best interest", "give back", "patron","patrons", "people-before-profit", "profit sharing", "prosperity", "reciprocity", "share in the profit", "surplus","surpluses"), 
                                              "300: Geopraphic"=c("Alberta", "allocation", "British Columbia", "community","communities", "here", "local", "Manitoba","Nova Scotia", "neighbourhood", "neighbourhoods", "Ontario", "province", "region", "rural", "Saskatchewan"), 
                                              "400: Stakeholder"=c("employee","employees", "small business","small businesses", "worker","workers", "staff"), 
                                              "500: Governance"=c("association", "autonomy","autonomous","central", "co-operative","co-operatives", "collaboration", "collective", "cooperative","cooperatives", "coordination", "delegate","delegates", "democracy","democratic","elect", "elected","election","federation", "movement", "mutual", "solidarity", "together", "voice","voices"), 
                                              "600: Horizon"=c("long term"), 
                                              "700: identity"=c("credit union", "credit unions","caisse", "caisses"), 
                                              "800: Values"=c("B Corp", "B-Corp","beside", "bipoc","black","blacks", "caring", "diversity", "equal", "ESG", "exist", "fair", "force for good", "gender","help", "honest", "inclusive", "mission", "openess", "principle","principles", "purpose","real economy", "reconciliation", "SDG", "self-help", "socially responsible investing","steward", "stewards","the 'why'", "values", "valued", "vision", "well being", "wellbeing", "you"))))

View(dictionary_list)
mydictio <- dictionary(dictionary_list)

# KWIC of dictionary terms
dickwic <- kwic(toks, pattern =  mydictio, window = 10)
write.csv(dickwic, file="kwic.csv")


### KWIC without preprocessing
# dickwic2 <- kwic(toks2, pattern =  mydictio, window = 10) # without preprocessed token
# dickwic2 <- dickwic2[order(dickwic2$keyword),]
# write.csv(dickwic2, file="kwic2.csv")

# check terms by dictionary category; contrast list/unlist
wordcount <- list(mydictio)
wordcount
wordcount <- unlist(mydictio)
wordcount

# get number of all terms in dictionary
length(unlist(mydictio))
#Get number of dictionary categories for data matrix
length(mydictio) # Should be 2 (16 in Yoshikoder version)

# Apply dictionary; 
# Consider updating commands away from deprecated 'dictionary' 
dicdfm <- dfm_lookup(name_dfm, dictionary = mydictio)
dicdfm

###########################################################
# Statistical Model
###########################################################

# implement statistical model for each of the two subcategories
# but first, transform into data frame (easier to extract info)
# adjust number of documents/groups here (nrow)

model <- matrix(dicdfm, nrow = 63, ncol = 16) #Make matrix
model <- as.data.frame(model) #Make data frame
model[,17] <- docvars(dicdfm, "Entity") #Variable in DFM from Corpus; analogous to Regime
model[,18] <- row.names(dicdfm) # Adds column for names
colnames(model)[18] <- "ID"
# make ID rownames 
row.names(model) <- model[,18] #Change order of dataframe columns
# model[,18] <- NULL #Get rid of old column 18; skip this to retain names for later
# model[,19] <- docvars(dicdfm, "Rank2") # This needs to be 18 if using previous line 
# colnames(model)[19] <- "Rank" # This needs to change to 18 if using previous line 
head(model)


# add up categories (Banks vs CUs)
model$bank <- model$V1 + model$V2 + model$V3 + model$V4 + model$V5 + model$V6 + model$V7 + model$V8  #adds first 8 columns
model$coop <- model$V9 + model$V10 + model$V11 + model$V12 + model$V13 + model$V14 + model$V15 + model$V16 #adds second 8 columns
model

# Erase original columns
model[,c(1:16)] <- NULL
model


# apply model to estimate scale positions bank vs coop rhetoric
# apply model
# test for first bank / CU (V1):
# q is the estimate of the position on the bank vs co-op scale
q <- log((model$bank[1]+0.5)/(model$coop[1]+0.5))
q
# sigi are sigma values for the first dimension (i =ideological orientation) 
# sqrt is the function which computes the square root of a numeric vector
sigi <- sqrt((model$bank[1]+0.5)^-1 + (model$coop[1]+0.5)^-1)
sigi

# now for all cases as loop
n <- nrow(model)
for (i in 1:n){
  bk <- model$bank[i]
  cp <- model$coop[i]
  q[i] <- log((bk+0.5)/(cp+0.5))
  sigi[i] <- sqrt((model$bank[i]+0.5)^-1 + (model$coop[i]+0.5)^-1)
}
q

# add scale for first dimension and sigma values as columns to model
model$scalentity <- q
model$sigientity <- sigi
model

# now, calculate the confidence intervals on the x-axis 
# based on the interval formula as suggested by Lowe (2011) 
# [q - 1.96xsigma, q + 1.96xsigma]
model$xneg <- c(model$scalentity-(1.96*model$sigientity))
model$xpos <- c(model$scalentity+(1.96*model$sigientity))
model

# rename column (labelled V17) with entity information
colnames(model)[colnames(model)=="V17"] <- "bkcoop" 
head(model)

# change order of columns
model<-model[, c("ID","bkcoop", "bank", "coop","scalentity","sigientity","xneg","xpos")]
# export model with scale scores and names of entities 
save(model, file = "Our Data/model_entity.RData")


# construct boxplot as first overview on dictionary findings:
#Data for boxplot:
set.seed(1234)
type=model$bkcoop
value=model$scalentity
data=model

##########################################
# Fancy Plots
##########################################

# Boxplot:
# qplot( x=names , y=value , data=data , geom=c("boxplot","jitter") , fill=names, show.legend = FALSE)+
g <- ggplot(data, aes(x=type, y=value, fill=type), show.legend = FALSE)+
  geom_boxplot() +
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#FF3300", "#00CC00"))+
  xlab("") + ylab("Co-op vs IOF Rhetoric")+
  theme_bw(base_size = 10) +
  theme(panel.grid = element_blank())+
  coord_flip()
g

# function that takes in vector of data and a coefficient,
# returns boolean vector if a certain point is an outlier or not
# outlier = outside of whiskers, with are 1.5 x 25th / 75th quantile
check_outlier <- function(v, coef=1.5){
  quantiles <- quantile(v,probs=c(0.25,0.75))
  IQR <- quantiles[2]-quantiles[1]
  res <- v < (quantiles[1]-coef*IQR)|v > (quantiles[2]+coef*IQR)
  return(res)
}

# apply this to our data and label our outliers;
# to apply labels, added column called name(=data$ID). It's not necessary
dat <- data.table(group=data$bkcoop,value=data$scalentity, name=data$ID)
dat[,outlier:=check_outlier(value),by=group]
dat[,label:=ifelse(outlier,name,"")]

head(dat)

#plot
g=ggplot(dat,aes(x=group,y=value, fill=type), show.legend = FALSE)+
  geom_boxplot()+
  guides(fill=FALSE)+
  scale_fill_manual(values=c("#FF3300", "#00CC00"))+
  xlab("") + ylab("Bank vs Co-op Rhetoric")+
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())+
  coord_flip()+
  geom_text_repel(aes(label=label, colour=type),
                  position = position_dodge2(width = 0.01),
                  show.legend = FALSE
  )+
  scale_color_manual(values=c("#FF3300", "#00CC00"))
g


#plot
# g=ggplot(dat,aes(x=group,y=value, fill=type), show.legend = FALSE)+
#   geom_boxplot()+
#   guides(fill=FALSE)+
#   scale_fill_manual(values=c("#FF3300", "#00CC00"))+
#   xlab("") + ylab("Bank vs Co-op Rhetoric")+
#   theme_bw(base_size = 15) +
#   theme(panel.grid = element_blank())+
#   geom_text_repel(aes(label=label),hjust=1.1, size=3, nudge_x=0.1, nudge_y=0.1, box.padding = unit(0.3, "lines"))+
#   coord_flip()
# g


# now, construct the scale:
# first, optimize entity types
# Seems to organize things using the factor command along bk-coop lines
ord <- c("Bank", "CU")
model$bkcoop <- factor(model$bkcoop,levels=ord)

# now make nice plot with ggplot2
theme_set(theme_bw())


bkcoop_plot <- ggplot(model, aes(reorder(rownames(model), scalentity), scalentity, label=scalentity, shape= bkcoop, color = bkcoop)) + 
  geom_point(position = position_dodge(1),size=.9) +
  theme(axis.text.x = element_text(family = "serif",face="bold", color="black", hjust = 1,
                                   vjust=0.5,size=6, angle=90))+
  geom_errorbar(aes(ymin=xneg, ymax=xpos), width=.2,position = position_dodge(1)) +
  scale_shape_manual(name = "Entity Types:", values = c(8, 16)) + 
  scale_colour_manual(name = "Entity Types:", values = c("#FF3300", "#00CC00")) +
  labs(y = "Co-op vs IOF Rhetoric", x= "") +
  ylim(-4.5, 4.5) +
  geom_hline(yintercept = 0) +
  theme(legend.position="bottom") +
  theme(text=element_text(family = "serif", color = "black", size=9,face="bold"),
        plot.margin = margin(0,0,0,0, "cm"))

bkcoop_plot

