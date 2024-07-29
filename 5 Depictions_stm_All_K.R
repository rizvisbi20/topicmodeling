
##################################################################
##### Analysis with unsupervised techniques, EU speeches  ########
##################################################################

# OVERVIEW, what we do here:
# Analyze and visualize stm model with x topics
# (for all crucial presteps, e.g. loading and cleaning corpus and robustness tests, see script "Analysis_stm_preprobust" and "Analysis_stm_cleanvalid")

# clean environment
rm(list = ls())


# load packages
library(dplyr)
library(tidyverse)
library(tidytext)
library(quanteda)
library(stm)
library(stringr)
library(stringi)
library(XML)
library(RCurl)
library(SnowballC)
library(xml2)
library(ggplot2)
library(Rtsne)
library(rsvd)
library(geometry)
library(igraph)
library(stmCorrViz)
library(stargazer)
library(xtable)
library(rmarkdown)
library(beepr)

############################################

######## Apply unsupervised techniques #####

############################################


############################################
##### STM: Structural Topic Model ##########
############################################

# FIRST: load data and model and find labels for topics
# SECOND: visualize topics and their correlation with speakers 
# THIRD:  do network analysis on correlation among topics

#########################------------------###################################

# FIRST: load model and all other data we need, analyze model, find labels

load("Our Data/stm_obj_XX.RData")
load("Our Data/meta_stm.Rdata")
load("Our Data/name_unique.Rdata")
load("Our Data/all_documents.RData")
load("Our Data/data_final_merge_unprocessed.RData")
# name<-meta$ID
# name_unique <- unique(name)


# plot topic model, simple overview
plot(stm_obj_XX, type="summary", xlim=c(0,1))

# get most representative words in each topic to find most intuitive labels
xXX <- labelTopics(stm_obj_XX, topics = NULL, n = 5, frexweight = 0.5)
head(xXX)
xXX


# NOTE: this function gives us four different kind of word selection:
# prob: most frequent words
# FREX: most frequent AND exclusive terms
# lift/score: some variants of FREX - See STM  vignette for more


# now, check below for each topic findThoughts (= get most typical texts) to validate/improve label
# NOTE: texts have to be here a character vector where each entry contains the text of a document
# would our sorted (!) supercorpus work here? 
# Yes, subset, texts of supercorpus only, imported as findtexts above
# set topic to specific topic number and identify/evaluate qualitatively the labels for each topic
# Note you can specify a topic by setting topics = XX_

validate_labels <- findThoughts(stm_obj_XX, texts = findtexts, n = 3)
validate_labels


# Create dataframe or vector with representative text files
# Rizvi: It's not obvious which annual report they are pulling. 
# I could do it manually and figure it out but I'd like to spit ou t1 - t14 into something I can read

# define validated labels as vector to be included in all key graphs
# Note1 : Governance in CU texts often discussed in context of mergers
# Note2: Digital and covid linked in 2020 but digital cuts across time
topiclabs3 <- c('Community','Profitable', 'Shareholders')
topiclabs5 <- c('Covid', 'Profitable','Shareholders','Governance', 'Channels')
topiclabs7<- c('Cooperation','Profitable','Shareholders','Governance','Covid','Yours','Channels')
topiclabs9 <- c('Covid','Profitable', 'Banking (NA)','Governance','Young People','Yours','Channels','Cooperation','Global Banking')  # Note -- Shareholders splits into two topics here
topiclabs11 <- c('Covid','Profitable','Banking (NA)','Purpose','Young People','Yours','Channels','Cooperation','Global Banking','Governance','Deposit Guarantee')
topiclabs14 <- c('Covid','Profitable','Banking (NA)','ESG','Young People','Yours', 'local','Loans','Global Banking','Governance','Financial Statements','Channels','Quebec','Wealth Management')
topiclabs17 <- c('Covid','Profitable','Banking (NA)','ESG','Philanthropy','Yours','Purpose','Loans','Global Banking','Governance','Fin Statements','Channels','Quebec','Volunteers', 'Wealth Management','Efficiency','Patronage')
topiclabs20 <- c('Covid','Profitable','Banking (NA)','ESG','Service','Yours','Purpose','Loans','Global Banking','Governance','Financial Statements','Channels','Quebec','Volunteers', 'Wealth Management','Efficiency','Patronage','Cooperation','Savings','Saskatchewan')


#############################################################

# SECOND: visualizations
# FIGURE 2 in paper 
# Before we plot, we run a regression to estimate the relationships between topics and metadata (here: ID = CU/BK) 
# we take ID=CU/BK because we want to see what proportions of topics the messages of each CU/BK contain
# and what kind of topics which speakers share

rel_stm_obj_XX <- estimateEffect(1:XX~ID, stm_obj_XX, meta)

# now, write function to plot all estimated topic proportions in a loop one after the other (not included in stm package)
# including the 10 FREX words for each topic in the subtitle

plot_entitytopics <- function(x,y){
  for (i in 1:length(x$topics)){
    keywords <- labelTopics(y, topics = NULL, n = 10, frexweight = 0.5)
    frex <- keywords$frex[i,]
    frex <- paste(frex, collapse = ", ")
    frex[[i]] <- paste("Expected Proportion of Topic ", i, ": ", frex, collapse = "/n")
    par(cex=0.65, cex.lab = 1.4, cex.main = 2,
        mar = c(5,5,5,5))
    plot <- plot(x, covariate= "ID",  
                 model=y,
                 topics = x$topics[i],
                 method = "pointestimate",
                 main = "Estimates for Topics per Entity",
                 xlab=frex[i],
                 xlim = c(-0.2, 0.9),
                 ylab="Entity",
                 labeltype = "custom",
                 custom.labels = name_unique)
    allplots <- list()
    allplots[[i]] <- plot
    dev.off
  }
}

# make use of my nice plot function for plotting the regression results with the model

plot_entitytopics(rel_stm_obj_XX, stm_obj_XX)

# x topics
# rewrite my plot function, this time with labels as suggested above
plot_entitytopics <- function(x,y){
  for (i in 1:length(x$topics)){
    par(cex=0.65, cex.lab = 1.4, cex.main = 2,
        mar = c(5,5,5,5))
    plot <- plot(x, covariate= "ID",  
                 model=y,
                 topics = x$topics[i],
                 method = "pointestimate",
                 main = "Estimates for Topics per Entity",
                 xlab=topiclabsXX[i], #be sure to change THIS
                 xlim = c(-0.2, 0.9),
                 ylab="Name",
                 labeltype = "custom",
                 custom.labels = name_unique)
    allplots <- list()
    allplots[[i]] <- plot
    dev.off
  }
}


# plot all topics in a row with labels
plot_entitytopics(rel_stm_obj_XX, stm_obj_XX)


# plot for latex in better size
# par(mar = c(5,11,1,1), mfrow=c(1,1), family = "serif", bty = "n")
# plot_speakerlatex <- function(x,y){
#  for (i in 1:length(x$topics)){
#    plot <- plot(x, covariate= "ID",  
#                 model=y,
#                 topics = x$topics[i],
#                method = "pointestimate",
#                 xlab=topiclabs14[i],  # Change here too
#                 xlim = c(0, 1),
#                 ylim = c(0,40),
#                 labeltype = "custom",
#                 custom.labels = name_unique,
#                 cex = 2,
#                 cex.lab = 1.2,
#                 cex.axis = 1,
#                 width = 200)
#    allplots <- list()
#    allplots[[i]] <- plot
#    dev.off
#  }
#}

#plot_speakerlatex(rel_stm_obj_XX, stm_obj_XX)
#pdf("valid_stm/select_5.pdf")
dev.off()

### TABLE 2 in paper

# compile data frame to be exported as LaTeX table with labels, most frequent and frex words
# (similar to Lucas et al.)
get_words <- labelTopics(stm_obj_XX, topics = NULL, n = 10, frexweight = 0.5)

# pull and merge all prob (most frequent words)
for (i in 1:5){
  prob <- data.frame(get_words$prob, stringsAsFactors = FALSE)
  prob <- unite(prob, allprob, c("X1":"X10"), sep = ", ", remove = TRUE)
}

#rlang::last_error()

# pull and merge all frex (most frequent AND exclusive words)
for (i in 1:5){
  frex <- data.frame(get_words$frex, stringsAsFactors = FALSE)
  frex <- unite(frex, allfrex, c("X1":"X10"), sep = ", ", remove = TRUE)
}

# merge them into one data frame together with topiclabs14
tabletopics <- data.frame(topiclabsXX, prob, frex)

# merge both word colums
tabletopics <- unite(tabletopics, merger, c("allprob", "allfrex"), sep = " newline ", remove = TRUE)
tabletopics


# Export as LaTeX table
stargazer(tabletopics, summary = FALSE, rownames = FALSE)


# now plot also with covariate = scaleideo as continuous variable as per our dictionary results
# to see which topics are more Co-op orIOF according to our dictionary results
# for this, get estimateEffects with covariate = scaleideo 
meta$scalentity<-as.numeric(meta$scalentity)
rel_scale_XX <- estimateEffect(1:XX ~scalentity, stm_obj_XX, meta)


# and rewrite my plot function for this:
plot_bkcoop <- function(x,y){
  par(cex=0.65, cex.lab = 1.4, cex.main = 2,
      mar = c(5,5,5,5))
  plot <- plot(x, covariate= "scalentity",  
               model=y,
               topics = x$topics,
               method = "difference",
               cov.value1 = 5,
               cov.value2 = -5,
               main = "Estimates for Topic Proportions in Annual Report Statements",
               xlab="IOFs vs Co-ops as per Dictionary Results",
               xlim = c(-0.5, 0.5),
               ylab="Topics",
               labeltype = "custom",
               custom.labels = topiclabsXX, # SPECIFY topic labels here
               verbose.labels = "")
  #custom.labels = prob)
}

# Text after hashtag denotes statistical significant association with identity

# K=XX
plot_bkcoop(rel_scale_XX, stm_obj_XX)
# IOF:  
# Co-ops: 


# now rewrite my plot function again, simplified for LaTeX table
plot_latex <- function(x,y){
  par(cex=0.65, cex.lab = 1.4, cex.main = 2,
      mar = c(2,0,0,0), bty = "n")
  plot <- plot(x, covariate= "scalentity",  
               model=y,
               topics = x$topics,
               method = "difference",
               cov.value1 = 5,
               cov.value2 = -5,
               xlim = c(-0.55, 0.55),
               xlab = "",
               labeltype = "custom",
               custom.labels = "",
               verbose.labels = "")
}

plot_latex(rel_scale_XX, stm_obj_XX)



#########################-----------------####################################

# THIRD: network analysis

# take our model of XX topics and do something similar to Lucas et al (2015):
# 1. Color topic nodes: red = IOF , green = Co-op
# 2. Change thickness of edge as per strength of correlation between topics
# 3. Change size of node as per proportion of topic in overall corpus
# for this, make use of instructions from Lucas et al.'s (2015) replication scripts:
# first, get the topic proportions in corpus to determine the size of the nodes
# This is the matrix of Correlations Between Topics:
cor(stm_obj_XX$theta)

# Size of Topic: Size depends on how you calculate it.  
# stm_obj_XX$theta is a D-by-K matrix (so in der Art: D = Diagonal, K = Gesamtsumme)
# with document d in 1:D and its loading onto each topic  
# If you want frequency by word tokens then you just have to 
# multiply through the word counts within each document.
head(stm_obj_XX$theta)

# Lucas uses this vector of wordcounts - yet, there was typo in his script (sum(2) not sum[2,])
wordcounts <- unlist(lapply(stm_obj_XX$theta, function(x) sum(2)))

# there are fractional wordcounts due to variational approximation.
round(stm_obj_XX$theta[,1] * wordcounts,2)

# Calculate the proportion of words devoted to topics
# Change XX to appropriate number
topicPropsInCorpus <- rep(NA,XX)
for(i in 1:XX){
  topicPropsInCorpus[i] <- (sum(stm_obj_XX$theta[,i] * wordcounts))/sum(wordcounts)
}
# This now holds the topic proportions in the corpus
topicPropsInCorpus
# sums to one, as it should
sum(topicPropsInCorpus)
# add the topic labels
names(topicPropsInCorpus) <- topiclabsXX
# ok

# now, plot the network using a non-binary distance matrix
mat2 <- cor(stm_obj_XX$theta)
mat2
# setting the negatives to zero (i.e., no positve correlation = 0)
mat2[mat2<0] <- 0
# setting the diagonal to zero
diag(mat2) <- 0
# this gives us positive correlations between topics
mat2
# rename this object as "out"
out <- mat2
# out <- matrix(sample(0:1, 196, replace=TRUE, prob=c(0.9,0.1)), nc=14)
# set a seed so that it's reproducable
set.seed(2138)
# make a graph object called g
g <- graph.adjacency(out, mode="undirected", weighted=T)
if(length(labels)==0) labels = paste("Topic", topics)

# make the edges thickness proportional to correlation 
# (I need them thicker than Lucas et al., put 90* instead of 35*)
cor(stm_obj_XX$theta)[,1]
E(g)
edges <- get.edgelist(g)
edgecors <- rep(NA,nrow(edges))
for(i in 1:nrow(edges)){
  edgecors[i] <- cor(stm_obj_XX$theta)[edges[i,1],edges[i,2]]
}
edge.width=90*edgecors

# look at and set other graph parameters
E(g)$weight
E(g)$size <- 0.5
E(g)$lty <- 1
E(g)$color <- "black"

#Make the colors indicate the direction of the coefficient
# get the estimates
meta$scaleideo<-as.numeric(meta$scalentity)
prep <- plot.estimateEffect(rel_scale_XX, covariate= "scalentity",  
                            model=stm_obj_XX,
                            method = "difference",
                            cov.value1 = 5,
                            cov.value2 = -5,
                            main = "Estimates for Topic Proportions in Statements by Entity",
                            xlab="IOF vs Co-ops as per Dictionary Results",
                            xlim = c(-0.4, 0.4),
                            ylab="Topics",
                            labeltype = "custom",
                            custom.labels = topiclabsXX,
                            verbose.labels = "")

est <- unlist(lapply(prep$means,function(x){return(x[1])}))
est

# get colors, make them green (liberal) and red (illiberal), similar to dictionary scale
mycols <- rev(colorRampPalette(c("Red", "Green"), bias=1)( XX )) ## (n)
mycols
# assign the color category for each coeff (set range as per coeff. range, length = topic numbers)
seq(-0.1,0.1,length.out=XX)
colcat <- rep(NA,length(est))
for(i in 1:length(colcat)){
  colcat[i] <- max(which(est[i] > seq(-0.2,0.2,length.out=XX)))
}
# These are now the color category for each coefficient
colcat

# and these are the associated colors
mycols[colcat]
# This checks to make sure the colors are working as I expect
# Greent should be on the left
plot(est,1:XX,pch=19,cex=2,col=mycols[colcat]);abline(v=0)
# ok

# label the vertices
V(g)$label=topiclabsXX
# set the size of vertices proportional to the proportion in the corpus
V(g)$size <- topicPropsInCorpus*400
# set the color of vertices
vertex.color = mycols[colcat]
# set other vertex characteristics
vertex.label.cex = 0.9
vertex.label.color = "black"
# set the edge color
edge.color = "gray60"
# set a seet so that the layout is reproduceable
set.seed(333)
# pull out the weights to include in the layout
wts <- E(g)$weight
# make the layout
mylayout <- layout.fruchterman.reingold(g,weight=wts)
# start the image file
#pdf("Pics/Network.pdf", 8,8)
# do the plot
par(mar=c(1,1,1,1))
plot(g, layout=mylayout,edge.color=edge.color,vertex.color=vertex.color, vertex.label.cex=vertex.label.cex, vertex.label.color=vertex.label.color,edge.width=edge.width)
# close the image file
dev.off()
# end of script.

