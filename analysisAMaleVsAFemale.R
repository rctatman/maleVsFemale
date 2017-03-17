# libraries we'll need
library(nnet)
library(ggplot2)

# read in data
POSdata <- read.csv("results.csv", header = T)
head(POSdata)

# read in list of tags
tags <- read.csv("pennTreebankLabels.csv", header = T)
head(tags)

# Not working right now, uncomment at your peril
# replace target POS tag with full discription
# for(i in POSdata$targetPOS){
#   POSdata$targetPOS[i] <- tags[tags$Tag == i, "Description"]
# }
# 
# # replace following POS with full discription
# for(i in POSdata$followingPOS){
#   POSdata$followingPOS[followingPOS == i] <- tags[tags$Tag == i, "Description"]
# }
# 
# # make sure it worked
# head(POSdata)

# # attach datafile for easier reference
# attach(POSdata)

#### data exploration ####

# summary tables
table(POSdata$file, POSdata$targetPOS)
table(POSdata$file, POSdata$followingPOS)
table(POSdata$targetPOS, POSdata$followingPOS)

# frequency of target POS, following POS & following word by file
targetPOSFreq <- orderedSubset(POSdata, "file", "targetPOS")
followingWordsFreq <- orderedSubset(POSdata, "file", "followingWord")
followingPOSsFreq <- orderedSubset(POSdata, "file", "followingPOS")

# data with only nouns (bare, plural or proper) as target POS
POSdata[grepl( ".NN*" , POSdata$targetPOS ), ]

# what is the following word if the target POS is an adjective?
with(data = POSdata[grepl( "JJ" , POSdata$targetPOS ),],
                    table(droplevels(followingWord), file))

# what's up with all those proper nouns?
wordFollowingProperNouns <- droplevels(POSdata[POSdata$targetPOS == "NNP", ])
orderedSubset(wordFollowingProperNouns, "file", "followingWord")

# what are the words after "female" that are tagged as noun noun compounds?
table(droplevels(POSdata[POSdata$file == "female" & 
                           POSdata$targetPOS == "NN" &
                           POSdata$followingPOS == "NN", 
                         "followingWord"]))
# following "male"
table(droplevels(POSdata[POSdata$file == "male" & 
                           POSdata$targetPOS == "NN" &
                           POSdata$followingPOS == "NN", 
                         "followingWord"]))


# most common following words & following POS
followingWords <- orderedSubset(POSdata, "file", "followingWord")
followingPOSs <- orderedSubset(POSdata, "file", "followingPOS")

#### visualization ####

# first remove all POS

# POS & following POS by freq
followingWordsFreq <- orderedSubset(POSdata, "targetPOS", "followingPOS")
# uncomment next line to remove sparse rows
 followingWordsFreq <- followingWordsFreq[followingWordsFreq$Freq > 10,]

femaleBiasList <- NULL
maleBiasList <- NULL
for(i in 1:dim(followingWordsFreq)[1]){
  subset <- POSdata[POSdata$targetPOS == followingWordsFreq$Var1[i] &
    POSdata$followingPOS == followingWordsFreq$Var2[i],]
  femaleBias <- summary(subset$file)["female"]/sum(summary(subset$file))
  maleBias <- summary(subset$file)["male"]/sum(summary(subset$file))
  femaleBiasList <- c(femaleBiasList, femaleBias)
  maleBiasList <- c(maleBiasList, maleBias)
}

followingWordsFreq$femaleBias <- femaleBiasList
followingWordsFreq$maleBias <- maleBiasList

# plot of female-biased squares
ggplot(followingWordsFreq, aes(Var2, Var1)) + 
  # width = Freq box width equiv. to freq
  geom_tile(aes(fill = femaleBias)) + 
  scale_fill_gradient(low = "cyan", high = "darkred") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Following POS") + ylab("Target POS") +
  labs(fill = "Proportion 'female'")

# plot of frequency of occurance
ggplot(followingWordsFreq, aes(Var2, Var1, fill = Freq)) + 
  geom_tile(aes(fill = Freq)) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 12),
        plot.title = element_text(size=16),
        axis.title=element_text(size=14,face="bold"),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("Following POS") + ylab("Target POS") +
  labs(fill = "")

  
#### statistical modeling ####

# chi square tests

# differences of Part of Speech
POSbyGender <- table(POSdata$file, POSdata$targetPOS)
chisq.test(POSbyGender)

# differences in following POS
followingPOSbyGender <- table(POSdata$file, POSdata$followingPOS)
chisq.test(followingPOSbyGender)

# some logistic regressioning
# logistic model of factors. 
model <- multinom(targetPOS ~ file, data = POSdata[,1:3])
summary(model)

model <- multinom(targetPOS ~ followingPOS, data = POSdata[,1:3])
summary(model)

#### Functions ####
# function to get a list of factor1 by factor2, sorted by frequency, w/ default
# option to return the top 10
orderedSubset <- function(data, factor1, factor2 = NULL, topTen = T){
  # make a table from the factor in the data, then order it
  if(is.null(factor2) == F){
    newTable <- as.data.frame(table(data[,factor1], data[,factor2]))
  }else{
    newTable <- as.data.frame(table(data[,factor1]))
  }
  newTable <-  newTable[order(-newTable$Freq),]
  
  # return top ten terms if asked for
  if(topTen == T){ print(newTable[1:10,])}
  
  # return ordered subset 
  return(newTable)
}
