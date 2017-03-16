# file for locally testing scripts

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

# differences of Part of Speech
POSbyGender <- table(POSdata$file, POSdata$targetPOS)
chisq.test(POSbyGender)

# differences in following POS
followingPOSbyGender <- table(POSdata$file, POSdata$followingPOS)
chisq.test(followingPOSbyGender)

# summary tables
table(POSdata$file, POSdata$targetPOS)
table(POSdata$file, POSdata$followingPOS)

# vizualize differenves



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

# frequency of target POS & following words & their
targetPOSFreq <- orderedSubset(POSdata, "file", "targetPOS")
followingWordsFreq <- orderedSubset(POSdata, "file", "followingWord")
followingPOSsFreq <- orderedSubset(POSdata, "file", "followingPOS")


POSdata[grepl( ".NN*" , POSdata$targetPOS ), ]
with(data = POSdata[grepl( "JJ" , POSdata$targetPOS ),],
                    table(droplevels(followingWord), file))

# what's up with all those proper nouns?
wordFollowingProperNouns <- droplevels(POSdata[POSdata$targetPOS == "NNP", ])
orderedSubset(wordFollowingProperNouns, "file", "followingWord")


table(droplevels(POSdata[POSdata$file == "female" & 
                           POSdata$targetPOS == "NNP" &
                           POSdata$followingPOS == "NN", 
                         "followingWord"]))

orderedSubset(POSdata, "followingPOS", "targetPOS")
followingWords <- orderedSubset(POSdata, "file", "followingWord")
followingPOSs <- orderedSubset(POSdata, "file", "followingPOS")

# some logistic regressioning
library(nnet)

lm(targetPOS ~ data = head(POSdata[,1:3]))

# logistic model of factors. 
model <- multinom(targetPOS ~ file, data = POSdata[,1:3])
summary(model)

model <- multinom(targetPOS ~ followingPOS, data = POSdata[,1:3])
summary(model)
