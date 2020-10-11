

if (1 == 0) {  ## change to (1 == 1) to run this

        library(data.table)
        library(dplyr)
        library(stringr)
        library(tidytext)
        
        setwd("~/R/datasciencecoursera/Course 10")
        
        # only do it onces, flag as off otherwise, as it takes 3 hours to run        

        # useful profiling function
        myTimeMeas <- function(...) {
                time_measurement <- system.time(eval(...))
                time_measurement[["user.self"]]
        }
        
                
        # read data from file
        
        con <- file("./Coursera-SwiftKey/final/en_US/en_US.news.txt", "rb")
        newsData <- readLines(con, encoding = "UTF-8")
        close(con)
        remove(con)
        
        # create a sample 10% of original
        
        set.seed(582)
        newsDataTiny <- newsData[sample(length(newsData),as.integer(0.4*length(newsData)))]
        
        # convert to data frame for tokenizing
        
        newsDataTinyDF <- as.data.frame(newsDataTiny)
        colnames(newsDataTinyDF) <- c("text")
        
        # tokenize the data into unique words
        
        myTimeMeas(tokens <- unnest_tokens(newsDataTinyDF,word,text))
        
        # first simple predictive model.  build unique 2-grams, and pointer to three 3-grams
        
        # make an index for text entry and word in text.  
        
        # First, Use token row names, and fix the 1st word issue by giving it num.0
        
        tokenRowNames <- as.data.frame(rownames(tokens))
        tokens$num <- as.character(apply(tokenRowNames,1,function(x) gsub("^[^.]*$",paste(x,".0",sep=""),x)))
        
        # split the numbers into text entry and word index
        
        textIndex <- function(x) {
                mysplit <- as.integer(strsplit(x["num"],"\\.")[[1]][1])
        }
        
        wordIndex <- function(x) {
                mysplit <- as.integer(strsplit(x["num"],"\\.")[[1]][2])
        }
        
        tokens$index1  <- apply(tokens,1,FUN=textIndex)
        tokens$index2  <- apply(tokens,1,FUN=wordIndex)
        
        write.csv(tokens,"tokensTimes4.csv")
        
}

if (1 == 0) {  ## change to (1 == 1) to run this
        
        library(data.table)
        library(dplyr)
        library(stringr)
        library(tidytext)
        
        setwd("~/R/datasciencecoursera/Course 10")
        
        # only do it onces, flag as off otherwise, as it takes 3 hours to run
        
        tokens <- read.csv("tokensTimes4.csv")

        # reduce to unique words and frequencies
        
        system.time(eval(uniqueTokens <- count(tokens, word, sort = TRUE)))
        
        names(uniqueTokens)[2] <- "freq"
        
        # analysis of word instances
        
        totalInstances <- sum(uniqueTokens[1:nrow(uniqueTokens),]$freq)
        
        # Turns out that 65000 words account for 99% of the nearly 3.5 million "word" 
        # instances found in this 10% sample of original data set.  
        # One finds this in this data set by trial and simple search.
        
        sum(uniqueTokens[1:80000,]$freq)/totalInstances
        
        # exclude words that amount to the less than 1% of word use criteria in this sample
        
        uniqueTokens <- uniqueTokens[1:80000,]
        
        # drop the frequency for this lookup index to words
        
        uniqueTokens <- uniqueTokens$word

        # make it a data table for creating a key
                
        uniqueTokensDT <- as.data.table(uniqueTokens)
        
        names(uniqueTokensDT)[1] <- "word"
        
        # optimize the lookup with an internal(hidden) sorted key
        # https://appsilon.com/fast-data-lookups-in-r-dplyr-vs-data-table/
        
        setkey(uniqueTokensDT)
        
        # add an index for quick reference
        
        uniqueTokensDT$index <- seq(1:nrow(uniqueTokensDT))
        
        # now we can select an element with an optimized key
        filter(uniqueTokensDT, word=="had")
        
        
        write.csv(uniqueTokensDT,"uniqueTokensDTv2.csv")
        
        
        # build the unique word key into a word index on tokens


        # function to assigning index for a given row
        
        indexToWords <- function(x) {
                
                tokenWord <- x["word"]
                myIndex <- -1
                if (tokenWord %in% uniqueTokensDT$word) {
                        myIndex <- filter(uniqueTokensDT, word == tokenWord)$index 
                }
                
                return (myIndex)
        }
        
        # do this in segments, as it takes considerable computing time
        
        seg1 <- tokens[1:500000,]
        system.time(eval(myIndex <- apply(seg1,1,indexToWords)))
        seg1$wordIndex <- myIndex
        
        write.csv(seg1,"seg1v2.csv")
        remove(seg1)
        gc()
        
        seg2 <- tokens[500001:1000000,]
        system.time(eval(myIndex <- apply(seg2,1,indexToWords)))
        seg2$wordIndex <- myIndex
        
        write.csv(seg2,"seg2v2.csv")
        remove(seg2)
        gc()
        
        seg3 <- tokens[1000001:1500000,]
        system.time(eval(myIndex <- apply(seg3,1,indexToWords)))
        seg3$wordIndex <- myIndex
        
        write.csv(seg3,"seg3v2.csv")
        remove(seg3)
        gc()
        
        seg4 <- tokens[1500001:2000000,]
        system.time(eval(myIndex <- apply(seg4,1,indexToWords)))
        seg4$wordIndex <- myIndex
        
        write.csv(seg4,"seg4v2.csv")
        remove(seg4)
        gc()
        
        seg5 <- tokens[2000001:2500000,]
        system.time(eval(myIndex <- apply(seg5,1,indexToWords)))
        seg5$wordIndex <- myIndex
        
        write.csv(seg5,"seg5v2.csv")
        remove(seg5)
        gc()
        
        seg6 <- tokens[2500001:3000000,]
        system.time(eval(myIndex <- apply(seg6,1,indexToWords)))
        seg6$wordIndex <- myIndex
        
        write.csv(seg6,"seg6v2.csv")
        remove(seg6)
        gc()

        seg7 <- tokens[3000001:3500000,]
        system.time(eval(myIndex <- apply(seg7,1,indexToWords)))
        seg7$wordIndex <- myIndex
        
        write.csv(seg7,"seg7v2.csv")
        remove(seg7)
        gc()
        
        
        seg8 <- tokens[3500001:4000000,]
        system.time(eval(myIndex <- apply(seg8,1,indexToWords)))
        seg8$wordIndex <- myIndex
        
        write.csv(seg8,"seg8v2.csv")
        remove(seg8)
        gc()
        
        seg9 <- tokens[4000001:4500000,]
        system.time(eval(myIndex <- apply(seg9,1,indexToWords)))
        seg9$wordIndex <- myIndex
        
        write.csv(seg9,"seg9v2.csv")
        remove(seg9)
        gc()

        
        seg10 <- tokens[4500001:5000000,]
        system.time(eval(myIndex <- apply(seg10,1,indexToWords)))
        seg10$wordIndex <- myIndex
        
        write.csv(seg10,"seg10v2.csv")
        remove(seg10)
        gc()
        
                
        seg11 <- tokens[5000001:5500000,]
        system.time(eval(myIndex <- apply(seg11,1,indexToWords)))
        seg11$wordIndex <- myIndex
        
        write.csv(seg11,"seg11v2.csv")
        remove(seg11)
        gc()
        

        seg12 <- tokens[5500001:6000000,]
        system.time(eval(myIndex <- apply(seg12,1,indexToWords)))
        seg12$wordIndex <- myIndex
        
        write.csv(seg12,"seg12v2.csv")
        remove(seg12)
        gc()
        
        seg13 <- tokens[6000001:6500000,]
        system.time(eval(myIndex <- apply(seg13,1,indexToWords)))
        seg13$wordIndex <- myIndex
        
        write.csv(seg13,"seg13v2.csv")
        remove(seg13)
        gc()
        
        
        seg14 <- tokens[6500001:7000000,]
        system.time(eval(myIndex <- apply(seg14,1,indexToWords)))
        seg14$wordIndex <- myIndex
        
        write.csv(seg14,"seg14v2.csv")
        remove(seg14)
        gc()
        
 
        seg15 <- tokens[7000001:7500000,]
        system.time(eval(myIndex <- apply(seg15,1,indexToWords)))
        seg15$wordIndex <- myIndex
        
        write.csv(seg15,"seg15v2.csv")
        remove(seg15)
        gc()
        
        
        seg16 <- tokens[7500001:8000000,]
        system.time(eval(myIndex <- apply(seg16,1,indexToWords)))
        seg16$wordIndex <- myIndex
        
        write.csv(seg16,"seg16v2.csv")
        remove(seg16)
        gc()
        
        
        seg17 <- tokens[8000001:8500000,]
        system.time(eval(myIndex <- apply(seg17,1,indexToWords)))
        seg17$wordIndex <- myIndex
        
        write.csv(seg17,"seg17v2.csv")
        remove(seg17)
        gc()
        
        
        seg18 <- tokens[8500001:9000000,]
        system.time(eval(myIndex <- apply(seg18,1,indexToWords)))
        seg18$wordIndex <- myIndex
        
        write.csv(seg18,"seg18v2.csv")
        remove(seg18)
        gc()
        
        
        seg19 <- tokens[9000001:9500000,]
        system.time(eval(myIndex <- apply(seg19,1,indexToWords)))
        seg19$wordIndex <- myIndex
        
        write.csv(seg19,"seg19v2.csv")
        remove(seg19)
        gc()
        
        
        seg20 <- tokens[9500001:10000000,]
        system.time(eval(myIndex <- apply(seg20,1,indexToWords)))
        seg20$wordIndex <- myIndex
        
        write.csv(seg20,"seg20v2.csv")
        remove(seg20)
        gc()
        
        
        seg21 <- tokens[10000001:10500000,]
        system.time(eval(myIndex <- apply(seg21,1,indexToWords)))
        seg21$wordIndex <- myIndex
        
        write.csv(seg21,"seg21v2.csv")
        remove(seg21)
        gc()
        
        
        seg22 <- tokens[10500001:11000000,]
        system.time(eval(myIndex <- apply(seg22,1,indexToWords)))
        seg22$wordIndex <- myIndex
        
        write.csv(seg22,"seg22v2.csv")
        remove(seg22)
        gc()
        
        
        seg23 <- tokens[11000001:11500000,]
        system.time(eval(myIndex <- apply(seg23,1,indexToWords)))
        seg23$wordIndex <- myIndex
        
        write.csv(seg23,"seg23v2.csv")
        remove(seg23)
        gc()
        
        
        seg24 <- tokens[11500001:12000000,]
        system.time(eval(myIndex <- apply(seg24,1,indexToWords)))
        seg24$wordIndex <- myIndex
        
        write.csv(seg24,"seg24v2.csv")
        remove(seg24)
        gc()
        
        
        seg25 <- tokens[12000001:12500000,]
        system.time(eval(myIndex <- apply(seg25,1,indexToWords)))
        seg25$wordIndex <- myIndex
        
        write.csv(seg25,"seg25v2.csv")
        remove(seg25)
        gc()
        
        
        seg26 <- tokens[12500001:13000000,]
        system.time(eval(myIndex <- apply(seg26,1,indexToWords)))
        seg26$wordIndex <- myIndex
        
        write.csv(seg26,"seg26v2.csv")
        remove(seg26)
        gc()
        
        
        seg27 <- tokens[13000001:13500000,]
        system.time(eval(myIndex <- apply(seg27,1,indexToWords)))
        seg27$wordIndex <- myIndex
        
        write.csv(seg27,"seg27v2.csv")
        remove(seg27)
        gc()        
        
        seg28 <- tokens[13500001:13891322,]
        system.time(eval(myIndex <- apply(seg28,1,indexToWords)))
        seg28$wordIndex <- myIndex
        
        write.csv(seg28,"seg28v2.csv")
        remove(seg28)
        gc()         
        
        
               
        
                
        # tokens <- seg1
        # tokens <- rbind(tokens,seg2)
        # tokens <- rbind(tokens,seg3)
        # tokens <- rbind(tokens,seg4)
        # tokens <- rbind(tokens,seg5)
        # tokens <- rbind(tokens,seg6)
        # tokens <- rbind(tokens,seg7)
        
        # write.csv(tokens,"tokens.csv")


}


if (1 == 0) {  ## change to (1 == 1) to run this
 
        # Read in Segments, rbind them, and write out as tokensWithIndex
        
        
        setwd("~/R/datasciencecoursera/Course 10")
        
        library(data.table)
        library(dplyr)
        library(stringr)
        library(tidytext)
        
        
        tokensWIndex <- read.csv("./segments/seg1v2.csv")
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg2v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg3v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg4v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg5v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg6v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg7v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg8v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg9v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg10v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg11v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg12v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg13v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg14v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg15v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg16v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg17v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg18v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg19v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg20v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg21v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg22v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg23v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg24v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg25v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg26v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg27v2.csv"))
        tokensWIndex <- rbind(tokensWIndex,read.csv("./segments/seg28v2.csv"))
        
        write.csv(tokensWIndex,"tokensWIndex_v2.csv")
        
}

if (1 == 0) {  ## change to (1 == 1) to run this
        
        library(data.table)
        library(dplyr)
        library(stringr)
        library(tidytext)
        
        setwd("~/R/datasciencecoursera/Course 10")
        
        tokens <- read.csv("tokensWIndex_v2.csv")
        uniqueTokensDT <- read.csv("uniqueTokensDTv2.csv")
        
        tokens$X.1 <- NULL
        tokens$X.2 <- NULL
        tokens$X   <- NULL
        tokens$num <- NULL
        
        uniqueTokensDT$X <- NULL
        
        # optimize the lookup with an internal(hidden) sorted key
        # https://appsilon.com/fast-data-lookups-in-r-dplyr-vs-data-table/
        

        
        # now build 3 grams that allow prediction of a 3rd character with 2 priors
        
        threeGram <- as.data.frame(tokens$index2)
        names(threeGram)[1] <- "index2"
        threeGram$word3Index <- tokens$wordIndex
        tempForWord2 <- threeGram$word3Index
        threeGram$word2Index <- append(c(-1),tempForWord2[1:length(tempForWord2)-1],)
        tempForWord1 <- threeGram$word2Index
        threeGram$word1Index <- append(c(-1),tempForWord1[1:length(tempForWord1)-1],)
        tempForFinal3Gram <- as.data.frame(threeGram$index2)
        names(tempForFinal3Gram)[1] <- "index2"
        #tempForFinal3Gram$wordIndex <- tokens$wordIndex
        tempForFinal3Gram$word1Index <- threeGram$word1Index
        tempForFinal3Gram$word2Index <- threeGram$word2Index
        tempForFinal3Gram$word3Index <- threeGram$word3Index
        
        tempForFinal3Gram <- filter(tempForFinal3Gram, index2 != 0)
        tempForFinal3Gram <- filter(tempForFinal3Gram, index2 != 1)
        tempForFinal3Gram <- filter(tempForFinal3Gram, word1Index != -1)
        tempForFinal3Gram <- filter(tempForFinal3Gram, word2Index != -1)
        tempForFinal3Gram <- filter(tempForFinal3Gram, word3Index != -1)
        
        threeGram <- as.data.frame(tempForFinal3Gram$word1Index)
        names(threeGram)[1] <- "word1Index"
        threeGram$word2Index <- tempForFinal3Gram$word2Index
        threeGram$word3Index <- tempForFinal3Gram$word3Index
        
        remove(tempForFinal3Gram)
        remove(tempForWord1)
        remove(tempForWord2)
        
        write.csv(threeGram,"threeGram_v2.csv")
        
        
}

if (1 == 0) {

        
        library(data.table)
        library(dplyr)
        library(stringr)
        library(tidytext)
        
        threeGram        <- read.csv("threeGram_v2.csv")
        threeGram$X <- NULL
        
        uniqueTokensDT   <- read.csv("uniqueTokensDTv2.csv")
        uniqueTokensDT$X <- NULL
        
        # remove all 3-grams for any words in 3-gram starting as numeric or spec charc 
        # this corresponds to word index 1 through 6550
                
        getAllMatchingSuggetions <- function(word1,word2) {
                
                index1 <- filter(uniqueTokensDT,word==word1)$index
                index2 <- filter(uniqueTokensDT,word==word2)$index
                
                filter(threeGramUniFreq, word1Index == index1, word2Index == index2)
                        
        }        
                
        
        # We have 12,757,477 three grams.  Filter out the ones with word starting with number
        
        threeGram <- filter(threeGram,word1Index>6550,word2Index>6550,word3Index>6550)
        
        # create frequency table of unique 3-grams.  This will reduce it to 7,614,035 3-grams
        
        threeGram <- as.data.table(threeGram)
        threeGramUniFreq <- count(threeGram,word1Index,word2Index,word3Index,sort=TRUE)
        
        remove(threeGram)
        
        write.csv(threeGramUniFreq,"threeGramUniFreq.csv")


}


library(data.table)
library(dplyr)
library(stringr)
library(tidytext)

threeGramUniFreq   <- read.csv("threeGramUniFreq_v2.csv")
threeGramUniFreq$X <- NULL
threeGramUniFreq   <- as.data.table(threeGramUniFreq)
threeGramUniFreq <- threeGramUniFreq[order(word1Index,word2Index,-n),]
setkey(threeGramUniFreq,word1Index,word2Index)

uniqueTokensDT     <- read.csv("uniqueTokensDTv2.csv")
uniqueTokensDT$X   <- NULL
uniqueTokensDT     <- as.data.table(uniqueTokensDT)
setkey(uniqueTokensDT,word)

getAllMatchingSuggestions <- function(word1,word2) {
   
        # default is the no-match empty single string
        returnList <- c("")

        if ((word1 %in% uniqueTokensDT$word) && (word2 %in% uniqueTokensDT$word))
        {
                index1 <- filter(uniqueTokensDT,word==word1)$index
                index2 <- filter(uniqueTokensDT,word==word2)$index

                # test that we don't have a starting numeric [0-9] or special character
                if ((index1 > 6550) && (index2 > 6550)) 
                {
                                
                        if ((index1 %in% threeGramUniFreq$word1Index) && 
                            (index2 %in% threeGramUniFreq$word2Index)) 
                        {
                                returnList <- filter(threeGramUniFreq, 
                                                     word1Index == index1, 
                                                     word2Index == index2)$word3Index
                                if (length(returnList) > 3) 
                                {
                                        returnList <- returnList[1:3]
                                }
                                # convert the indexes to words
                                returnList <- apply(as.data.frame(returnList),1,function(x) uniqueTokensDT[x]$word)

                        }
                }
        } 
                
        return (returnList)
}

getAllMatchingSuggestions("we","have")
getAllMatchingSuggestions("while","i'll")

lastTwoWords <- function(sentence) 
{
        # tokenize the sentence into unique words
        sentenceTokens <- unnest_tokens(sentence,word,text)
        return (c(sentenceTokens[nrow(sentenceTokens)-1,"word"],sentenceTokens[nrow(sentenceTokens),"word"]))
}


suggestAnother <- function(sentence)
{
        sentenceIn <- as.data.frame(sentence)
        names(sentenceIn)[1] <- "text"
        
        lastTwo <- lastTwoWords(sentenceIn)
        getAllMatchingSuggestions(lastTwo[1],lastTwo[2])  
}


suggestAnother("mary had a little lamb whose fleece was white as ")

