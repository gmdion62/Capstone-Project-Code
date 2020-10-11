#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
      
          
        output$suggestion1 <-  renderText({
              
                theSuggestions <- suggestAnother(input$sentence)
                
                if (length(theSuggestions) == 0)
                        paste("Sorry, No Suggestion For This Phraase","",sep=" ")     
                else if (length(theSuggestions) >= 1)
                        paste("Choice 1: ",theSuggestions[1],sep=" ")
                else
                        ""
                
        })
        
        output$suggestion2 <-  renderText({
                
                theSuggestions <- suggestAnother(input$sentence)
                
                
                if (length(theSuggestions) >= 2)
                        paste("Choice 2: ",theSuggestions[2],sep=" ")
                else
                        ""
                
        })
        
        output$suggestion3 <-  renderText({
                
                theSuggestions <- suggestAnother(input$sentence)
                
                
                if (length(theSuggestions) >= 3)
                        paste("Choice 3: ",theSuggestions[3],sep=" ")
                else
                        ""
                
        })

        
})
