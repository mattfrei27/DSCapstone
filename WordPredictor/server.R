library(shiny)
library(data.table)
library(tm)
library(stringi)

load('sub.pred.RData')
profanity <- read.csv('Terms-to-Block.csv',header = FALSE, skip=4,col.names=c('junk','words')) #List of profane words taken from: http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/


predictNextNWords <- function(sentence,n=5){
  if(n >5){
    n <- 5
  }
  cleanSentence <- as.character(cleanCorpus(VCorpus(VectorSource(sentence)),profanity = profanity)[[1]])
  cleanSentence <- stringi::stri_trans_general(cleanSentence,"latin-ascii")
  toks <- tail(unlist(strsplit(cleanSentence,'\\s',perl=TRUE)),3)
  pred <- vector(mode='character')
  for (i in 1:length(toks)){
    selectedPrefix <- toks[i:length(toks)]
    prefixStr <- paste(selectedPrefix, collapse = ' ')
    predAtCurLength <- sub.pred[(sub.pred$prefix == prefixStr) & (sub.pred$ngram.size == (length(selectedPrefix) + 1)),word]
    pred <- unique(append(pred,predAtCurLength[!is.na(predAtCurLength)]))
    if(sum(!is.na(pred)) >=n)
      return(pred[1:n])
  }
  return(sub.pred[(sub.pred$prefix == '') & (sub.pred$ngram.size == 1),word][1:n])
}


cleanCorpus <- function(corpus_in,profanity){
  corpus <- tm_map(corpus_in, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation) 
  corpus <- tm_map(corpus, removeNumbers) 
  corpus <- tm_map(corpus, removeWords, profanity$words)
  tm_map(corpus, stripWhitespace)
}


shinyServer(function(input, output) {
  pred <- reactive({ predictNextNWords(input$text) })
  word <- reactive({ write.table(pred()[1],
                                 row.names = FALSE,
                                 col.names = FALSE,
                                 sep = '\t',
                                 quote = FALSE) })
  otherWords <- reactive({ write.table(pred()[2:4],
                                       row.names = FALSE,
                                       col.names = FALSE,
                                       sep = '\t',
                                       quote = FALSE) })
  output$word <- renderPrint({ word() })
  output$otherWords <- renderPrint({ otherWords() })
  
})

# return(write.table(data.frame(pred[1:n]),
#                    row.names = FALSE, 
#                    col.names = FALSE,
#                    sep = "\t", 
#                    quote = FALSE))
