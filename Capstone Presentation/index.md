---
title       : Word Prediction
subtitle    : Data Science Capstone
author      : Matt Frei
job         : 
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

## Summary

In the modern world, an ever increasing share of the world's population communicates by text entered on mobile phones. For many, typing on a tiny mobile keyboard is a source of irritation. That's why companies such as SwiftKey have worked to develop algorithms that predict the next word you will type. 

Here I present a simple word prediction algorithm of my own design.

---

## Data

The word prediction algorithm is built on a large corpus of English text from the following three sources:

1. Blogs

2. Tweets

3. News Stories

---

## Data Processing

The raw data required significant preprocessing before it could be used to build a predictive model. The following are the steps taken:

1. Conversion to lower case

2. Removal of punctuation and numbers

3. Removal of offensive words ([List of profanities removed] (http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/))

4. Random Sampling

5. Tokenization into n-grams (consecutive sequences) of 1 to 4 words

---

## Predictive Model

Word prediction is performed using a simple n-gram model. The employed algorithm ranks word based on the frequency with which they are encountered immediately following a given sequence of 0 to 3 words (the 'prefix'). For example, suppose that the sequence "I like to" appears three times in the set of input text. If the next word is "swim" in one document and "eat" in the other two, the algorithm will rank eat as the more likely word to follow that prefix.

If a given prefix never appears in the input text, the first word in the prefix is discarded until a previously encountered prefix is found. If the prefix consists of a single word only, the most frequently occuring word in the input text is predicted.

Note that words appearing in fewer than 10 of the 600,000 documents sampled (200K from each of the three sources) have been removed.

---

## Interactive Application

The algorithm is available as an online [interactive tool](https://mattfrei.shinyapps.io/DS_Capstone_Word_Predictor/). Simply enter a few words in the first box and the algorithm will present you with words likely to follow those you entered.

Note that this n-gram approach considers at maximum, only the three preceeding words. So, context appearing earlier in a sentence does not influence the prediction.
