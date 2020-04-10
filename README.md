# Text-Mining

Raw Code in File -  Text_Analysis.R

Knitr Code in Text Mining.Rmd

Visual Output in Text Mining.html

PART 1.
R script to download, extract and load the Sentence Polarity dataset v1.0 from [1]. The
dataset contains two files of 5331 positive and 5331 negative sentences from movie reviews in
Rotten Tomatoes website. After removing English stop-words:
a) A frequency plot of top 15 words
b) A word-cloud of 50 most common words
c) A grouped word-cloud of 50 most common words in positive and negative sentences
d) A plot of lexical diversity of randomly selected 20 sentences
e) A dendogram of hierarchical clustering of randomly selected 20 sentences.

PART 2.
Finding sentiment of tokens using Lexicoder Sentiment Dictionary
created by Young and Soroka (i.e. “data_dictionary_LSD2015” dictionary in quateda). Assigning
sentiment to each sentence based on the sentiment of most tokens and compare the results with
original sentiment in terms of accuracy, precision, recall, and F1-measure.

PART 3.
Splitting the corpus randomly in training (70% of sentences) and test (30% of sentences) datasets.
Training a Naïve Bayes classifier using the “textmodel_nb()” on training data and reporting its
performance on the test data in terms of accuracy, precision, recall, and F1-measure
