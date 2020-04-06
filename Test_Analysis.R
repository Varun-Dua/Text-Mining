
URL = "http://www.cs.cornell.edu/people/pabo/movie-review-data/rt-polaritydata.tar.gz"
download.file(URL, "rt-polaritydata.tar.gz")
untar("rt-polaritydata.tar.gz")
library(readtext)
library(quanteda)

df_neg <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.neg"), 
                     stringsAsFactors = FALSE)
df_neg['sentiment'] <- "neg"

df_pos <- data.frame(sentence = readLines("./rt-polaritydata/rt-polarity.pos"), 
                     stringsAsFactors = FALSE)
df_pos['sentiment'] <- "pos"

corp_moives <- corpus(rbind(df_neg, df_pos), text_field='sentence')
summary(corp_moives,10)
head(corp_moives)

toks_corp <- tokens(corp_moives, remove_punct = TRUE)

nostop_corp_movies <- tokens_select(toks_corp, pattern = stopwords('en'), selection = 'remove')
print(nostop_corp_movies)

df <- dfm(nostop_corp_movies)
tf <- topfeatures(df, 15)
ts <- textstat_frequency(df)

ts<-head(ts,15)
tf

library(ggplot2)
fplot<- ggplot(data = ts, aes(ts$feature, ts$frequency)) + geom_line(group = 1)
fplot

set.seed(132)
textplot_wordcloud(df, max_words = 50)

dfmat_grouped <- dfm(nostop_corp_movies, groups = "sentiment")
set.seed(132)
textplot_wordcloud(dfmat_grouped, comparison = TRUE, max_words = 50)

library(dplyr)


tstat_lexdiv <- textstat_lexdiv(df)
sampled_df<- sample_n(tstat_lexdiv,20)
splot<- ggplot(data = sampled_df, aes(sampled_df$document, sampled_df$TTR)) + geom_line(group = 1)
splot

set.seed(123)
cs<- corpus_sample(corp_moives, 20)
toks_new <- tokens(cs)
df <- dfm(toks_new, remove = stopwords('en'))
dist_mat <- as.dist(textstat_dist(df))
clust<- hclust(dist_mat)
plot(clust, xlab = "Distance", ylab = NULL)


sentiment <- tokens_lookup(nostop_corp_movies, dictionary = data_dictionary_LSD2015[1:2], 
              exclusive = FALSE, nested_scope = "dictionary")
print(sentiment)
sent_mat<-dfm(sentiment)
gr<-dfm_group(sent_mat,group = 'sentiment', fill = TRUE) 

a<-sent_mat[,c("negative", "positive")]
a[,"negative"]
v<-dfm_select(sent_mat, pattern = c("negative", "positive"))

test<- v[,"negative"] - v[,"positive"]
class(test)
n <- dfm_subset(v, as.vector(test))
n
p<- dfm_subset(v, as.vector(!test))
p

f<-function(test){
  if(test == 0){
    print("Nuetral")
  }
  else if(test>0){
    print("Neg")
  }
  else{
    print("pos")
  }
}

docvars(v, field = "NewSentiment") <- f(test)
v
v[,"New Sentiment"]


library(quanteda)
library(quanteda.textmodels)
library(quanteda.corpora)
library(caret)


set.seed(300)
id_train <- sample(1:10662, 7463, replace = FALSE)
head(id_train, 10)

corp_moives$id_numeric <- 1:ndoc(corp_moives)
summary(corp_moives)

dfmat_training <- corpus_subset(corp_moives, id_numeric %in% id_train) %>%
  dfm(remove = stopwords("english"), stem = TRUE)
dfmat_test <- corpus_subset(corp_moives, !id_numeric %in% id_train) %>%
  dfm(remove = stopwords("english"), stem = TRUE)
tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$sentiment)
summary(tmod_nb)

dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))

actual_class <- dfmat_matched$sentiment
predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
tab_class <- table(actual_class, predicted_class)
tab_class
confusionMatrix(tab_class, mode = "everything")

