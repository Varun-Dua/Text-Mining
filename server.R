#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(e1071)
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

corp_movies <- corpus(rbind(df_neg, df_pos), text_field='sentence')

toks_corp <- tokens(corp_movies, remove_punct = TRUE)

nostop_corp_movies <- tokens_select(toks_corp, pattern = stopwords('en'), selection = 'remove')
df <- dfm(nostop_corp_movies)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {


    output$Plot <- renderPlot({

        if(input$radio1 == 1)
        {
            
            set.seed(132)
            
            return (textplot_wordcloud(df, max_words = input$slider1))
        }
        else if(input$radio1 == 2)
        {
            dfmat_grouped <- dfm(nostop_corp_movies, groups = "sentiment")
            set.seed(132)
            return (textplot_wordcloud(dfmat_grouped, comparison = TRUE, max_words = input$slider1))
        }
        
        else if(input$radio1 == 3)
        {
            ts <- textstat_frequency(df)
            
            ts <- head(ts,input$slider1)
            library(ggplot2)
            fplot<- ggplot(data = ts, aes(ts$feature, ts$frequency)) + geom_line(group = 1) + labs(x = "Feature", y = "Frequency")
            return(fplot)  
        }
        
        else if(input$radio1 == 4)
        {
            library(dplyr)
            set.seed(1234)
            tstat_lexdiv <- textstat_lexdiv(df)
            sampled_df<- sample_n(tstat_lexdiv,input$slider1)
            splot<- ggplot(data = sampled_df, aes(sampled_df$document, sampled_df$TTR)) + geom_line(group = 1) + labs(x = "Document", y = "Frequency")
            return(splot)
        }
        
        else if(input$radio1 == 5)
        {
            set.seed(123)
            cs<- corpus_sample(corp_movies, input$slider1)
            toks_new <- tokens(cs)
            df <- dfm(toks_new, remove = stopwords('en'))
            dist_mat <- as.dist(textstat_dist(df))
            clust<- hclust(dist_mat)
            return(plot(clust, xlab = "Distance", ylab = NULL))
        }
        
        
        else
        {
            return ("No plot selected.")
        }

    })
    
    output$Confusion <- renderPrint({
        
        if(input$radio2 == 1)
        {
            library(quanteda)
            library(quanteda.corpora)
            library(caret)
            sentiment <- tokens_lookup(nostop_corp_movies, dictionary = data_dictionary_LSD2015[1:2], 
                                       exclusive = FALSE, nested_scope = "dictionary")
            sent_mat<-dfm(sentiment)
            sent_df<- convert(sent_mat[,c("negative","positive")], to = "data.frame")
            sent_df<- cbind(sent_df, docvars(sent_mat))
            
            library(dplyr)
            sent_df<- mutate(sent_df, diff = positive - negative)
            sent_df$newSent<- with(sent_df, ifelse(diff>=0, "pos", "neg"))
            
            sent_df<- select(sent_df, c(sentiment, newSent))
            
            tab_class <- table(sent_df$sentiment, sent_df$newSent)
            
            tab_class
            
            return (confusionMatrix(tab_class, mode = "everything"))
        }
        
        else if(input$radio2 == 2)
        {
            library(quanteda)
            library(quanteda.textmodels)
            library(quanteda.corpora)
            library(caret)
            
            set.seed(300)
            id_train <- sample(1:10662, 7463, replace = FALSE)
            head(id_train, 10)
            
            corp_movies$id_numeric <- 1:ndoc(corp_movies)
            
            dfmat_training <- corpus_subset(corp_movies, id_numeric %in% id_train) %>%
                dfm(remove = stopwords("english"), stem = TRUE)
            dfmat_test <- corpus_subset(corp_movies, !id_numeric %in% id_train) %>%
                dfm(remove = stopwords("english"), stem = TRUE)
            tmod_nb <- textmodel_nb(dfmat_training, dfmat_training$sentiment)
            summary(tmod_nb)
            
            dfmat_matched <- dfm_match(dfmat_test, features = featnames(dfmat_training))
            
            actual_class <- dfmat_matched$sentiment
            predicted_class <- predict(tmod_nb, newdata = dfmat_matched)
            tab_class <- table(actual_class, predicted_class)
            
            tab_class
            
            return(confusionMatrix(tab_class, mode = "everything"))
        }
        
        else
        {
            return("No matrix selected.")
        }
    })
    

})
