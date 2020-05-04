#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)

create_corpus <- function(data, language='en') {
    
    my_corpus <- VCorpus(VectorSource(data))
    
    my_corpus <- tm_map(my_corpus, content_transformer(tolower))
    
    # my_corpus <- tm_map(my_corpus, stripWhitespace)
    
    # my_corpus <- tm_map(my_corpus, function(x) stemDocument(x, language))
    
    # my_corpus <- tm_map(my_corpus, removeWords, stopwords(language))
    
    my_corpus <- tm_map(my_corpus, removeNumbers)
    
    my_corpus <- tm_map(my_corpus, content_transformer(function(x) gsub("http.*", "", x, ignore.case = TRUE)))
    
    my_corpus <- tm_map(my_corpus, function(x) removePunctuation(x, ucp=TRUE, preserve_intra_word_contractions=TRUE))
    
    my_corpus <- tm_map(my_corpus, content_transformer(function(x) gsub("[^[:alnum:][:blank:]+?&/\\-]", "", x)))
    
    my_corpus <- tm_map(my_corpus, content_transformer(function(x) gsub(" *\\b[[:alpha:]]{1,1}\\b *", " ", x)))
    
    my_corpus <- tm_map(my_corpus, content_transformer(function(x) gsub("[^A-Za-z]", " ", x)))
    
    my_corpus <- tm_map(my_corpus, stripWhitespace)
    
    my_corpus
    
}

stupid_backoff = function(input_vector, word_frequency_list, backoff_unigram, topN=5, lambda=0.4) {
    
    # we need to start with n-gram prefix.
    # e.g. We have 4-gram word frequency but input_vector has length 2, then we have to start with bigram prefix.
    # e.g. We have 4-gram word frequency but input_vector has length 10, then we have to start with trigram prefix.
    word_length = length(input_vector)
    n = min(length(word_frequency_list)-1, word_length)
    
    length_unigrams = length(backoff_unigram)
    
    if (n == 0) {
        names(backoff_unigram[1:topN])
    } else {
        # Initialize final score to be 1.
        score_df = data.frame(word=names(backoff_unigram),
                              unigram_score = backoff_unigram,
                              score=replicate(length_unigrams, 1),
                              computed=replicate(length_unigrams, FALSE),
                              find_n_gram=replicate(length_unigrams, 1))
        
        discount = 1
        
        while (n >= 1) {
            
            prefix = paste(input_vector[(word_length-n+1):word_length], collapse = " ")
            # print (n)
            # print (prefix)
            # print ("###########")
            if (prefix %in% names(word_frequency_list[[n]])) {
                
                count_prefix = word_frequency_list[[n]][prefix]
                
                score_df['temp_word'] = sapply(score_df['word'], function (x) paste(prefix, x))
                
                # will return na if not in word frequency list
                score_df['temp_score'] = sapply(score_df['temp_word'], function (x) word_frequency_list[[n+1]][x])
                
                needs_compute = score_df['computed'] == FALSE & !is.na(score_df['temp_score'])
                score_df[needs_compute,'score'] = discount / count_prefix  * score_df[needs_compute, 'temp_score']
                
                # Set the words after computing the score to be computed.
                score_df[needs_compute, 'find_n_gram'] = n+1
                score_df[needs_compute, 'n_gram'] = score_df[needs_compute, 'temp_word']
                score_df[needs_compute, 'n_gram_freq'] = score_df[needs_compute, 'temp_score']
                score_df[needs_compute, 'n_minus_1_gram'] = prefix
                score_df[needs_compute, 'n_minus_1_gram_freq'] = count_prefix
                score_df[needs_compute, 'discount'] = discount
                score_df[needs_compute, 'computed'] = TRUE
                
                
            }
            
            n = n - 1
            discount = discount * lambda
            
        }
        
        # Remaining uncomputed words will be replaced with unigram score * discount.
        score_df[score_df['computed']==FALSE, 'score'] = discount * 0.000001 * score_df[score_df['computed']==FALSE, 'unigram_score']
        
        index = order(score_df$score, decreasing = TRUE)[1:topN]
        # final_score = score_df[index,'score']
        # names(final_score) = score_df[index,'word']
        
        # score_df[index,c("word", "unigram_score", "score",
        #                  "find_n_gram", "n_gram", "n_gram_freq", "n_minus_1_gram", "n_minus_1_gram_freq", "discount")]
        
        score_df[index,c("word")]
        
        
    }
    
    
}

score_input = function(input,word_frequency_list,backoff_unigram,topN=5,lambda=0.4) {
    
    input_corpus = create_corpus(input)
    input_after_tokenization = trimws(input_corpus[[1]]$content)
    
    stupid_backoff(strsplit(input_after_tokenization, " ")[[1]],word_frequency_list,backoff_unigram,topN,lambda)
}

filtered_word_freq_sample = readRDS("filtered_word_freq.rds")

backoff_unigram_sample_remove_stop_words = readRDS("unigram_probabilities.rds")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    result = eventReactive(input$submit, {
        score_input(input$textInput, 
                    filtered_word_freq_sample, 
                    backoff_unigram_sample_remove_stop_words, 
                    topN=input$topN, 
                    lambda=input$discount)
        
    })
    
    output$value <- renderTable({
        result()
        
    }, colnames = FALSE, rownames = TRUE)
   

})
