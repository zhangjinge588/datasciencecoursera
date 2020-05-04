
require(NLP)
library(tm)
library(slam)
library(RWeka)
library(dplyr)
library(lsa)
Sys.setenv(TZ="GMT")

set.seed(1994-01-10)


work_directory = "~/Downloads"
data_news     = readLines(paste0(work_directory, "/final/en_US/en_US.news.txt"), skipNul = TRUE, n=-1L)
data_blogs    = readLines(paste0(work_directory, "/final/en_US/en_US.blogs.txt"), skipNul = TRUE, n=-1L)
data_twitter  = readLines(paste0(work_directory, "/final/en_US/en_US.twitter.txt"), skipNul = TRUE, encoding = "UTF16LE", n=-1L)
# data_glove    = data.table::fread('glove.6B.300d.txt', data.table = F,  encoding = 'UTF-8') 
# colnames(data_glove) = c('word',paste('dim',1:300,sep = '_'))


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

# Generate 1-gram, 2-gram, ..., n-gram
generateNGrams <- function (corpus, n) {
  
  result = list()
  for (i in 1:n) {
    print (paste("generate ", i, "gram"))
    result[[i]] = DocumentTermMatrix(corpus, control = list(tokenize=function(x) NGramTokenizer(x, Weka_control(min=i, max=i))))
  }
  
  result
  
}

generateNGramsFreq <- function (dtm_list) {
  result = list()
  for (i in 1:length(dtm_list)) {
    temp_word_frequency = col_sums(dtm_list[[i]], na.rm=TRUE)
    result[[i]] = temp_word_frequency[order(temp_word_frequency, decreasing = TRUE)]
  }
  
  result
  
}

pruneNGramsFreq <- function (word_frequency_list, threshold_list) {
  for (i in 1:length(word_frequency_list)) {
    word_frequency_list[[i]] = word_frequency_list[[i]][word_frequency_list[[i]] > threshold_list[[i]]]
  }
  word_frequency_list
}

combine_scores <- function(main_list, list_to_combine, combine_strategy='+') {
  
  names_main_list = names(main_list)
  names_list_to_combine = names(list_to_combine)
  
  # print ("main_list")
  # print (main_list)
  # print ("list_to_combine")
  # print (list_to_combine)
  # print ("############")
  
  for (i in 1:length(names_list_to_combine)) {
    
    temp_name = names_list_to_combine[i]
    
    if ((length(names_main_list) > 0) && (temp_name %in% names_main_list)) {
      a = main_list[[temp_name]] 
      b = list_to_combine[[temp_name]][names(a)]
      
      if (combine_strategy == "+") {
        b[is.na(b)] = 0.0
        temp_df = rbind(a, b[names(a)])
        temp_sum = col_sums(temp_df)
        main_list[[temp_name]] = temp_sum
        
      } else if (combine_strategy == "merge") {
        
        main_list[[temp_name]] = c(main_list[[temp_name]], list_to_combine[[temp_name]])
      } else {
        print ("Unknown combine_strategy... Will return the first list as is.")
      }
      
    } else {
      
      main_list[[temp_name]] = list_to_combine[[temp_name]]
    }
  }
  
  main_list
  
}

stupid_backoff = function(input_vector, word_frequency_list, backoff_unigram, topN=5, lambda=0.4) {
  
  # we need to start with n-gram prefix.
  # e.g. We have 4-gram word frequency but input_vector has length 2, then we have to start with bigram prefix.
  # e.g. We have 4-gram word frequency but input_vector has length 10, then we have to start with trigram prefix.
  word_length = length(input_vector)
  n = min(length(word_frequency_list)-1, word_length)
  
  length_unigrams = length(backoff_unigram)
  
  if (n == 0) {
    backoff_unigram[1:topN]
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
    
    # print (score_df[index,])
    
    score_df[index,c("word", "unigram_score", "score", 
                     "find_n_gram", "n_gram", "n_gram_freq", "n_minus_1_gram", "n_minus_1_gram_freq", "discount")]
    
    # final_score
  }
  
  
}

# Score the output in a single corpus.
score_single_corpus = function(output,word_frequency_list,backoff_unigram,topN=5) {
  
  output_corpus = create_corpus(output)
  output_after_tokenization = as.character(sapply(output_corpus, function(x) trimws(x$content)))
  
  final_result = lapply(output_after_tokenization, function(x) {
    
    x_list = strsplit(x, " ")[[1]]
    
    print (x)
    # print (x_list)
    print ("###############")
    
    result = stupid_backoff(x_list,word_frequency_list,backoff_unigram,topN)
    
    lambda = 0.01
    
    # for (i in 1:(length(x_list)-1)) {
    # 
    #   temp_prefix = paste(x_list[1:(length(x_list)-i)], collapse = " ")
    #   print (temp_prefix)
    # 
    #   temp_result = stupid_backoff(temp_prefix, word_frequency_list, backoff_unigram,topN, lambda)
    # 
    #   lambda = lambda * 0.01
    # 
    #   # print ("temp_result in lapply")
    #   # print (temp_result)
    #   # print ("##############")
    # 
    #   result = combine_scores(result,
    #                           temp_result,
    #                           "+")
    # 
    # }
    # result[order(result, decreasing = TRUE)][1:topN]
    result
    
  })
  
  names(final_result) = output
  
  final_result
  
}

multiple_choice <- function(problem, options, word_frequency_list,backoff_unigram,topN=5) {
  result = score_single_corpus(problem,word_frequency_list,backoff_unigram,topN)
  final_result = list()
  for (i in 1:length(problem)) {
    final_result[[i]] = result[[problem[i]]][options[[i]],]
    final_result[[i]] = final_result[[i]][order(final_result[[i]]['score'], decreasing = TRUE),]
    
    
  }
  
  names(final_result) = problem
  final_result
}





data           = c(data_news, data_blogs, data_twitter)

# corpus         = create_corpus(data, "en")
# saveRDS(corpus, file = "~/Desktop/corpus.rds")
corpus         = readRDS("~/Desktop/corpus.rds")

# dtm_list = generateNGrams(corpus, 3)
# saveRDS(dtm_list, file = "~/Desktop/dtm_list.rds")
dtm_list = readRDS("~/Desktop/dtm_list.rds")

# word_freq = generateNGramsFreq(dtm_list)
# saveRDS(word_freq, file = "~/Desktop/word_freq.rds")
word_freq = readRDS("~/Desktop/word_freq.rds")

threshold_list = list(2,3,4)

filtered_word_freq_sample = pruneNGramsFreq(word_freq,threshold_list)

saveRDS(filtered_word_freq_sample, "~/Desktop/filtered_word_freq.rds")

# Compute Backoff Score for Unigram. e.g. Count(unigram) / Number of Unigrams.
backoff_unigram_sample = filtered_word_freq_sample[[1]] / length(filtered_word_freq_sample[[1]])
# Elinimate stopwords.
backoff_unigram_sample_remove_stop_words = backoff_unigram_sample[!names(backoff_unigram_sample) %in% stopwords()]

saveRDS(backoff_unigram_sample_remove_stop_words, "~/Desktop/unigram_probabilities.rds")



quiz2 <- c("The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
           "You're the reason why I smile everyday. Can you follow me please? It would mean the",
           "Hey sunshine, can you follow me and make me the",
           "Very early observations on the Bills game: Offense still struggling but the",
           "Go on a romantic date at the",
           "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
           "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
           "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
           "Be grateful for the good times and keep the faith during the",
           "If this isn't the cutest thing you've ever seen, then you must be")

quiz2_choices <- list(c("cheese", "soda", "beer", "pretzels"), 
                c("world", "universe", "best", "most"),
                c("saddest", "smelliest", "happiest", "bluest"),
                c("crowd", "defense", "players", "referees"),
                c("beach", "mall", "grocery", "movies"),
                c("way", "phone", "horse", "motorcycle"),
                c("thing", "weeks", "years", "time"),
                c("fingers", "ears", "eyes", "toes"),
                c("sad", "hard", "bad", "worse"),
                c("asleep", "insensitive", "callous", "insane"))


quiz3 <- c("When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd",
           "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his",
           "I'd give anything to see arctic monkeys this",
           "Talking to your mom has the same effect as a hug and helps reduce your",
           "When you were in Holland you were like 1 inch away from me but you hadn't time to take a",
           "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the",
           "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each",
           "Every inch of you is perfect from the bottom to the",
           "I’m thankful my childhood was filled with imagination and bruises from playing",
           "I like how the same people are in almost all of Adam Sandler's")

quiz3_choices <- list(c("die", "give", "sleep", "eat"),
                      c("spiritual","marital", "financial", "horticultural"),
                      c("weekend", "decade", "morning", "month"),
                      c("hunger", "sleepiness", "stress", "happiness"),
                      c("look", "picture", "minute", "walk"),
                      c("incident", "account", "matter", "case"),
                      c("finger", "arm", "toe", "hand"),
                      c("top", "center", "side", "middle"),
                      c("daily", "inside", "outside", "weekly"),
                      c("movies", "pictures", "stories", "novels"))

# print (score_single_corpus(quiz2,filtered_word_freq_sample,backoff_unigram_sample_remove_stop_words,50))
# 
# print (multiple_choice(quiz2, quiz2_choices, filtered_word_freq_sample,backoff_unigram_sample_remove_stop_words,5000))
# 
# print (multiple_choice(quiz3[1:2], quiz3_choices[1:2], filtered_word_freq_sample,backoff_unigram_sample_remove_stop_words,length(backoff_unigram_sample_remove_stop_words)))

solution_quiz3_stupid_backoff = multiple_choice(quiz3,
                                                quiz3_choices, 
                                                filtered_word_freq_sample,
                                                backoff_unigram_sample_remove_stop_words,
                                                length(backoff_unigram_sample_remove_stop_words))


input = c("today is ")

print (score_single_corpus(input, filtered_word_freq_sample, backoff_unigram_sample_remove_stop_words, 10))






