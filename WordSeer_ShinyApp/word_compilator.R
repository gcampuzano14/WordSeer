
load_data <- function(){
    unigram_dt<<-readRDS(file.path('app_data', 'unigram_hash.rds'))
    bigram_dt<<-readRDS(file.path('app_data', 'bigram_hash.rds'))
    trigram_dt<<-readRDS(file.path('app_data', 'trigram_hash.rds'))
    tetragram_dt<<-readRDS(file.path('app_data', 'tetragram_hash.rds'))
}


# sum(object.size(unigram_dt), object.size(bigram_dt), object.size(trigram_dt), object.size(tetragram_dt))/sum(dim(unigram_dt)[1], dim(bigram_dt)[1], dim(trigram_dt)[1], dim(tetragram_dt)[1] )

get_words_full <- function(input_str){
    #input_str<-c('v a couple of')
    
    # GET REVERSED VECTOR OF INPUT WORDS
    #input_str='hthe house,  is on '
    input_str_choped<-gsub("[^[:space:][:alnum:]]", '', input_str)
    input_str_choped<-stripWhitespace(input_str_choped)
    input_str_choped <- tolower(input_str_choped)
    word_vector<-rev(str_match_all(input_str_choped, "\\S+")[[1]])
    #print(word_vector)
    
    # CONVERT INPUT N-GRAM HEAD TO NUMERIC KEY WITH SPOOKY.32 
    tetra_in<-as.numeric(spooky.32(paste(paste(word_vector[3], word_vector[2], word_vector[1]))))
    tri_in<-as.numeric(spooky.32(paste(paste(word_vector[2], word_vector[1]))))
    bi_in<-as.numeric(spooky.32(paste(paste(word_vector[1]))))
    
    # GET ALL ENTRIES WHERE N-GRAM HEAD EXISTS IN N-GRAM HASHES - SUBSETTING BY NUMERIC KEY
    tetra_subset = tetragram_dt[ngram_number==tetra_in, .(word_out, ngram_freq)]
    tri_subset = trigram_dt[ngram_number==tri_in, .(word_out, ngram_freq)]
    bi_subset = bigram_dt[ngram_number==bi_in, .(word_out, ngram_freq)]
    
    # ORDER RETURNED ROWS BY FREQUENCY OF OCCURRENCE
    tetra_subset<-tetra_subset[order(-ngram_freq)]
    tri_subset<-tri_subset[order(-ngram_freq)]
    bi_subset<-bi_subset[order(-ngram_freq)]
    unigram_dt<-unigram_dt[order(-ngram_freq)]
    
    
    len_terta <<- dim(tetra_subset)[1]
    len_tri <<- dim(tri_subset)[1]
    len_bi <<- dim(bi_subset)[1]
    len_uni <<- dim(unigram_dt)[1]
    
    # NORMALIZE COUNTS BY FREQUENCY OF MATCHING NGRAMS -GET PROBABILITIES - MLEs
    # FIX ENCODING
    ngram_list<-list('tetra_subset'=tetra_subset, 
                     'tri_subset'=tri_subset, 
                     'bi_subset'=bi_subset)
    rm(tetra_subset, tri_subset, bi_subset)
    for (e in names(ngram_list)){
        ngram_freq_vect = ngram_list[[e]][,ngram_freq]
        ngram_list[[e]][, paste0('prob_', e ):=round(ngram_freq/sum(ngram_freq_vect),6)] 
        Encoding(ngram_list[[e]]$word_out)<-'unknown'
    }
    
    ngram_freq_vect = unigram_dt[,ngram_freq]
    unigram_dt[, prob_uni:=round(ngram_freq/sum(ngram_freq_vect),6)]
    Encoding(unigram_dt$word_out)<-'unknown'
    
    # MERGE WORDS TO SINGLE DT
    tbls = list(ngram_list[['tetra_subset']],
                ngram_list[['tri_subset']],
                ngram_list[['bi_subset']],
                unigram_dt)
    lapply(tbls, function(i) setkey(i, 'word_out'))
    merged = Reduce(function(...) merge(..., all = T), tbls) 
    
    for (i in seq_along(merged)) set(merged, i=which(is.na(merged[[i]])), j=i, value=0)# REPLACE NA WITH 0
    
    merged = merged[ , sum_prob := round(sum(prob_tetra_subset, 
                                       prob_tri_subset, 
                                       prob_bi_subset, 
                                       prob_uni),5), by=1:NROW(merged)]

    merged_long = merged[order(-sum_prob)][1:200]# ORDER AND ONLY DYSPLAY TO 30 CHOICES
    merged_short = merged[order(-sum_prob)][1:30]# ORDER AND ONLY DYSPLAY TO 30 CHOICES
    
    #REMANE COLUMNS
    setnames(merged_short, "sum_prob", "Weight")
    setnames(merged_short, "word_out", "Word")
    merged_short <- merged_short[, Weight, Word]
    return(list(merged_short, merged_long[, sum_prob, word_out], len_terta, len_tri, len_bi, len_uni))
    
}
