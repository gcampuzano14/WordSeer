## LOAD REQUIRED LIBRARIES
rm(list=ls())
libs <- c('RWeka', 'tm', 'data.table', 'stringr', 'hashFunction', 'parallel')
lapply(libs, suppressPackageStartupMessages(require), character.only = TRUE)

#Preparing the parallel cluster using the cores
gc(reset=T)
cl <- makeCluster(detectCores()-2)
invisible(clusterEvalQ(cl, library(tm)))
invisible(clusterEvalQ(cl, library(RWeka)))
options(mc.cores=1)

# SAMPLING________________
sampleFile <- function(fileIn, fileOut, testOut){
    set.seed(6666)
    print(fileIn)
    conIn  <- file(fileIn, 'rb', blocking=FALSE, encoding = "UTF-8")
    conOut <- file(fileOut, 'w')
    corp <- readLines(conIn, skipNul = TRUE,  warn = F, encoding = "UTF-8")
    n <- length(corp)
    prob <- 0.05
    set.seed(6666)
    
    subset_in<-as.logical(rbinom(n,1,prob))
    corp_out<-corp[subset_in]
    #corp_out<-sample(corp, n*prob, replace = F, prob = NULL)
    #print(corp_out)
    cor_test<-corp[-subset_in][as.logical(rbinom(length(corp[-subset_in]),1,0.02))]
     
    writeLines(corp_out, conOut) 
    writeLines(cor_test, testOut) 
    
    close(conIn)
    close(conOut)  
}

sampleDir <- file.path('raw_data','samples')
testDir <- file.path('raw_data','test')
rawDataDir   <- file.path('raw_data')
rawDataFiles <- list.files(path=rawDataDir, pattern='*.txt', full.names=T, recursive=FALSE)
rawDataFilenames  <- unlist(lapply(rawDataFiles, basename))
sampleFiles <- file.path(sampleDir, paste0('sample_', rawDataFilenames))
testFiles <- file.path(testDir, paste0('test_', rawDataFilenames))
dir.create(file.path('.', sampleDir))  
dir.create(file.path('.', testDir))  
sapply(1:3, function(x) sampleFile(rawDataFiles[x], sampleFiles[x], testFiles[x]))

# CREATE CORPUS
full_corp<-Corpus(DirSource(sampleDir), 
                  readerControl = list(language='en_US', encoding ='UTF-8'))

#CLEAN THE CORPUS
badwords <- scan('badwords.txt', '')
bad_marks <- function(x) gsub('[^[:space:][:alnum:]]','',x)# THIS REMOVES PUNCTUATION ALSO
bad_letters <- function(x) gsub('[^[a-z|A-Z|[:space:]]','',x)# REMOVE NON ENGLISH CHARACTERS
full_corp <- tm_map(full_corp, content_transformer(bad_marks))
full_corp <- tm_map(full_corp, content_transformer(bad_letters))
#full_corp <- tm_map(full_corp, removeWords, badwords)
full_corp <- tm_map(full_corp, removeNumbers) 
full_corp <- tm_map(full_corp, stripWhitespace)
full_corp <- tm_map(full_corp, content_transformer(tolower))

# DEFINE TOKENIZER FUNCTIONS
delimiters = ' '
unigramTokenizer <- function(x) NGramTokenizer(
    x, Weka_control(min = 1, max = 1, delimiters = delimiters))
bigramTokenizer <- function(x) NGramTokenizer(
    x, Weka_control(min = 2, max = 2, delimiters = delimiters))
trigramTokenizer <- function(x) NGramTokenizer(
    x, Weka_control(min = 3, max = 3, delimiters = delimiters))
tetragramTokenizer <- function(x) NGramTokenizer(
    x, Weka_control(min = 4, max = 4, delimiters = delimiters))

## CREATE TDMs FOR EACH N-GRAM SIZE
unigramTdm <- TermDocumentMatrix(full_corp, control = list(tokenize = unigramTokenizer))
bigramTdm <- TermDocumentMatrix(full_corp, control = list(tokenize = bigramTokenizer))
trigramTdm <- TermDocumentMatrix(full_corp, control = list(tokenize = trigramTokenizer))
tetragramTdm <- TermDocumentMatrix(full_corp, control = list(tokenize = tetragramTokenizer))

## CREATE HASH
# UNIGRAM HASH
dt_unigrams = data.table(ngram_in=character(), ngram_number = numeric(), 
                         word_out=character(), word_number=numeric(), 
                         ngram_freq=numeric())
patt_in  <- function(x) str_trim(str_extract(x, '^(\\S+)'))
hash_fx <- function(x) as.numeric(spooky.32(x))
word_out <- sapply(as.vector(unigramTdm$dimnames$Terms), patt_in)
ngram_in <- word_out
ngram_number <- sapply(as.vector(ngram_in), hash_fx)
word_number <- ngram_number
ngram_counts <- as.vector(rowSums(as.matrix(unigramTdm))) 
dt = data.table(ngram_in=ngram_in, 
                ngram_number = ngram_number, 
                word_out=word_out, 
                word_number=word_number, 
                ngram_freq=ngram_counts)
dt<-dt[ngram_freq>1,]# ELIMINATE AL TERMS WITH A SINGLE (1) OCCURRENCE
file<-'unigram_hash.rds'
print(file)
saveRDS(dt, file)
rm(dt)
gc(reset=T)

# HIGHER LEVEL NGRAM HASHES
the_input = list(bigram=list(tdm=bigramTdm,
                             patt_in='^(\\S+\\s+)',
                             file_name='bigram_hash.rds'),
                 trigram=list(tdm=trigramTdm,
                             patt_in='^((\\S+\\s+){2})',
                             file_name='trigram_hash.rds'),
                 tetragram=list(tdm=tetragramTdm,
                             patt_in='^((\\S+\\s+){3})',
                             file_name='tetragram_hash.rds'))
for (e in the_input){
    patt_in  <- function(x) str_trim(str_extract(x, e$patt_in))
    patt_out <- function(x) str_trim(str_extract(x, '(\\S+)$'))
    hash_fx <- function(x) as.numeric(spooky.32(x))
    ngram_in <- sapply(as.vector(e$tdm$dimnames$Terms), patt_in)
    word_out <- sapply(as.vector(e$tdm$dimnames$Terms), patt_out)
    ngram_number <- sapply(as.vector(ngram_in), hash_fx)
    word_number <- sapply(as.vector(word_out), hash_fx)
    ngram_counts <- as.vector(rowSums(as.matrix(e$tdm))) 
    
    dt = data.table(ngram_in=ngram_in, 
                    ngram_number = ngram_number, 
                    word_out=word_out, 
                    word_number=word_number, 
                    ngram_freq=ngram_counts)
    
    print(e$file_name)
    saveRDS(dt, e$file_name)
    rm(dt)
    gc(reset=T)
}

# END & CLEAN ALL
rm(list=ls())
# unigram_dt<-readRDS(file.path('app_data', 'unigram_hash.rds'))
# bigram_dt<-readRDS(file.path('app_data', 'bigram_hash.rds'))
# trigram_dt<-readRDS(file.path('app_data', 'trigram_hash.rds'))
tetragram_dt<-readRDS(file.path('tetragram_hash.rds'))
