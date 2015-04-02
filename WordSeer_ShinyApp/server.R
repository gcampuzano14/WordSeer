#runApp()
# libs <- c('NLP','slam', 'tm','data.table', 'RColorBrewer', 'wordcloud','xtable', 
#           'stringr','hashFunction','parallel','shiny')
# lapply(libs, require, character.only = TRUE)

library('NLP')
library('slam')
library('tm')
library('data.table')
library('RColorBrewer')
library('wordcloud')
library('xtable')
library('stringr')
library('hashFunction')
library('parallel')
library('shiny')

source('word_compilator.R')


# Define server logic required to summarize and view the selected dataset
shinyServer(
    function(input, output, session) {
        
        progress <- shiny::Progress$new(session, min=1, max=10000)
        on.exit(progress$close())
        
        progress$set(message = 'Loading precious data',
                     detail = 'Being patient pays off...')
        
        for (i in 1:10000) {
            progress$set(value = i)
            #Sys.sleep(0.5)
        }
        
        load_data()
        
        dvalues <- reactive({
        
        

            input_str = input$wt_1
            ptm <- proc.time()
            my_data <- get_words_full(input_str)
            merged <- my_data[[1]]
            awc<<-input$a_wc
            len_terta <- my_data[[3]]
            len_tri <- my_data[[4]]
            len_bi <- my_data[[5]]
            len_uni <- my_data[[6]]
            merged_full <<- my_data[[2]]
            proc.time() - ptm
            sys_time <<- (ptm["sys.self"])
            a=1
            for (e in names(input)){                
                input_test = unlist(gregexpr('labels_', e))
                if (input_test == -1){
                    next
                } 
                if (input[[e]][1] == 1){
                    new_word=merged[a, Word]
                    input_str <- sub("\\s+$", "", input_str)
                    the_sentense<-paste(input_str, new_word, sep=' ')
                    updateTextInput(session, "wt_1", value = the_sentense)
                    break
                }
                a=a+1
            }

            return(list(merged, merged_full))   
        })
        
        output$Dynamic <- renderUI({
            merged<-dvalues()[[1]]
            out_words <- vector("list") 
            a=10
            for(i in row(merged)[,1]){
                labels<-merged[i,][, Word]
                out_words[[i]] <- list(actionButton(
                    inputId = paste0('labels_',as.character(a)), label = labels, value = labels,
                    style = "background-color:black;color:white;width:6em"))
                a=a+1
            } 
            return(out_words)
        })
    
        output$matrix <- renderTable({
            merged<-dvalues()[[1]]
            xtable(merged)
        }, include.rownames = FALSE, sanitize.text.function = function(x) x)
        
        
        output$time <- renderText({
            dvalues()
            return(paste0(sys_time, ' seconds'))
        })
        
        output$tetracnts <- renderText({
            dvalues()
            return(paste0('4-grams: ', len_terta, ' occurrences'))
        })
        
        output$tricnts <- renderText({
            dvalues()
            return(paste0('3-grams: ', len_tri, ' occurrences'))
        })
        
        output$bicnts <- renderText({
            dvalues()
            return(paste0('2-grams: ', len_bi, ' occurrences'))
        })
        
        output$unicnts <- renderText({
            dvalues()
            return(paste0('1-grams: ', len_uni, ' occurrences'))
        })
    
#     output$the_input <- renderText({
#         dvalues()
#         return(as.character(the_input))
#     })
#     
#     

        output$plot <- renderPlot({
            merged_full<-dvalues()[[2]]
            if (awc == TRUE){
            wordcloud(merged_full[,word_out], merged_full[,sum_prob], 
                      min.freq = 0.001, max.words=50,
                      colors=brewer.pal(8, "Dark2"))
            }
        })
        

    })