
# Define UI for dataset viewer application
shinyUI(fluidPage(
    # Application title
    titlePanel(em(strong("WordSeer", style = "color:#00008B; size=4"))),
    h4("Word Prediction Engine for the Coursera Data Science Specialization Capstone Project", style = "color:#202020 "),
    hr(style = "color:#00008B"),
    mainPanel(
        tabsetPanel(
            tabPanel("Prediction engine", 
                     fluidRow( 
                         column(5, 
                                br(),
                                strong("Enter phrase",  style = "color:#202020 "),
                                br(),
                                br(),
                                tags$textarea(id="wt_1", rows=5, cols=40, ""),
                                uiOutput("Dynamic"),
                                hr(style = "color:#00008B"),
                                strong("Processing time",  style = "color:#202020 "),
                                textOutput('time'),
                                strong("N-gram occurrences",  style = "color:#202020 "),
                                textOutput('tetracnts'),
                                textOutput('tricnts'),
                                textOutput('bicnts'),
                                textOutput('unicnts'),                                
                                br(),
                                strong('Developed by German Campuzano, 2015'),
                                br(),
                                a("Overview presentation", href="http://rpubs.com/gcampuzano14/coursera_capstone_wordseer"),
                                br(),
                                a("Source code - GitHub", href="http://www.google.com")
                                ),
                         column(2, offset = 1,
                                br(),
                                strong("Word weights",  style = "color:#202020 "),
                                br(),
                                br(),
                                uiOutput("matrix")
                                ),
                         column(3, offset = 1,
                                br(),
                                strong("Suggested input WordCloud",  style = "color:#202020 "),
                                plotOutput("plot"),
                                checkboxInput('a_wc', 'Uncheck to suppress wordcloud and increase speed', TRUE)
                         )
                         )
                     ), 
            tabPanel("App information", 
                     fluidRow( 
                         column(8, br(),
                                strong("The Underlying Algorithm"),
                                p("- The model was trained using a random sample of 5% from the HC corpora containing English documents from twitter, blogs and news feeds."),
                                p("- Data for each N-gram (range of N = 4 - 1) collection is stored in hash tables (R data.table  and spooky.32 for efficiency)."),
                                p("- WordSeer employs a  Markov process and is based on a modified 'Stupid Backoff' algorithm. Using all input N-grams (4-grams - 1-grams) it calculates the probability for all the words following that N-gram (maximum likelihood estimator or MLE). These probabilities are summed across all N-gram collections (1 to k) to obtain a weight for that word (W)."),
                                br(),
                                headerPanel(withMathJax("$$W_{word}=\\sum_{n=1}^kP(word|input_{ngram})_{n}$$")),
                                headerPanel(withMathJax("$$W_{word}=\\sum_{n=1}^k(MLE_{word})_{n}$$")),
                                br()
                         )
                         )
                     )
            )
        )
)
)
