shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel("2016 GOP Debate Trends"),
    tabsetPanel(
      tabPanel("Ngrams",
        sidebarLayout(
          sidebarPanel(
            helpText("Use this tab to see what words candidates tend to use together."),
            
            selectInput("cands1", 
                          label = "Select the candidates you would like to analyze:", 
                          choices =   c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                                "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                          selected = "TRUMP", 
                          multiple = T, 
                          selectize = TRUE, width = NULL, size = NULL
            ),
            
            sliderInput("nWords", 
                        label = "Select how many words you would like to group together:",
                        min = 2,
                        max = 10, 
                        value = 3
            )
            
        ),
        mainPanel(
          dataTableOutput("polls")
        )
      )
    ),
    tabPanel("Speaker Statistics", 
      sidebarLayout(
        sidebarPanel(
          helpText("Use this view various statistics of the candidates."),
             
          selectInput("cands2", 
                      label = "Select the candidates you would like to analyze:", 
                      choices =  c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                    "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                      selected = c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                    "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                      multiple = T, 
                      selectize = TRUE, width = NULL, size = NULL
          ),
          
          selectInput("Stat",
                      label = "Select a statistic of interest",
                      choices = names(candidateInfo[-c(1,2,4,6,12,13)]),
                      selected = "NStatements",
                      multiple = F, selectize = TRUE, width = NULL, size = NULL
          )
        ),
        mainPanel(
          plotOutput("desc_stat")
      )
    )
  ), 
  tabPanel("Sentiment", 
    sidebarLayout(
      sidebarPanel(
        selectInput("cands3", 
                    label = "Select the candidates you would like to analyze:", 
                    choices =  c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                 "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                    selected = c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                 "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                    multiple = T, 
                    selectize = TRUE, width = NULL, size = NULL
        ),
        
        selectInput("sent_scores",
                    label = "Select the sentiment score of interest:",
                    choices = names(sentimentInfo[-1]),
                    selected = "pos",
                    multiple = F, selectize = TRUE, width = NULL, size = NULL
        )
      ),
      mainPanel(
        plotOutput("sentimentPlot")
      )
    )
  ),
  tabPanel("Poll Performance", 
    sidebarLayout(
      sidebarPanel(
        dateRangeInput("pollDates", 
                    label = "Select the poll timeframe of interest:",
                    start = Sys.Date() - years(1),
                    end = lastDate,
                    min = firstDate,
                    max = lastDate
        ),
        
        selectInput("cands4", 
                    label = "Select the candidates you would like to analyze:", 
                    choices =  c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                 "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                    selected = c("TRUMP", "BUSH", "FIORINA", "HUCKABEE", "CARSON", 
                                 "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"), 
                    multiple = T, 
                    selectize = TRUE, width = NULL, size = NULL
        )
      ),
      mainPanel(
        plotOutput("pollPlot")
      )
    )
  ),
  tabPanel("About",
           sidebarLayout(
             sidebarPanel(
              helpText("All analysis is based off of the first 4 GOP debates for the 2016 
                    presidential election.  Transcripts were web scraped from Time magazine's
                    website. Poll data was sourced from realclearpolitics.com.  Any questions about this
                    product should be directed to John Herold at johnherold5@gmail.com.")
            ),
            mainPanel()
          )

  )
)
))
