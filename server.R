# install packages --------------------------------------------------------------  
if(!(require(plyr))) install.packages("plyr"); library(plyr)
if(!(require(dplyr))) install.packages("dplyr"); library(dplyr)
if(!(require(ngram))) install.packages("ngram"); library(ngram)
if(!(require(ggplot2))) install.packages("ggplot2"); library(ggplot2)
if(!(require(rvest))) install.packages("rvest"); library(rvest)
if(!(require(stringr))) install.packages("stringr"); library(stringr)
if(!(require(tm))) install.packages("tm"); library(tm)
if(!(require(lubridate))) install.packages("lubridate"); library(lubridate)
if(!(require(reshape2))) install.packages("reshape2"); require(reshape2)

source("DebateScrape.R")

shinyServer(function(input, output) {
  
  agg_info <- reactive({
    candidateInfo %>% filter(Speaker %in% input$cands1)
  })
  
  ngrams <- reactive({
    ng <- ngram(as.character(agg_info() %>% select(AllStatements)), n = input$nWords)
    out <- get.phrasetable(ng)
    out[,3] <- round(out[,3], 7)
    names(out) <- c("Ngrams", "Frequency", "Proportion of Text")
    return(out)
  })
  
  graph_info <- reactive({
    candidateInfo[, c(1, which(names(candidateInfo) %in% input$Stat))] %>%
      filter(Speaker %in% input$cands2)
  })
  
  sentiment_info <- reactive({
    sentimentInfo[, c(1, which(names(sentimentInfo) %in% input$sent_scores))]  %>%
      filter(Speaker %in% input$cands3)
  })
  
  poll_data <- reactive({
    colNums <- match(input$cands4,sub(" ", "", toupper(names(series))))
    out <- series %>% filter(Date >= format(input$pollDates[1]),
                      Date <= format(input$pollDates[2])) 
    out <- out[,c(1,colNums)]
    out <- melt(out, id = "Date")
    names(out)[3] <- "Average Active Poll Points"
    return(out)
  })
  
  output$polls <- renderDataTable({
    ngrams()
  })
  
  output$desc_stat <- renderPlot({
    print(ggplot(graph_info(), aes_string(colnames(graph_info())[1], 
                                          colnames(graph_info())[2])) + 
            geom_bar(stat = "identity"))
  })
  
  output$sentimentPlot <- renderPlot({
    print(ggplot(sentiment_info(), aes_string(colnames(sentiment_info())[1], 
                                          colnames(sentiment_info())[2])) + 
            geom_bar(stat = "identity"))
  })
  
  output$pollPlot <- renderPlot({
    print(ggplot(poll_data(),
                 aes(x=Date,y=`Average Active Poll Points`,
                     colour=variable,group=variable)) + 
            geom_line())
  })
})