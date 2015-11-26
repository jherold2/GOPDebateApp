rm(list=ls())

URLs <- c("http://time.com/3988276/republican-debate-primetime-transcript-full-text/",
          "http://time.com/4037239/second-republican-debate-transcript-cnn/",
          "http://time.com/4091301/republican-debate-transcript-cnbc-boulder/",
          "http://time.com/4107636/transcript-read-the-full-text-of-the-fourth-republican-debate-in-milwaukee/")
pollURL <- "http://www.realclearpolitics.com/epolls/2016/president/us/2016_republican_presidential_nomination-3823.html#polls"

debateDates <- as.Date(c("2015-08-06", "2015-09-16", "2015-10-28", "2015-11-10"))

# List of mods by debate ------
mods <- list(c("BAIER", "KELLY", "WALLACE"),
             c("TAPPER", "BASH", "HEWITT"),
             c("QUINTANILLA", "QUICK", "HARWOOD"),
             c("BAKER", "BARTIROMO", "CAVUTO")
)
# List of candidates by debate ----             
candidates <- list(c("TRUMP", "BUSH", "WALKER", "HUCKABEE", "CARSON", 
                     "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH"),
                   c("TRUMP", "BUSH", "WALKER", "HUCKABEE", "CARSON", 
                     "CRUZ", "RUBIO", "PAUL", "CHRISTIE", "KASICH", "FIORINA"),
                   c("TRUMP", "BUSH", "HUCKABEE", "CARSON", "CRUZ", 
                     "RUBIO", "PAUL", "CHRISTIE", "KASICH", "FIORINA"),
                   c("TRUMP", "BUSH", "CARSON", "CRUZ", 
                     "RUBIO", "PAUL", "KASICH", "FIORINA")
)

# read debate text in --------------------------------------------------------------  
readDebateText <- function(URLs) {  

  allDebates <- list()
  for(i in 1:length(URLs)) {
    #Scrape the html
    debate_text <- read_html(URLs[i]) %>% 
      html_nodes (".article-body") %>% 
      html_text()
    
    #Deletes end reference
    if(regexpr("CQ Transcriptions", debate_text) != -1) {
      debate_text <- substr(debate_text, 1, regexpr("CQ Transcriptions", debate_text)-1)
    }
    
    #Extract speaker
    Speaker <- unlist(str_extract_all(debate_text, "((?<=)[A-Z]{4,30}+(:| [A-Z]{4,10}:))|(?<=\n)[A-Z]{4,30}+ ")) 
    Speaker <- gsub(":| ", "", Speaker)
    #Extract statement
    Statement <- unlist(str_split(debate_text, "([A-Z]{4,30}+(:| [A-Z]{4,10}:))|(\n[A-Z]{5,10}(:| ))|PAUL "))
    Statement <- Statement[-1]
    
    debate_text <- data.frame(Speaker, Statement)
    
    allDebates[[i]] <- debate_text
  }
  return(allDebates)
}

# assign speaker identities --------------------------------------------------------------  
assignSpeakerIdentity <- function(debateList, mods, candidates) {
  for(i in 1:length(debateList)) {
    debateList[[i]]$SpeakerType <- rep("OTHER", nrow(debateList[[i]]))
    debateList[[i]]$SpeakerType <- ifelse(debateList[[i]]$Speaker %in% mods[[i]], "MODERATOR", debateList[[i]]$SpeakerType)
    debateList[[i]]$SpeakerType <- ifelse(debateList[[i]]$Speaker %in% candidates[[i]], "CANDIDATE", debateList[[i]]$SpeakerType)
    
    
    #Misspell in data
    if(length(which(as.character(debateList[[i]]$Speaker)=="FIONNA") != 0)) {
      misspell.ind <- which(debateList[[i]]$Speaker=="FIONNA")
      debateList[[i]]$Speaker[misspell.ind] <- "FIORINA"
      debateList[[i]]$SpeakerType[misspell.ind] <- "CANDIDATE"
    }
  }
  return(list(debateList, mods, candidates))
}

# creates DTM --------------------------------------------------------------  
getDTM <- function(data) {
 
  data <- VCorpus(VectorSource(data))

  # convert to lower case
  data <- tm_map(data, tolower)
  # remove punctuation
  data <- tm_map(data, removePunctuation)
  # remove numbers
  data <- tm_map(data, removeNumbers)
  # stemming
  data <- tm_map(data,stemDocument)
  # remove stop words
  data <- tm_map(data, removeWords, stopwords("en"))
  data <- tm_map(data, PlainTextDocument)
  data <- DocumentTermMatrix(data)
  return(data)
}

# adds audience responses --------------------------------------------------------------  
addDebateInfo <- function(df) {
  if(!(require(Hmisc))) install.packages("Hmisc"); library(Hmisc)
  #Extract all audience responses
  aud.responses <- unique(unlist(str_extract_all(df[,2], "[(][A-Z]{4,30}[)]")))
  #Get number of initial debates columns
  s.col <- ncol(df)
  #Go through audience responses and add a dummy column for each
  for(i in 1:length(aud.responses)) {
    df[grep(aud.responses[i], df$Statement),s.col+i] <- 1
    df[-grep(aud.responses[i], df$Statement),s.col+i] <- 0
    #Remove audience response from script
    df$Statement <- gsub(aud.responses[i], " ", df[,2])
    df$Statement <- gsub("[(] [)]", " ", df[,2], perl=T)
  }
  #Appropriately name new columns
  names(df)[(s.col+1):(s.col+length(aud.responses))] <- capitalize(tolower(gsub("[(]|[)]", "", aud.responses, perl=T)))
  
  #Add a column for number of stutters
  df$Stutters <- unlist(lapply(gregexpr("—", debates.df[,2]), function(x) mean(ifelse(x==-1, 0, length(x)))))
  df$Statement <- gsub("—", " ", df[,2])
  df$WordCount <- sapply(gregexpr("\\W+", df$Statement), length) + 1
  return(df)
}

# join all debate information --------------------------------------------------------------  
joinAllDebates <- function(debateList) {
  #Join debate list
  df <- do.call(rbind, debateList)
  rowFill.ind <- 1
  #Add Debate ID
  df$DebateID <- rep(NA, nrow(df))
  for(i in 1:length(debateList)) {
    df$DebateID[rowFill.ind:(rowFill.ind-1+nrow(debateList[[i]]))] <- rep(i, nrow(debateList[[i]]))
    rowFill.ind <- rowFill.ind + nrow(debateList[[i]])
  }
  return(df)
}

# aggregate information by speaker --------------------------------------------------------------  
aggSpeaker <- function(df) {
  sums <- aggregate(df[,5:ncol(df)], list(df[,1]), sum)
  numStatements <- aggregate(df[,1], list(df[,1]), length)
  allStatements <- aggregate(df$Statement, list(df$Speaker), paste0, collapse=" ")
  speakerType <- unique(df[,c(1,3)]); names(speakerType)[1] <- "Group.1"
  out <- list(allStatements, numStatements, speakerType,  sums)
  out <- Reduce(function(x,y) merge(x,y, by = "Group.1"), out)
  names(out)[c(1:3)] <- c("Speaker", "AllStatements", "NStatements")
  
  #Force out duplicate
  out <- out[-which(out$Speaker == "FIORINA" & out$SpeakerType == "OTHER"),]
  return(out)
}

# retrieve poll data from internet --------------------------------------------------------------  
getPollData <- function(pollURL) {
  poll_data <- read_html(pollURL) %>% 
    html_nodes ("td") %>% 
    html_text() 
  poll_data <- poll_data[-c(1:max(grep("\\.", poll_data)))]
  poll_cols <- read_html(pollURL) %>% 
    html_nodes ("th") %>% 
    html_text()
  poll_cols <- poll_cols[1:min(grep("Spread", poll_cols))]
  poll_data <- as.data.frame(matrix(poll_data, ncol=length(poll_cols), byrow = T))
  names(poll_data) <- poll_cols
  poll_data <- convertPollDates(poll_data)
  #Impute 0's where no poll data exists
  poll_data[,-which(names(poll_data) %in% c("Poll", "Date", "Spread", "From", "To"))] <-
    apply(poll_data[,-which(names(poll_data) %in% c("Poll", "Date", "Spread", "From", "To"))],
          2, function(x) ifelse(x=="--", 0, x))
  return(poll_data)
}

# adjusts poll data --------------------------------------------------------------  
convertPollDates <- function(df) {
  
  #Apply functions
  f1 <- function(s) strsplit(s, " - ")[[1]][1]
  f2 <- function(s) strsplit(s, " - ")[[1]][2]
  #Get from dates
  df$From <- as.Date(sapply(as.character(df$Date), f1), format = "%m/%d")
  #Assign correct year
  temp <- c(13, month(df$From[1:(length(df$From)-1)]))
  temp <- cumsum(ifelse(temp< month(df$From), 1, 0))
  df$From <- df$From - years(temp) 
  #Get to dates
  df$To <- as.Date(sapply(as.character(df$Date), f2), format = "%m/%d")
  #Assign correct year
  temp <- c(13, month(df$To[1:(length(df$To)-1)]))
  temp <- cumsum(ifelse(temp< month(df$To), 1, 0))
  df$To <- df$To - years(temp) 
  return(df)
}

# get sentiment score --------------------------------------------------------------  
sentimentScore <- function(sentences, vNegTerms, negTerms, posTerms, vPosTerms){
  final_scores <- matrix('', 0, 5)
  scores <- laply(sentences, function(sentence, vNegTerms, negTerms, posTerms, vPosTerms){
    initial_sentence <- sentence
    
    # remove unnecessary characters and split up by word 
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('\\d+', '', sentence)
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    
    # build vector with matches between sentence and each category
    vPosMatches <- match(words, vPosTerms)
    posMatches <- match(words, posTerms)
    vNegMatches <- match(words, vNegTerms)
    negMatches <- match(words, negTerms)
    
    # sum up number of words in each category
    vPosMatches <- sum(!is.na(vPosMatches))
    posMatches <- sum(!is.na(posMatches))
    vNegMatches <- sum(!is.na(vNegMatches))
    negMatches <- sum(!is.na(negMatches))
    score <- c(vNegMatches, negMatches, posMatches, vPosMatches)
    
    # add row to scores table
    newrow <- c(initial_sentence, score)
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, vNegTerms, negTerms, posTerms, vPosTerms)
  return(scores)
}

# get sentiments of all candidates --------------------------------------------------------------  
getSentiment <- function(debates.df) {
  
  debates.df <- debates.df %>% filter(SpeakerType == "CANDIDATE")
  # The AFINN wordlist found at http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010
  # which has 2477 words and phrases rated from -5 [very negative] to +5 [very positive]. 
  afinn_list <- read.delim(file='AFINN/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
  names(afinn_list) <- c('word', 'score')
  
  vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
  negTerms  <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], 
                 "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", 
                 "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", 
                 "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
  posTerms  <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], 
                 "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", 
                 "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", 
                 "unpretentious")
  vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", 
                 "fascinating", "dazzling", "legendary")
  
  posResult <- as.data.frame(sentimentScore(debates.df$Statement, vNegTerms, negTerms, posTerms, vPosTerms))
  posResult <- cbind(debates.df %>% select(-Statement), posResult) 
  colnames(posResult) <- c(names(debates.df)[-2], 'Statement', 'vNeg', 'neg', 'pos', 'vPos')
  posResult <- posResult %>%
    mutate(Statement = as.character(Statement),
           vNeg = as.numeric(vNeg),
           neg = as.numeric(neg),
           pos = as.numeric(pos),
           vPos = as.numeric(vPos))
  
  Agg_posResult <- posResult %>% select(vNeg, neg, pos, vPos, Speaker, WordCount) %>%
    group_by(Speaker) %>%
    select(-Speaker) %>% 
    summarise_each(funs(sum)) %>%
    mutate(Polar_Percentage = (vNeg + vPos)/WordCount,
           Positive_to_Negative_Ratio = (pos + vPos)/(vNeg + neg),
           Percentage_of_Very_Negative_Words = vNeg/WordCount,
           Percentage_of_Negative_Words = neg/WordCount,
           Percentage_of_Positive_Words = pos/WordCount,
           Percentage_of_Very_Positive_Words = vPos/WordCount
    ) 
  
  return(Agg_posResult)
}
# create daily poll levels and calculations --------------------------------------------------------------  
makePollTimeSeries <- function(df) {
  out <- data.frame(Date = seq(min(df$From), max(df$To), by=1))
  out[,2:(nrow(polls)+1)] <- matrix(rep(NA, nrow(out)*nrow(polls)), ncol=(nrow(polls)))
  
  candidates <- getNumericPollData(df)
  #Dummies for if each date is within poll day range
  for(i in 2:ncol(out)) {
    out[,i] <- ifelse(out$Date %in% seq(df$From[i-1], df$To[i-1], by=1), 1, 0)
  }
  pollsActivebyDate <- apply(out[,-1], 1, function(x) which(x==1))
  out <- as.data.frame(out$Date)
  out[,2:(ncol(candidates)+1)] <-  matrix(rep(NA, nrow(out)*ncol(candidates), ncol=(ncol(candidates))))
  names(out) <- c("Date", names(candidates))
  for(i in 1:nrow(out)) {
    #If no polling data for that date, just use previous poll averages
    if(length(pollsActivebyDate[[i]])==0) {
      out[i,2:ncol(out)] <- out[i-1,2:ncol(out)]
      #Get the average of all active polls
    } else out[i,2:ncol(out)] <- colMeans(as.data.frame(candidates[pollsActivebyDate[[i]],]))
  }
  return(out)
}

getNumericPollData <- function(df) {
  candidates <- df %>% select(-Poll, -Date, -Spread, -From, -To)  
  candidates <- as.data.frame(sapply(candidates,as.numeric)); names(candidates) <- names(df %>% select(-Poll, -Date, -Spread, -From, -To))
  return(candidates)
}

getDebateImpact <- function(dates, poll_data) {
  impact <- list()
  for(i in 1:length(dates)) {
    before <- seq(dates[i] - weeks(1), dates[i], by=1)
    after <- seq(dates[i] + weeks(1), dates[i] + weeks(4), by=1)
    
    data_before <- colMeans(poll_data %>% 
                              filter(Date %in% before) %>%
                              select(-Date))
    
    data_after <- colMeans(poll_data %>% 
                             filter(Date %in% after) %>%
                             select(-Date))
    
    impact[[i]] <- data_after-data_before
  }
  return(impact)
}

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# run helper functions for desired shiny output --------------------------------------------------------------  
debates <- readDebateText(URLs)
debates <- assignSpeakerIdentity(debates, mods, candidates)[[1]]

debates.df <- joinAllDebates(debates)
debates.df <- addDebateInfo(debates.df)

deb_by_speaker <- aggSpeaker(debates.df)
write.csv(deb_by_speaker, "debates.csv")
polls <- getPollData(pollURL)
series <- makePollTimeSeries(polls)
firstDate <- min(series$Date); lastDate <- max(series$Date)
impact <- getDebateImpact(debateDates, series)
impact <- do.call(rbind, impact)

candidateInfo <- deb_by_speaker %>% filter(SpeakerType=="CANDIDATE")
dtm <- getDTM(candidateInfo$AllStatements)
means.barplot <- qplot(x=Speaker, y=Stutters, 
                       data=candidateInfo)
                       
sentimentInfo <- getSentiment(debates.df)
