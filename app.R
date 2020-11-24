if(require(shiny)){
  
  library(tidyverse)
  library(flexdashboard)
  library(shiny)
  library(wordcloud2)
  library(tm)
  library(htmlwidgets)
  library(htmltools)
  library(webshot)
  library(rsconnect)
  
  set.seed(1)
  # Global variables can go here
  trump_df = 
    merge(
      read_csv("./datasets/trump1.csv"),
      read_csv("./datasets/trump2.csv"),
      all = TRUE
    ) %>%
    select(!X1) %>%
    mutate(hashtag = "Trump")
  
  biden_df = 
    merge(
      read_csv("./datasets/biden1.csv"),
      read_csv("./datasets/biden2.csv"),
      all = TRUE
    ) %>%
    select(!X1) %>%
    mutate(hashtag = "Biden")
  
  tweets_usa =
    merge(biden_df, trump_df, all = TRUE) %>% 
    filter(country == "United States of America")
  
  election_df =
    read_csv("./datasets/president_county_candidate.csv") %>% 
    group_by(state, party) %>% 
    mutate(party_total = sum(total_votes)) %>% 
    ungroup() %>% 
    group_by(state) %>%
    mutate(state_winner = case_when(
      party_total == max(party_total) ~ TRUE,
      party_total != max(party_total) ~ FALSE),
      state_total = sum(total_votes)
    )
  
  state_election_df = 
    election_df %>% 
    filter(state_winner == TRUE) %>% 
    select(state, candidate, party, party_total, state_total) %>% 
    distinct()
  
  main_tweets_usa = 
    left_join(tweets_usa, state_election_df, by = "state") %>% 
    rename(
      winner_candidate = candidate,
      winner_party = party
    ) %>% 
    select(tweet, hashtag, winner_party)

  wordcount_df = function(df) {
    
    if (!is.data.frame(df)) {
      stop("Input must be a dataframe")
    }
    
    df_text = 
      df %>% 
      slice_head(n = 1000) %>% 
      select(tweet) %>%
      mutate(
        tweet = gsub("#[[:alpha:]]*", "", tweet),
        tweet = gsub("@[[:alpha:]]*", "", tweet),
        tweet = gsub("https\\S*", "", tweet),
        tweet = gsub("http\\S*", "", tweet),
        tweet = gsub("@\\S*", "", tweet),
        tweet = gsub("amp", "", tweet),
        tweet = gsub("[\r\n]", "", tweet),
        tweet = gsub("[0-9]", "", tweet),
        tweet = gsub("[[:punct:]]", "", tweet)
      )
    
    docs_df =
      Corpus(VectorSource(df_text)) %>% 
      tm_map(removeNumbers) %>%
      tm_map(removePunctuation) %>%
      tm_map(stripWhitespace) %>% 
      tm_map(content_transformer(tolower)) %>% 
      tm_map(removeWords, stopwords("english")) %>% 
      tm_map(removeWords, stopwords("spanish"))
    
    words_df = 
      TermDocumentMatrix(docs_df) %>% 
      as.matrix() %>% 
      rowSums() %>% 
      sort(decreasing = TRUE)
    
    word_count_df = data.frame(word = names(words_df),freq = words_df)
    
    return(word_count_df)
    
  }
  
  # Define the UI
  ui <- fluidPage(
    
    titlePanel("Election Tweets Explorer"),
    
    sidebarPanel(
    radioButtons(
      "Hashtag_choice",
      h4("Tweets containing..."),
      inline = TRUE,
      choiceNames = list("#Biden / #JoeBiden", "#Trump / #DonaldTrump", "Both"),
      choiceValues = list("Biden", "Trump", "Both")
    ),
    radioButtons(
      "Party_choice",
      h4("Tweets from states won by..."),
      inline = TRUE,
      choiceNames = list("Democrats", "Republicans", "All States"),
      choiceValues = list("DEM", "REP", "Both")
    ),
    sliderInput(
      "cut_top",
      h4("Number of words removed from top..."),
      min = 0, max = 10,
      value = 3
    )),
    mainPanel(
    wordcloud2Output('wordcloud2'), width = 8
    )
  )
  
  
  # Define the server code
  server <- function(input, output) {
    output$wordcloud2 <- renderWordcloud2({
      
      if (input[["Hashtag_choice"]] == "Both" & input[["Party_choice"]] == "Both") {
        
        main_tweets_usa %>% 
          wordcount_df() %>% 
          slice_tail(n = nrow(.) - input[["cut_top"]]) %>%
          wordcloud2(size = 1, figPath = "twitter.jpg", ellipticity = 1, minSize = 6)
        
      } else if (input[["Hashtag_choice"]] != "Both" & input[["Party_choice"]] != "Both") {
        
        main_tweets_usa %>% 
          filter(
            hashtag == input[["Hashtag_choice"]],
            winner_party == input[["Party_choice"]]
          ) %>% 
          wordcount_df() %>% 
          slice_tail(n = nrow(.) - input[["cut_top"]]) %>%
          wordcloud2(
            size = 1, 
            color = 'random-dark',
            ellipticity = 1
          )
        
      } else if (input[["Hashtag_choice"]] == "Both" & input[["Party_choice"]] != "Both") {
        
        main_tweets_usa %>% 
          filter(
            winner_party == input[["Party_choice"]]
          ) %>% 
          wordcount_df() %>% 
          slice_tail(n = nrow(.) - input[["cut_top"]]) %>%
          wordcloud2(
            size = 1, 
            color = 'random-dark',
            ellipticity = 1
          )
      } else if (input[["Hashtag_choice"]] != "Both" & input[["Party_choice"]] == "Both") {
        
        main_tweets_usa %>% 
          filter(
            hashtag == input[["Hashtag_choice"]]
          ) %>% 
          wordcount_df() %>% 
          slice_tail(n = nrow(.) - input[["cut_top"]]) %>%
          wordcloud2(
            size = 1, 
            color = 'random-dark',
            ellipticity = 1
          )
      }
    })
  }
  # Return a Shiny app object
  ## Do not Run!
  shinyApp(ui = ui, server = server)
}