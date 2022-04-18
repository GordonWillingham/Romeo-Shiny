library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tinytex)
library(viridis)
#install.packages("wordcloud")
library(wordcloud)
library(shinythemes)
library(RColorBrewer)
library(rsconnect)
library(readtext)
library(signalHsmm)
#install.packages('tidytext')
library(tidytext)

# task1: add in the sidebarLayout with sidebarPanel and mainPanel
# task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
# task2: add in the inputs in the sidebarPanel
# task3: Reorder Word Count plot
# task4: add in getFreq function for pre-processing: Complete
# task5: add in reactivity for getFreq function based on inputs
# task6: add in shinythemes function
# task6: and modify your figure heights

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")


getFreq <- function(book, stopwords = TRUE) {
  if (!(book %in% books))
    stop("Unknown book")

  text <- tibble(text = readLines(sprintf("data/%s.txt", book), encoding = "UTF-8"))
  
  text<- text %>%
    unnest_tokens(word,text) %>%
    count(word, sort = TRUE)
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }

  return(text)

}

ui <- fluidPage(
  
  theme = shinytheme("cosmo"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), #Application title
  sidebarLayout(
    position = "left",
    sidebarPanel(
    selectInput("book", "Choose a Book", books),
    checkboxInput("stopwords","stopwords", value = TRUE),
    actionButton("run", "Rerun"),
    h3("Word Cloud Settings"),
    sliderInput("maxwords", "Max # of Words", min = 10, max = 250, value = 100, step = 10),
    sliderInput("large_word", "Size of largest words:", min = 1, max = 10, value = 4),
    sliderInput("small_word", "Size of smallest words:", min = .1, max = 4, value = .5),
    hr(),
    h3("Word Count Settings"),
    sliderInput("min_word_count", "Minimum words for Counts Chart", min = 10, max = 150, value = 25),
    sliderInput("word_size", "Word size for Counts Chart", min = 10, max = 40, value = 14)
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Word Cloud", plotOutput("cloud"), height = "750px"),
        tabPanel("Word Counts", plotOutput("freq", height = "750px"))
      )
    )
  )
  
)

server <- function(input, output) {
  
  freq = eventReactive(
    input$run,
    {
      withProgress({
        setProgress(message = "Processing corpus...")
        getFreq(input$book, input$stopwords)})
    }
  )
  
  output$cloud = renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$large_word, input$small_word),
          random.order = FALSE, 
          max.words = input$maxwords, 
          colors=pal))
  })

  output$freq = renderPlot({
    v <- freq()
    v %>%
      filter(n > input$min_word_count) %>%
      ggplot(aes(x = reorder(word, n), y = n)) +
      geom_col() +
      coord_flip() +
      theme(text = element_text(size=input$word_size)) +
      labs(x = '', y = '')
    
  })
  
}

shinyApp(ui = ui, server = server)
