# check-me and add-me shiny
library(shiny)
library(dplyr)

# using critical_words_df

ui <- fluidPage(
  titlePanel("CritBot Word Query"),
  sidebarLayout(
    sidebarPanel(
      textInput("word", "Enter a word to query:"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      textOutput("result"),
      uiOutput("agreement_ui")
    )
  )
)

server <- function(input, output, session) {
  result <- reactiveVal(NULL)
  
  observeEvent(input$submit, {
    word <- tolower(input$word)
    found_word <- critical_words_df %>%
      filter(tolower(Keyword) == word)
    
    if (nrow(found_word) > 0) {
      result(list(word = word, definition = found_word$Definition[1]))
    } else {
      result(NULL)
    }
  })
  
  output$result <- renderText({
    req(result())
    if (!is.null(result())) {
      paste("Definition:", result()$definition)
    } else {
      "Word not found in the database."
    }
  })
  
  output$agreement_ui <- renderUI({
    req(result())
    if (!is.null(result())) {
      tagList(
        radioButtons("agreement", "Do you agree with this definition?",
                     choices = c("Yes", "No")),
        conditionalPanel(
          condition = "input.agreement == 'No'",
          textInput("new_definition", "Enter your alternative definition:"),
          actionButton("submit_new", "Submit Alternative Definition")
        )
      )
    }
  })
  
  observeEvent(input$submit_new, {
    req(input$new_definition)
    
    new_entry <- data.frame(
      Word = result()$word,
      Alternative_Definition = input$new_definition,
      Timestamp = Sys.time()
    )
    
    file_path <- "alternative_definitions.csv"
    
    if (!file.exists(file_path)) {
      write.csv(new_entry, file = file_path, row.names = FALSE)
    } else {
      write.table(new_entry, file = file_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    }
    
    showNotification("Your alternative definition has been saved.", type = "message")
  })
}

shinyApp(ui, server)
