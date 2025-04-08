# critbot-shiny/app.R
library(shiny)
library(dplyr)
library(here)

ui <- fluidPage(
  titlePanel("CritBot Word Query"),
  sidebarLayout(
    sidebarPanel(
      textInput("word", "Enter a word to query:"),
      actionButton("submit", "Submit"),
      tags$hr(),
      actionButton("reset", "Clear Results", class = "btn-warning")
    ),
    mainPanel(
      textOutput("result"),
      uiOutput("agreement_ui"),
      uiOutput("add_word_ui"),
      uiOutput("next_steps_ui")  # New UI element for next steps
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for state management
  rv <- reactiveValues(
    current_word = NULL,
    definition = NULL,
    show_agreement = FALSE,
    show_add_word = FALSE,
    show_next_steps = FALSE
  )
  
  # Load critical words dataset
  critical_words_df <- tryCatch({
    read.csv(here("data", "critical_words_dataset.csv"), 
             stringsAsFactors = FALSE)
  }, error = function(e) {
    showNotification("Critical dataset not found! Contact administrator.", 
                     type = "error")
    data.frame(Keyword = character(), Definition = character())
  })
  
  # Handle word submission
  observeEvent(input$submit, {
    req(input$word)
    word <- tolower(trimws(input$word))
    
    # Reset UI state
    rv$show_agreement <- FALSE
    rv$show_add_word <- FALSE
    rv$show_next_steps <- FALSE
    updateRadioButtons(session, "agreement", selected = character(0))
    updateTextInput(session, "new_definition", value = "")
    updateTextInput(session, "new_word_definition", value = "")
    
    if(nchar(word) > 0) {
      found_word <- critical_words_df %>%
        filter(tolower(Keyword) == word)
      
      if (nrow(found_word) > 0) {
        rv$current_word <- word
        rv$definition <- found_word$Definition[1]
        rv$show_agreement <- TRUE
      } else {
        rv$current_word <- word
        rv$definition <- NULL
        rv$show_add_word <- TRUE
      }
    }
  })
  
  # Handle reset
  observeEvent(input$reset, {
    updateTextInput(session, "word", value = "")
    rv$current_word <- NULL
    rv$definition <- NULL
    rv$show_agreement <- FALSE
    rv$show_add_word <- FALSE
    rv$show_next_steps <- FALSE
  })
  
  # Display main result
  output$result <- renderText({
    if (is.null(rv$current_word)) {
      return("Please enter a word to query.")
    }
    if (!is.null(rv$definition)) {
      return(paste("Definition:", rv$definition))
    }
    "Word not found in the database."
  })
  
  # Agreement UI elements
  output$agreement_ui <- renderUI({
    if (rv$show_agreement) {
      tagList(
        radioButtons("agreement", "Do you agree with this definition?",
                     choices = c("Yes", "No"), selected = character(0)),
        conditionalPanel(
          condition = "input.agreement == 'No'",
          textInput("new_definition", "Enter your alternative definition:"),
          actionButton("submit_new", "Submit Alternative Definition")
        )
      )
    }
  })
  
  # Next steps UI after agreement
  output$next_steps_ui <- renderUI({
    if (rv$show_next_steps) {
      tagList(
        h4("Next Steps:"),
        actionButton("new_search", "Search New Term", class = "btn-primary"),
        actionButton("related_terms", "Find Related Terms", class = "btn-info")
      )
    }
  })
  
  # Handle agreement selection
  observeEvent(input$agreement, {
    req(input$agreement)
    if (input$agreement == "Yes") {
      rv$show_next_steps <- TRUE
    } else {
      rv$show_next_steps <- FALSE
    }
  })
  
  # Handle new search
  observeEvent(input$new_search, {
    updateTextInput(session, "word", value = "")
    rv$current_word <- NULL
    rv$definition <- NULL
    rv$show_agreement <- FALSE
    rv$show_add_word <- FALSE
    rv$show_next_steps <- FALSE
    updateTextInput(session, "word", placeholder = "Enter a new word to query")
  })
  
  # Handle related terms search
  observeEvent(input$related_terms, {
    req(rv$current_word)
    showNotification("Related terms feature coming soon!", type = "message")
    # Placeholder for future implementation
    # You could add semantic analysis or pre-defined relationships here
  })
  
  # New UI for adding words
  output$add_word_ui <- renderUI({
    if (rv$show_add_word) {
      tagList(
        h4("Word not found. Would you like to add it to our database?"),
        textAreaInput("new_word_definition", "Enter your definition:",
                      rows = 3, resize = "vertical"),
        actionButton("submit_new_word", "Add New Word")
      )
    }
  })
  
  # Handle new word submission
  observeEvent(input$submit_new_word, {
    req(input$new_word_definition)
    
    new_entry <- data.frame(
      Keyword = rv$current_word,
      Definition = trimws(input$new_word_definition),
      stringsAsFactors = FALSE
    )
    
    tryCatch({
      # Add to main dataset
      critical_words_df <<- bind_rows(critical_words_df, new_entry)
      write.csv(critical_words_df, here("data", "critical_words_dataset.csv"),
                row.names = FALSE)
      
      # Also add to alternatives
      alt_entry <- data.frame(
        Word = rv$current_word,
        Alternative_Definition = trimws(input$new_word_definition),
        Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        stringsAsFactors = FALSE
      )
      
      output_path <- here("data", "alternative_definitions.csv")
      if (!file.exists(output_path)) {
        write.csv(alt_entry, file = output_path, row.names = FALSE)
      } else {
        existing <- read.csv(output_path)
        if(!any(existing$Alternative_Definition == alt_entry$Alternative_Definition)) {
          write.table(alt_entry, file = output_path, sep = ",",
                      col.names = FALSE, row.names = FALSE, append = TRUE)
        }
      }
      
      showNotification("New word added successfully!", type = "message")
      rv$definition <- input$new_word_definition
      rv$show_add_word <- FALSE
      rv$show_agreement <- TRUE
      updateTextInput(session, "new_word_definition", value = "")
    }, error = function(e) {
      showNotification("Failed to add new word. Please try again.", type = "error")
    })
  })
  
  # Existing alternative definition submission
  observeEvent(input$submit_new, {
    req(input$new_definition)
    
    new_entry <- data.frame(
      Word = rv$current_word,
      Alternative_Definition = trimws(input$new_definition),
      Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    
    output_path <- here("data", "alternative_definitions.csv")
    
    tryCatch({
      if (!file.exists(output_path)) {
        write.csv(new_entry, file = output_path, row.names = FALSE)
      } else {
        existing <- read.csv(output_path)
        if(!any(existing$Alternative_Definition == new_entry$Alternative_Definition)) {
          write.table(new_entry, file = output_path, sep = ",",
                      col.names = FALSE, row.names = FALSE, append = TRUE)
        }
      }
      showNotification("Definition saved successfully!", type = "message")
      updateTextInput(session, "new_definition", value = "")
    }, error = function(e) {
      showNotification("Failed to save definition. Please try again.", type = "error")
    })
  })
}

shinyApp(ui, server)

