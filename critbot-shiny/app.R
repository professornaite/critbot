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
      htmlOutput("suggestion_prompt"),
      uiOutput("next_steps_ui")
    )
  )
)

server <- function(input, output, session) {
  # Reactive values for state management
  rv <- reactiveValues(
    current_word = NULL,
    definition = NULL,
    show_agreement = FALSE,
    show_next_steps = FALSE,
    suggestion = "",
    agreement_choice = NULL
  )
  
  # Load critical words dataset
  critical_words_path <- here("data", "critical_words_dataset.csv")
  critical_words_df <- tryCatch({
    if (file.exists(critical_words_path)) {
      # Read the CSV, select only Keyword and Definition columns
      df <- read.csv(critical_words_path, stringsAsFactors = FALSE) %>%
        select(Keyword, Definition)
      df
    } else {
      showNotification("Critical dataset not found! Contact administrator.",
                       type = "error")
      data.frame(Keyword = character(), Definition = character())
    }
  }, error = function(e) {
    showNotification(paste("Error reading dataset:", e$message),
                     type = "error")
    data.frame(Keyword = character(), Definition = character())
  })
  
  # Handle word submission
  observeEvent(input$submit, {
    req(input$word)
    word <- tolower(trimws(input$word))
    
    # Reset UI state
    rv$show_agreement <- FALSE
    rv$show_next_steps <- FALSE
    rv$suggestion <- ""
    rv$agreement_choice <- NULL
    
    if (nchar(word) > 0) {
      found_word <- critical_words_df %>%
        filter(tolower(Keyword) == word)
      
      if (nrow(found_word) > 0) {
        rv$current_word <- word
        rv$definition <- found_word$Definition[1]
        rv$show_agreement <- TRUE
      } else {
        rv$current_word <- word
        rv$definition <- NULL
        rv$show_agreement <- FALSE # Ensure agreement UI doesn't show for not found words
      }
    }
  })
  
  # Handle reset
  observeEvent(input$reset, {
    updateTextInput(session, "word", value = "")
    rv$current_word <- NULL
    rv$definition <- NULL
    rv$show_agreement <- FALSE
    rv$show_next_steps <- FALSE
    rv$suggestion <- ""
    rv$agreement_choice <- NULL
  })
  
  # Display main result
  output$result <- renderText({
    if (is.null(rv$current_word)) {
      return("Please enter a word to query.")
    }
    if (!is.null(rv$definition)) {
      return(paste("Definition:", rv$definition))
    }
    "Word not found in the database. Please provide a suggestion below:"  # Update the message
  })
  
  # Agreement UI elements
  output$agreement_ui <- renderUI({
    if (rv$show_agreement) {
      tagList(
        radioButtons("agreement", "Do you agree with this definition?",
                     choices = c("Yes", "No"), selected = character(0)),
        conditionalPanel(
          condition = "input.agreement == 'No'",
          textAreaInput("suggestion_text", "Enter your alternative definition:", rows = 3, placeholder = "Enter suggestion here"),
          uiOutput("send_suggestion_button")
        )
      )
    } else if (!is.null(rv$current_word) && is.null(rv$definition)) {  # New condition for not found words
      tagList(
        textAreaInput("suggestion_text", "Enter a suggested definition:", rows = 3, placeholder = "Enter suggestion here"),
        uiOutput("send_suggestion_button")
      )
    } else {
      NULL
    }
  })
  
  # Dynamic button rendering
  output$send_suggestion_button <- renderUI({
    if (!is.null(input$suggestion_text) && input$suggestion_text != "") {
      actionButton("send_suggestion", "Send Suggestion")
    } else {
      NULL
    }
  })
  
  # Observe agreement choice
  observeEvent(input$agreement, {
    rv$agreement_choice <- input$agreement
    if (input$agreement == "Yes") {
      rv$show_next_steps <- TRUE
    } else {
      rv$show_next_steps <- FALSE
    }
  })
  
  # Observe suggestion text
  observeEvent(input$suggestion_text, {
    rv$suggestion <- input$suggestion_text
  })
  
  # Suggestion prompt (Email)
  output$suggestion_prompt <- renderUI({
    NULL
  })
  
  # Handle "Send Suggestion" button click
  observeEvent(input$send_suggestion, {
    req(input$suggestion_text) # Ensure suggestion is provided
    
    if (!is.null(rv$current_word)) {  # Always true, but kept for safety
      # Construct the mailto link
      email_content <- paste0(
        "mailto:quantshopusers@gmail.com",
        "?subject=", URLencode(ifelse(is.null(rv$definition), paste("New Word Suggestion - ", rv$current_word), paste("Suggestion for CritBot - ", rv$current_word))),  # Dynamic Subject
        "&body=", URLencode(paste0(
          ifelse(is.null(rv$definition), "Hi there. Please add this word to your database:\n\n", "Hi there. I believe the definition for this term should be as follows:\n\n"),  # Dynamic Body Intro
          input$suggestion_text, "\n\n",
          "Thank you!"
        ))
      )
      
      # Create the HTML link for the user to click
      output$suggestion_prompt <- renderUI({
        HTML(paste(
          "Send us an email with your suggestion pre-filled:",
          "<a href='", email_content, "'>Open Email</a>"
        ))
      })
    }
    rv$show_next_steps <- TRUE # only shows after a button is clicked.
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
  
  # Handle new search
  observeEvent(input$new_search, {
    updateTextInput(session, "word", value = "")
    rv$current_word <- NULL
    rv$definition <- NULL
    rv$show_agreement <- FALSE
    rv$show_next_steps <- FALSE
    rv$suggestion <- ""
    rv$agreement_choice <- NULL
  })
  
  # Handle related terms search
  observeEvent(input$related_terms, {
    req(rv$current_word)
    showNotification("Related terms feature coming soon!", type = "message")
  })
}

shinyApp(ui, server)


