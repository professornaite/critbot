# check-me and add-me tests
library(dplyr)

# critical_words_df is already loaded

# Function to query the database
query_word <- function(word) {
  result <- critical_words_df %>%
    filter(tolower(Keyword) == tolower(word))
  
  if (nrow(result) > 0) {
    cat("Word found!\n")
    cat("Definition:", result$Definition[1], "\n")
    
    agreement <- readline(prompt = "Do you agree with this definition? (yes/no): ")
    
    if (tolower(agreement) == "no") {
      new_definition <- readline(prompt = "Please enter your alternative definition: ")
      save_alternative_definition(word, new_definition)
      cat("Thank you for your input. Your alternative definition has been saved.\n")
    } else {
      cat("Thank you for your feedback.\n")
    }
  } else {
    cat("Word not found in the database.\n")
  }
}

# Function to save alternative definition to CSV
save_alternative_definition <- function(word, new_definition) {
  new_entry <- data.frame(
    Word = word,
    Alternative_Definition = new_definition,
    Timestamp = Sys.time()
  )
  
  file_path <- "alternative_definitions.csv"
  
  if (!file.exists(file_path)) {
    write.csv(new_entry, file = file_path, row.names = FALSE)
  } else {
    write.table(new_entry, file = file_path, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  }
  
  cat("Alternative definition saved to", file_path, "\n")
}

# Main loop
while (TRUE) {
  word <- readline(prompt = "Enter a word to query (or 'quit' to exit): ")
  
  if (tolower(word) == "quit") {
    break
  }
  
  query_word(word)
}
