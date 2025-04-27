# app.R
library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(shinythemes)
# Define UI
ui <- fluidPage(theme= shinytheme("slate"),
  titlePanel("Reading Difficulty Analyzer (BNC-COCA Word Lists)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("text_file", "Upload Text File (TXT)", accept = ".txt"),
      helpText("Or use our sample files below:"),
      actionButton("load_sample", "Load Sample Files"),
      hr(),
      fileInput("k1_file", "Upload K1 Wordlist (TXT)", accept = ".txt"),
      fileInput("k2_file", "Upload K2 Wordlist (TXT)", accept = ".txt"),
      fileInput("k3_file", "Upload K3 Wordlist (TXT)", accept = ".txt"),
      fileInput("awl_file", "Upload AWL Wordlist (TXT)", accept = ".txt"),
      actionButton("analyze", "Analyze Text", class = "btn-primary"),
      hr(),
      p("This app analyzes text difficulty using Paul Nation's BNC-COCA word lists."),
      p("K1: Most frequent 1,000 words"),
      p("K2: Next most frequent 1,000 words"),
      p("K3: Next most frequent 1,000 words"),
      p("AWL: Academic Word List")
    ),
    mainPanel(
      h3("Analysis Results"),
      tableOutput("results"),
      plotOutput("coverage_plot"),
      hr(),
      h4("Interpretation Guide"),
      p("- K1 Coverage: 80-90% = Easy, 70-80% = Moderate, <70% = Difficult"),
      p("- Off-list words >10% indicates challenging text"),
      p("- Higher Type-Token Ratio indicates greater lexical diversity, and higher difficulty")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Modified version of your function to work with uploaded files
  analyze_word_frequency_difficulty <- function(text_file, k1_file, k2_file, k3_file, awl_file) {
    # Helper to read word lists from uploaded files
    read_wordlist <- function(file) {
      words <- tolower(readLines(file, warn = FALSE)) %>% 
        str_split("\\s+") %>% 
        unlist() %>% 
        str_replace_all("[^a-z]", "") %>%
        .[. != ""]
      return(words)
    }
    
    # Read in text and wordlists
    text <- tolower(readLines(text_file, warn = FALSE)) %>% 
      paste(collapse = " ") %>%
      str_replace_all("[^a-z\\s]", "") %>%
      str_split("\\s+") %>%
      unlist()
    
    k1_words <- read_wordlist(k1_file)
    k2_words <- read_wordlist(k2_file)
    k3_words <- read_wordlist(k3_file)
    awl_words <- read_wordlist(awl_file)
    
    # Clean and remove empty strings
    text <- text[text != ""]
    
    # Calculate frequencies
    total_tokens <- length(text)
    unique_types <- length(unique(text))
    
    k1_count <- sum(text %in% k1_words)
    k2_count <- sum(text %in% k2_words)
    k3_count <- sum(text %in% k3_words)
    awl_count <- sum(text %in% awl_words)
    
    off_list_count <- total_tokens - (k1_count + k2_count + k3_count + awl_count)
    
    # Build the final result table
    result_table <- tibble(
      Measure = c("K1 Coverage", "K2 Coverage", "K3 Coverage", "AWL Coverage", "Off-list Words", "Type-Token Ratio"),
      Percentage = c(
        round(100 * k1_count / total_tokens, 2),
        round(100 * k2_count / total_tokens, 2),
        round(100 * k3_count / total_tokens, 2),
        round(100 * awl_count / total_tokens, 2),
        round(100 * off_list_count / total_tokens, 2),
        round(100 * unique_types / total_tokens, 2)
      )
    )
    
    return(result_table)
  }
  
  # Reactive expression for the analysis
  analysis_results <- eventReactive(input$analyze, {
    req(input$text_file, input$k1_file, input$k2_file, input$k3_file, input$awl_file)
    
    analyze_word_frequency_difficulty(
      text_file = input$text_file$datapath,
      k1_file = input$k1_file$datapath,
      k2_file = input$k2_file$datapath,
      k3_file = input$k3_file$datapath,
      awl_file = input$awl_file$datapath
    )
  })
  
  # Render the results table
  output$results <- renderTable({
    analysis_results()
  })
  
  # Render the coverage plot
  output$coverage_plot <- renderPlot({
    results <- analysis_results()
    if (!is.null(results)) {
      coverage_data <- results %>% 
        filter(Measure %in% c("K1 Coverage", "K2 Coverage", "K3 Coverage", "AWL Coverage", "Off-list Words"))
      
      ggplot(coverage_data, aes(x = Measure, y = Percentage, fill = Measure)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(Percentage, "%")), vjust = -0.3) +
        ylim(0, 100) +
        labs(title = "Word Coverage by Frequency Band",
             y = "Percentage of Text (%)",
             x = "Word List") +
        theme_minimal() +
        theme(legend.position = "none")
    }
  })
  
  # Load sample data
  observeEvent(input$load_sample, {
    # This assumes you have sample files in a 'sample_data' folder
    # You'll need to provide these files or modify this code
    updateTextInput(session, "text_file", value = "sample_data/sample_text.txt")
    updateTextInput(session, "k1_file", value = "sample_data/K1.txt")
    updateTextInput(session, "k2_file", value = "sample_data/K2.txt")
    updateTextInput(session, "k3_file", value = "sample_data/K3.txt")
    updateTextInput(session, "awl_file", value = "sample_data/AWL.txt")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
