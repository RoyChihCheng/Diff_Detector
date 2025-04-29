# app.R
library(shiny)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(shinythemes)
library(readr) 
# Define UI
ui <- fluidPage(
  theme = shinytheme("simplex"),
  titlePanel("Reading Difficulty Analyzer (BNC-COCA Word Lists)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("text_file", "Upload Your Text File (TXT)", accept = ".txt"),
      helpText("If you don't have a text file, click 'Use Sample Files' below."),
      actionButton("load_sample", "Use Sample Files"),
      hr(),
      strong("Upload Your Wordlists (Optional):"),
      helpText("If you don't upload, the app will use our built-in K1, K2, K3, and AWL files."),
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
      p("- Higher Type-Token Ratio indicates greater lexical diversity and higher difficulty")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive value to track if sample text should be used
  sample_files <- reactiveVal(FALSE)
  
  # Function to read wordlists
  read_wordlist <- function(uploaded_file, default_file_path) {
    if (is.null(uploaded_file)) {
      # Use the default built-in wordlist
      readLines(default_file_path, encoding = "UTF-8")
    } else {
      # Use the uploaded user file
      readLines(uploaded_file$datapath, encoding = "UTF-8")
    }
  }
  
  # When user clicks "Use Sample Files"
  observeEvent(input$load_sample, {
    sample_files(TRUE)
    showNotification("Sample files loaded! Ready to analyze.", type = "message")
  })
  
  # If user uploads their own text file, reset to use uploaded file
  observe({
    if (!is.null(input$text_file)) {
      sample_files(FALSE)
    }
  })
  
  # Perform analysis when "Analyze" is clicked
  analysis_results <- eventReactive(input$analyze, {
    
    if (sample_files()) {
      text <- tolower(readLines("sample_text.txt", warn = FALSE)) %>%
        paste(collapse = " ") %>%
        str_replace_all("[^a-z\\s]", "") %>%
        str_split("\\s+") %>%
        unlist()
      
      k1_words <- read_wordlist(NULL, "K1.txt")
      k2_words <- read_wordlist(NULL, "K2.txt")
      k3_words <- read_wordlist(NULL, "K3.txt")
      awl_words <- read_wordlist(NULL, "AWL.txt")
      
    } else {
      req(input$text_file) # Require at least a text file
      
      text <- tolower(readLines(input$text_file$datapath, warn = FALSE)) %>%
        paste(collapse = " ") %>%
        str_replace_all("[^a-z\\s]", "") %>%
        str_split("\\s+") %>%
        unlist()
      
      k1_words <- read_wordlist(input$k1_file, "K1.txt")
      k2_words <- read_wordlist(input$k2_file, "K2.txt")
      k3_words <- read_wordlist(input$k3_file, "K3.txt")
      awl_words <- read_wordlist(input$awl_file, "AWL.txt")
    }
    
    text <- text[text != ""]
    
    total_tokens <- length(text)
    unique_types <- length(unique(text))
    
    k1_count <- sum(text %in% k1_words)
    k2_count <- sum(text %in% k2_words)
    k3_count <- sum(text %in% k3_words)
    awl_count <- sum(text %in% awl_words)
    off_list_count <- total_tokens - (k1_count + k2_count + k3_count + awl_count)
    
    tibble(
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
  })
  
  output$results <- renderTable({
    analysis_results()
  })
  
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
}

# Run app
shinyApp(ui = ui, server = server)



