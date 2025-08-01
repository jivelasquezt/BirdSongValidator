library(shiny)
library(tuneR)
library(seewave)
library(readxl)
library(dplyr)
library(audio)
library(DT)

# Load master recordings
data <- read_excel("master_recordings.xlsx") %>%
  mutate(across(c(recording_id, file, species, location), as.character))
data <- data %>% mutate(reviewed = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Bird Sound Validation System"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("user_name", "Enter your name:"),
      selectInput("species_select", "Select species:", choices = unique(data$species)),
      actionButton("prev_btn", "Previous"),
      actionButton("next_btn", "Next"),
      
      hr(),
      actionButton("approve_btn", "Approve"),
      actionButton("reject_btn", "Reject"),
      actionButton("uncertain_btn", "Uncertain"),
      textInput("comment", "Comments (max 50 chars):"),
      downloadButton("download_data", "Download Review Table")
    ),
    
    mainPanel(
      plotOutput("spectrogram"),
      uiOutput("audio_controls"),
      verbatimTextOutput("metadata"),
      DTOutput("review_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  values <- reactiveValues(
    index = 1,
    data = data,
    results = data.frame(recording_id=character(), species=character(), file=character(), user=character(), decision=character(), comment=character(), stringsAsFactors = FALSE)
  )
  
  get_current_record <- reactive({
    req(input$species_select)
    sp_data <- values$data %>% filter(species == input$species_select)
    if (nrow(sp_data) == 0) return(NULL)
    sp_data[values$index, ]
  })
  
  observeEvent(input$species_select, {
    values$index <- 1
  })
  
  observeEvent(input$next_btn, {
    sp_data <- values$data %>% filter(species == input$species_select)
    if (values$index < nrow(sp_data)) values$index <- values$index + 1
  })
  
  observeEvent(input$prev_btn, {
    if (values$index > 1) values$index <- values$index - 1
  })
  
  # output$spectrogram <- renderPlot({
  #   record <- get_current_record()
  #   req(record)
  #   wav <- readWave(paste0("./www/", record$file))
  #   spectro(wav, flim = c(0, 10), main = paste("Spectrogram for", record$file))
  # })
  # 
  
  output$spectrogram <- renderPlot({
    record <- get_current_record()
    req(record)
    wav <- readWave(paste0("./www/", record$file))
    
    # Observe the new image: The sounds seem to be predominantly below ~10 kHz,
    # with noise extending lower. Let's adjust flim accordingly.
    # The previous image had sounds up to ~8-9kHz. Let's set flim to 0-10 or 0-12 to capture this.
    
    spectro(wav,
            f = wav@samp.rate,
            flim = c(0, 12),          # Adjusted based on your new image to capture relevant frequencies
            main = paste("Spectrogram for", record$file),
            wl = 768,                # Still a good balance, experiment with 512-1024
            ovlp = 90,               # High overlap for smoothness
            dB = "max0",             # Crucial for proper intensity mapping
            osc = FALSE,             # No oscillogram
            scale = TRUE,            # Keep the dB color scale
            colbg = "white",         # Set background color to white
            palette = colorRampPalette(c("white", "grey90", "grey70", "grey50", "grey30", "black")),
            
            # Adjust other colors to contrast with the white background
            collevels = seq(-50, 0, length.out = 256),
            colgrid = "grey50",      # Grid lines visible but not too strong
            colaxis = "black",       # Black axis labels and ticks
            collab = "black",        # Black axis titles
            cexlab = 1.2,
            cexaxis = 1
    )
  })

  
  output$audio_controls <- renderUI({
    record <- get_current_record()
    req(record)
    tags$audio(src = record$file, type = "audio/wav", controls = TRUE)
  })
  
  output$metadata <- renderPrint({
    record <- get_current_record()
    req(record)
    paste("File:", record$file,
          "\nLocation:", record$location,
          "\nDate:", record$date,
          "\nTime of Day:", record$time)
  })
  
  handle_decision <- function(decision) {
    record <- get_current_record()
    req(record, input$user_name)
    new_entry <- data.frame(
      recording_id = record$recording_id,
      species = record$species,
      file = record$file,
      user = input$user_name,
      decision = decision,
      comment = input$comment,
      stringsAsFactors = FALSE
    )
    # Remove previous entry for same recording & user
    values$results <- values$results %>%
      filter(!(recording_id == record$recording_id & user == input$user_name)) %>%
      bind_rows(new_entry)
    
    # Save locally
    write.csv(values$results, "review_results.csv", row.names = FALSE)
  }
  
  observeEvent(input$approve_btn, {
    handle_decision("approve")
    isolate({ input$next_btn })
    sp_data <- values$data %>% filter(species == input$species_select)
    if (values$index < nrow(sp_data)) values$index <- values$index + 1
  })
  
  observeEvent(input$reject_btn, {
    handle_decision("reject")
    isolate({ input$next_btn })
    sp_data <- values$data %>% filter(species == input$species_select)
    if (values$index < nrow(sp_data)) values$index <- values$index + 1
  })
  
  observeEvent(input$uncertain_btn, {
    handle_decision("uncertain")
    isolate({ input$next_btn })
    sp_data <- values$data %>% filter(species == input$species_select)
    if (values$index < nrow(sp_data)) values$index <- values$index + 1
  })
  
  output$review_table <- renderDT({
    datatable(values$results)
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("review_results_", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(values$results, file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
