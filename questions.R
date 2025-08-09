# User Q&A App - app.R
library(shiny)
library(DT)
library(shinydashboard)
library(shinyjs)
library(googlesheets4)

# Configure Google Sheets Authentication
message("Setting up Google Sheets authentication...")

# Use authentication instead of deauth
gs4_auth(
  cache = ".secrets",
  email = TRUE,
  use_oob = TRUE  # Out-of-band auth for server environments
)

# Google Sheets URL - your sheet ID
SHEET_URL <- "163KZ9gJGxkjAhID2rB_zuZbevTzFYOnPkUWjc-VWG6c"

message("Authentication complete. Sheet ID: ", SHEET_URL)

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Q&A Session - Submit & Vote"),
  
  dashboardSidebar(disable = TRUE),
  
  dashboardBody(
    useShinyjs(),
    
    fluidPage(
        
        fluidRow(
          box(
            title = "Submit a Question", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            
            textAreaInput(
              "question_text",
              "Your Question:",
              placeholder = "Type your question here...",
              rows = 3,
              width = "100%"
            ),
            
            textInput(
              "submitter_name",
              "Your Name (optional):",
              placeholder = "Anonymous",
              width = "50%"
            ),
            
            actionButton(
              "submit_question",
              "Submit Question",
              class = "btn-primary",
              icon = icon("paper-plane")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Questions & Voting",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            div(
              style = "margin-bottom: 15px;",
              actionButton(
                "refresh_questions",
                "Refresh Questions",
                class = "btn-info",
                icon = icon("refresh")
              )
            ),
            
            DT::dataTableOutput("questions_table")
          )
        )
      )
    )
  )

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store questions
  questions_data <- reactiveVal(data.frame(
    id = integer(0),
    question = character(0),
    submitter = character(0),
    votes = integer(0),
    timestamp = character(0),
    status = character(0),
    stringsAsFactors = FALSE
  ))
  
  # Function to read from Google Sheets
  read_questions <- function() {
    tryCatch({
      # Try to read the data
      data <- read_sheet(SHEET_URL, sheet = 1, col_types = "cccccc")
      
      # If empty sheet, create structure
      if (nrow(data) == 0 || ncol(data) == 0) {
        data <- data.frame(
          id = integer(0),
          question = character(0),
          submitter = character(0),
          votes = integer(0),
          timestamp = character(0),
          status = character(0),
          stringsAsFactors = FALSE
        )
        return(data)
      }
      
      # Ensure all required columns exist
      required_cols <- c("id", "question", "submitter", "votes", "timestamp", "status")
      for (col in required_cols) {
        if (!col %in% names(data)) {
          if (col == "id") {
            data[[col]] <- seq_len(nrow(data))
          } else if (col == "votes") {
            data[[col]] <- 0
          } else if (col == "status") {
            data[[col]] <- "pending"
          } else if (col == "timestamp") {
            data[[col]] <- as.character(Sys.time())
          } else {
            data[[col]] <- ""
          }
        }
      }
      
      # Convert data types safely
      data$id <- as.integer(as.numeric(data$id))
      data$votes <- as.integer(as.numeric(data$votes))
      data$question <- as.character(data$question)
      data$submitter <- as.character(data$submitter)
      data$timestamp <- as.character(data$timestamp)
      data$status <- as.character(data$status)
      
      # Replace NA values
      data$id[is.na(data$id)] <- seq_along(data$id[is.na(data$id)])
      data$votes[is.na(data$votes)] <- 0
      data$submitter[is.na(data$submitter)] <- "Anonymous"
      data$status[is.na(data$status)] <- "pending"
      
      return(data)
    }, error = function(e) {
      message("Error reading from Google Sheets: ", e$message)
      return(data.frame(
        id = integer(0),
        question = character(0),
        submitter = character(0),
        votes = integer(0),
        timestamp = character(0),
        status = character(0),
        stringsAsFactors = FALSE
      ))
    })
  }
  
  # Function to write to Google Sheets
  write_questions <- function(data) {
    tryCatch({
      message("Attempting to write ", nrow(data), " rows to Google Sheets...")
      
      # Ensure data has the right structure
      if (nrow(data) > 0) {
        # Make sure all columns are present and in right order
        data <- data[, c("id", "question", "submitter", "votes", "timestamp", "status")]
        message("Data structure confirmed")
      }
      
      # Write to sheet (this will overwrite existing data)
      sheet_write(data, ss = SHEET_URL, sheet = 1)
      message("Write successful!")
      return(TRUE)
    }, error = function(e) {
      message("Error writing to Google Sheets: ", e$message)
      showNotification(paste("Error saving to Google Sheets:", e$message), type = "error", duration = 5)
      return(FALSE)
    })
  }
  
  # Load existing questions on startup and set up auto-refresh
  observe({
    questions_data(read_questions())
    # Auto-refresh questions every 10 seconds
    invalidateLater(10000)
  })
  
  # Submit question
  observeEvent(input$submit_question, {
    if (input$question_text != "") {
      current_questions <- questions_data()
      new_id <- ifelse(nrow(current_questions) == 0, 1, max(current_questions$id, na.rm = TRUE) + 1)
      
      new_question <- data.frame(
        id = new_id,
        question = input$question_text,
        submitter = ifelse(input$submitter_name == "", "Anonymous", input$submitter_name),
        votes = 0,
        timestamp = as.character(Sys.time()),
        status = "pending",
        stringsAsFactors = FALSE
      )
      
      updated_questions <- rbind(current_questions, new_question)
      questions_data(updated_questions)
      
      # Save to Google Sheets
      if (write_questions(updated_questions)) {
        showNotification("Question submitted successfully!", type = "message")
        # Clear inputs
        updateTextAreaInput(session, "question_text", value = "")
        updateTextInput(session, "submitter_name", value = "")
      }
    } else {
      showNotification("Please enter a question before submitting.", type = "warning")
    }
  })
  
  # Refresh questions
  observeEvent(input$refresh_questions, {
    questions_data(read_questions())
    showNotification("Questions refreshed!", type = "message")
  })
  
  # Render questions table
  output$questions_table <- DT::renderDataTable({
    current_questions <- questions_data()
    
    if (nrow(current_questions) > 0) {
      # Filter out deleted questions and sort by votes (descending)
      display_questions <- current_questions[current_questions$status != "deleted", ]
      
      if (nrow(display_questions) > 0) {
        display_questions <- display_questions[order(-display_questions$votes, -display_questions$id), ]
        
        # Create vote buttons
        display_questions$vote_button <- paste0(
          '<button class="btn btn-sm btn-success vote-btn" data-id="', 
          display_questions$id, 
          '" style="cursor: pointer;">
          <i class="fa fa-thumbs-up"></i> Vote (', display_questions$votes, ')
          </button>'
        )
        
        # Add status badges with better handling of different statuses
        display_questions$status_badge <- sapply(display_questions$status, function(status) {
          if (status == "asked") {
            '<span class="label label-success">Asked</span>'
          } else if (status == "pending") {
            '<span class="label label-default">Pending</span>'
          } else {
            # Fallback for any other status
            '<span class="label label-default">Pending</span>'
          }
        })
        
        # Select columns for display
        display_questions <- display_questions[, c("question", "submitter", "vote_button", "status_badge", "timestamp")]
        colnames(display_questions) <- c("Question", "Submitted By", "Votes", "Status", "Submitted At")
        
        return(display_questions)
      }
    }
    
    # Return empty message if no questions
    data.frame(Message = "No questions submitted yet.")
  }, 
  escape = FALSE,
  options = list(
    pageLength = 10,
    searching = FALSE,
    ordering = FALSE,
    columnDefs = list(
      list(targets = c(2, 3), orderable = FALSE, width = "120px"),
      list(targets = 4, width = "150px")
    )
  ))
  
  # Add JavaScript for voting
  observe({
    shinyjs::runjs("
      $(document).on('click', '.vote-btn', function(e) {
        e.preventDefault();
        var questionId = parseInt($(this).attr('data-id'));
        if (!isNaN(questionId)) {
          Shiny.setInputValue('vote_question_id', questionId, {priority: 'event'});
        }
      });
    ")
  })
  
  # Handle voting
  observeEvent(input$vote_question_id, {
    if (!is.null(input$vote_question_id)) {
      current_questions <- questions_data()
      question_id <- input$vote_question_id
      
      # Find and update the vote count
      if (question_id %in% current_questions$id) {
        current_questions[current_questions$id == question_id, "votes"] <- 
          current_questions[current_questions$id == question_id, "votes"] + 1
        
        questions_data(current_questions)
        
        # Save to Google Sheets
        if (write_questions(current_questions)) {
          showNotification("Vote recorded!", type = "message", duration = 2)
        }
      }
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)