# Moderator Q&A App - moderator_app.R
library(shiny)
library(DT)
library(shinydashboard)
library(shinyjs)
library(googlesheets4)
library(dplyr)

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
  dashboardHeader(title = "Q&A Session - Moderator Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Manage Questions", tabName = "manage", icon = icon("cogs")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),
    
    tabItems(
      # Question Management Tab
      tabItem(
        tabName = "manage",
        
        fluidRow(
          box(
            title = "Question Management",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            div(
              style = "margin-bottom: 15px;",
              actionButton(
                "refresh_mod_questions",
                "Refresh Questions",
                class = "btn-info",
                icon = icon("refresh")
              ),
              
              actionButton(
                "mark_all_pending",
                "Reset All to Pending",
                class = "btn-warning",
                icon = icon("undo"),
                style = "margin-left: 10px;"
              )
            ),
            
            DT::dataTableOutput("moderator_questions_table")
          )
        )
      ),
      
      # Statistics Tab
      tabItem(
        tabName = "stats",
        
        fluidRow(
          valueBoxOutput("total_questions"),
          valueBoxOutput("pending_questions"),
          valueBoxOutput("asked_questions")
        ),
        
        fluidRow(
          box(
            title = "Question Statistics",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            
            DT::dataTableOutput("stats_table")
          )
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive values to store questions
  moderator_questions_data <- reactiveVal(data.frame(
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
      # Ensure data has the right structure
      if (nrow(data) > 0) {
        # Make sure all columns are present and in right order
        data <- data[, c("id", "question", "submitter", "votes", "timestamp", "status")]
      }
      
      # Write to sheet
      sheet_write(data, ss = SHEET_URL, sheet = 1)
      return(TRUE)
    }, error = function(e) {
      message("Error writing to Google Sheets: ", e$message)
      return(FALSE)
    })
  }
  
  # Load existing questions on startup
  observe({
    moderator_questions_data(read_questions())
  })
  
  # Refresh questions
  observeEvent(input$refresh_mod_questions, {
    moderator_questions_data(read_questions())
    showNotification("Questions refreshed!", type = "message")
  })
  
  # Reset all questions to pending
  observeEvent(input$mark_all_pending, {
    current_questions <- moderator_questions_data()
    if (nrow(current_questions) > 0) {
      current_questions[current_questions$status != "deleted", "status"] <- "pending"
      moderator_questions_data(current_questions)
      if (write_questions(current_questions)) {
        showNotification("All questions reset to pending status!", type = "success")
      }
    }
  })
  
  # Render moderator questions table
  output$moderator_questions_table <- DT::renderDataTable({
    current_questions <- moderator_questions_data()
    
    if (nrow(current_questions) > 0) {
      # Filter out deleted questions and sort by votes (descending)
      display_questions <- current_questions[current_questions$status != "deleted", ]
      display_questions <- display_questions[order(-display_questions$votes, -display_questions$id), ]
      
      # Create action buttons
      display_questions$actions <- paste0(
        '<div class="btn-group" role="group">',
        '<button class="btn btn-xs btn-success mark-asked-btn" data-id="', 
        display_questions$id, 
        '" onclick="mark_asked(', display_questions$id, ')" ',
        ifelse(display_questions$status == "asked", 'disabled', ''), '>
        <i class="fa fa-check"></i> Mark Asked
        </button>',
        '<button class="btn btn-xs btn-warning mark-pending-btn" data-id="', 
        display_questions$id, 
        '" onclick="mark_pending(', display_questions$id, ')" ',
        ifelse(display_questions$status == "pending", 'disabled', ''), '>
        <i class="fa fa-clock-o"></i> Mark Pending
        </button>',
        '<button class="btn btn-xs btn-danger delete-btn" data-id="', 
        display_questions$id, 
        '" onclick="delete_question(', display_questions$id, ')">
        <i class="fa fa-trash"></i> Delete
        </button>',
        '</div>'
      )
      
      # Add status badges
      display_questions$status_badge <- ifelse(
        display_questions$status == "asked", 
        '<span class="label label-success">Asked</span>',
        ifelse(display_questions$status == "pending", 
               '<span class="label label-warning">Pending</span>',
               '<span class="label label-default">Unknown</span>')
      )
      
      # Select columns for display
      display_questions <- display_questions[, c("question", "submitter", "votes", "status_badge", "timestamp", "actions")]
      colnames(display_questions) <- c("Question", "Submitted By", "Votes", "Status", "Submitted At", "Actions")
      
      display_questions
    } else {
      data.frame(Message = "No questions available.")
    }
  }, 
  escape = FALSE,
  options = list(
    pageLength = 15,
    searching = TRUE,
    ordering = TRUE,
    columnDefs = list(
      list(targets = c(2, 3, 5), className = "dt-center"),
      list(targets = 5, orderable = FALSE, width = "200px")
    )
  ))
  
  # Add JavaScript for moderator actions
  observe({
    shinyjs::runjs("
      $(document).on('click', '.mark-asked-btn', function() {
        var questionId = $(this).data('id');
        Shiny.setInputValue('mark_asked_id', questionId, {priority: 'event'});
      });
      
      $(document).on('click', '.mark-pending-btn', function() {
        var questionId = $(this).data('id');
        Shiny.setInputValue('mark_pending_id', questionId, {priority: 'event'});
      });
      
      $(document).on('click', '.delete-btn', function() {
        var questionId = $(this).data('id');
        if (confirm('Are you sure you want to delete this question? This action cannot be undone.')) {
          Shiny.setInputValue('delete_question_id', questionId, {priority: 'event'});
        }
      });
      
      // Alternative functions for manual calls
      window.mark_asked = function(question_id) {
        Shiny.setInputValue('mark_asked_id', question_id, {priority: 'event'});
      };
      
      window.mark_pending = function(question_id) {
        Shiny.setInputValue('mark_pending_id', question_id, {priority: 'event'});
      };
      
      window.delete_question = function(question_id) {
        if (confirm('Are you sure you want to delete this question? This action cannot be undone.')) {
          Shiny.setInputValue('delete_question_id', question_id, {priority: 'event'});
        }
      };
    ")
  })
  
  # Handle mark as asked
  observeEvent(input$mark_asked_id, {
    current_questions <- moderator_questions_data()
    question_id <- input$mark_asked_id
    
    current_questions[current_questions$id == question_id, "status"] <- "asked"
    moderator_questions_data(current_questions)
    
    if (write_questions(current_questions)) {
      showNotification("Question marked as asked!", type = "success", duration = 3)
    }
  })
  
  # Handle mark as pending
  observeEvent(input$mark_pending_id, {
    current_questions <- moderator_questions_data()
    question_id <- input$mark_pending_id
    
    current_questions[current_questions$id == question_id, "status"] <- "pending"
    moderator_questions_data(current_questions)
    
    if (write_questions(current_questions)) {
      showNotification("Question marked as pending!", type = "info", duration = 3)
    }
  })
  
  # Handle delete question
  observeEvent(input$delete_question_id, {
    current_questions <- moderator_questions_data()
    question_id <- input$delete_question_id
    
    current_questions[current_questions$id == question_id, "status"] <- "deleted"
    moderator_questions_data(current_questions)
    
    if (write_questions(current_questions)) {
      showNotification("Question deleted!", type = "warning", duration = 3)
    }
  })
  
  # Statistics value boxes
  output$total_questions <- renderValueBox({
    current_questions <- moderator_questions_data()
    total <- nrow(current_questions[current_questions$status != "deleted", ])
    
    valueBox(
      value = total,
      subtitle = "Total Questions",
      icon = icon("question-circle"),
      color = "blue"
    )
  })
  
  output$pending_questions <- renderValueBox({
    current_questions <- moderator_questions_data()
    pending <- sum(current_questions$status == "pending")
    
    valueBox(
      value = pending,
      subtitle = "Pending Questions",
      icon = icon("clock-o"),
      color = "yellow"
    )
  })
  
  output$asked_questions <- renderValueBox({
    current_questions <- moderator_questions_data()
    asked <- sum(current_questions$status == "asked")
    
    valueBox(
      value = asked,
      subtitle = "Asked Questions",
      icon = icon("check-circle"),
      color = "green"
    )
  })
  
  # Statistics table
  output$stats_table <- DT::renderDataTable({
    current_questions <- moderator_questions_data()
    
    if (nrow(current_questions) > 0) {
      # Create summary statistics
      active_questions <- current_questions[current_questions$status != "deleted", ]
      
      # Group by status and calculate stats
      status_summary <- aggregate(cbind(votes) ~ status, data = active_questions, 
                                  FUN = function(x) c(count = length(x), 
                                                      avg_votes = round(mean(x, na.rm = TRUE), 1),
                                                      total_votes = sum(x, na.rm = TRUE)))
      
      # Flatten the result
      stats_data <- data.frame(
        Status = status_summary$status,
        Count = status_summary$votes[, "count"],
        `Avg Votes` = status_summary$votes[, "avg_votes"],
        `Total Votes` = status_summary$votes[, "total_votes"],
        check.names = FALSE
      )
      
      stats_data
    } else {
      data.frame(Message = "No statistics available.")
    }
  },
  options = list(
    pageLength = 5,
    searching = FALSE,
    ordering = FALSE,
    paging = FALSE,
    info = FALSE
  ))
}

# Run the app
shinyApp(ui = ui, server = server)