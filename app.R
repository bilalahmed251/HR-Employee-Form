# Load the Shiny library
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("HR Employee Form"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("emp_name", "Employee Name"),
      passwordInput("emp_password", "Secret Information"),
      textAreaInput("emp_story", "Brief Personal Story"),
      
      numericInput("emp_age", "Age", value = 25, min = 18, max = 55),
      sliderInput("experience", "Years of Experience", min = 0, max = 30, value = 2),
      sliderInput("contract_range", "Contract Duration (Months)", min = 1, max = 24, value = c(6, 12)),
      
      dateInput("dob", "Date of Birth"),
      dateRangeInput("vacation", "Vacation Period"),
      
      selectInput("language", "Favorite Programming Languages",
                  choices = c("R", "Python", "Java", "C++", "JavaScript"),
                  multiple = TRUE),
      
      checkboxGroupInput("hobbies", "Hobbies",
                         choices = c("Reading", "Traveling", "Gaming", "Cooking")),
      
      checkboxInput("side_job", "Available for Side Job", FALSE),
      
      fileInput("file_upload", "Upload Resume / ID Proof"),
      
      actionButton("submit", "Submit", class = "btn-success"),
      actionButton("cancel", "Cancel", class = "btn-danger"),
      actionButton("home", "Home", class = "btn-primary")
    ),
    
    mainPanel(
      h3("Submitted Details:"),
      uiOutput("success_message"),     
      tableOutput("form_data"),        
      tableOutput("file_info")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # Reactive value to store form data
  form_data <- reactiveValues()
  
  # Reactive trigger to track submission
  data_submitted <- reactiveVal(FALSE)
  
  # Observe submit button
  observeEvent(input$submit, {
    # Validate inputs
    validate(
      need(input$emp_age >= 18 && input$emp_age <= 55, "Age must be between 18 and 55"),
      need(input$emp_name != "", "Please enter a name"),
      need(length(input$language) > 0, "Please select at least one programming language")
    )
    
    # Store form data
    form_data$name <- input$emp_name
    form_data$story <- input$emp_story
    form_data$secret <- input$emp_password
    form_data$age <- input$emp_age
    form_data$experience <- input$experience
    form_data$contract <- paste(input$contract_range, collapse = " to ")
    form_data$dob <- as.character(input$dob)
    form_data$vacation <- paste(input$vacation, collapse = " to ")
    form_data$languages <- paste(input$language, collapse = ", ")
    form_data$hobbies <- paste(input$hobbies, collapse = ", ")
    form_data$side_job <- ifelse(input$side_job, "Yes", "No")
    form_data$resume <- if (!is.null(input$file_upload)) input$file_upload$name else "No file uploaded"
    
    # Trigger form submission display
    data_submitted(TRUE)
    
    # Show success message
    output$success_message <- renderUI({
      div(class = "alert alert-success", "Form submitted successfully!")
    })
  })
  
  # Reset form on cancel
  observeEvent(input$cancel, {
    updateTextInput(session, "emp_name", value = "")
    updateTextAreaInput(session, "emp_story", value = "")
    updatePasswordInput(session, "emp_password", value = "")
    updateNumericInput(session, "emp_age", value = 25)
    updateSliderInput(session, "experience", value = 2)
    updateSliderInput(session, "contract_range", value = c(6, 12))
    updateDateInput(session, "dob", value = Sys.Date())
    updateDateRangeInput(session, "vacation", start = NULL, end = NULL)
    updateSelectInput(session, "language", selected = character(0))
    updateCheckboxGroupInput(session, "hobbies", selected = character(0))
    updateCheckboxInput(session, "side_job", value = FALSE)
    output$success_message <- renderUI(NULL)
    data_submitted(FALSE)
  })
  
  # Return to home
  observeEvent(input$home, {
    updateTextInput(session, "emp_name", value = "")
    output$success_message <- renderUI(NULL)
    data_submitted(FALSE)
  })
  
  # Display submitted data
  output$form_data <- renderTable({
    if (data_submitted()) {
      data.frame(
        Field = c("Name", "Personal Story", "Secret Info", "Age", "Experience",
                  "Contract Duration", "Date of Birth", "Vacation Period",
                  "Programming Languages", "Hobbies", "Side Job Available", "Resume"),
        Value = c(form_data$name, form_data$story, "****",
                  form_data$age, form_data$experience, form_data$contract,
                  form_data$dob, form_data$vacation, form_data$languages,
                  form_data$hobbies, form_data$side_job, form_data$resume)
      )
    }
  })
  
  # Display uploaded file info
  output$file_info <- renderTable({
    if (!is.null(input$file_upload)) {
      data.frame(
        FileName = input$file_upload$name,
        FileType = input$file_upload$type,
        FileSizeKB = paste0(round(input$file_upload$size / 1024, 2), " KB")
      )
    }
  })
}

# Run the app
shinyApp(ui, server)
