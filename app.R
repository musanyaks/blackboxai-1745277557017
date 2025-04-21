# Load required libraries
library(shiny)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Real-Time Vehicle Insurance Fraud Detection"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter Vehicle Insurance Claim Details"),
      numericInput("claim_amount", "Claim Amount ($):", value = 1000, min = 0),
      numericInput("claim_frequency", "Number of Claims in Last Year:", value = 1, min = 0),
      selectInput("vehicle_type", "Vehicle Type:",
                  choices = c("Sedan", "SUV", "Truck", "Motorcycle", "Other")),
      selectInput("claim_type", "Claim Type:",
                  choices = c("Accident", "Theft", "Fire", "Other")),
      actionButton("check_fraud", "Check Fraud")
    ),
    
    mainPanel(
      h4("Fraud Detection Result"),
      verbatimTextOutput("result"),
      br(),
      h5("Explanation:"),
      textOutput("explanation")
    )
  )
)

# Define server logic for fraud detection
server <- function(input, output, session) {
  
  # Simple fraud detection logic based on rules
  detect_fraud <- reactive({
    req(input$check_fraud)
    
    # Basic rules for fraud detection
    suspicious_amount <- input$claim_amount > 10000
    frequent_claims <- input$claim_frequency > 3
    suspicious_claim_type <- input$claim_type == "Other"
    
    fraud_flag <- suspicious_amount || frequent_claims || suspicious_claim_type
    
    list(
      fraud = fraud_flag,
      reasons = c(
        if (suspicious_amount) "High claim amount",
        if (frequent_claims) "Multiple claims in short period",
        if (suspicious_claim_type) "Unusual claim type"
      )
    )
  })
  
  output$result <- renderText({
    res <- detect_fraud()
    if (res$fraud) {
      paste("Potential Fraud Detected! Reasons:", paste(res$reasons, collapse = ", "))
    } else {
      "No fraud detected based on the input data."
    }
  })
  
  output$explanation <- renderText({
    "This app uses simple rule-based checks to flag potential fraud cases based on claim amount, frequency, and claim type."
  })
}

# Run the application
shinyApp(ui = ui, server = server)
