library(shiny)
library(quantmod)
library(copula)
library(tseries)
library(urca)
library(PerformanceAnalytics)

# Helper functions
get_stock_data <- function(stocks, train_start, train_end) {
  vec <- list()
  for (i in 1:length(stocks)) {
    data <- as.data.frame(getSymbols(stocks[i], src = "yahoo", from = train_start, to = train_end, auto.assign = FALSE))
    adjusted_col <- paste0(stocks[i], ".Adjusted")
    adjusted_prices <- data[[adjusted_col]]
    
    vec[[i]] <- adjusted_prices
  }
  df <- data.frame(vec)
  colnames(df) <- stocks
  return(df)
}

convert_to_pobs <- function(data) {
  data_matrix <- as.matrix(data)
  pobs_data <- pobs(data_matrix)
  if (any(is.na(pobs_data))) {
    stop("Pseudo-observations contain NA values.")
  }
  return(pobs_data)
}

fit_and_evaluate_copula <- function(data, copula_type) {
  pobs_data <- convert_to_pobs(data)
  tryCatch({
    if (copula_type == "Student-t") {
      copula_fit <- fitCopula(tCopula(dim=2), pobs_data, method="ml")
    } else if (copula_type == "Clayton") {
      copula_fit <- fitCopula(claytonCopula(dim=2), pobs_data, method="ml")
    } else if (copula_type == "Gumbel") {
      copula_fit <- fitCopula(gumbelCopula(dim=2), pobs_data, method="ml")
    } else {
      stop(paste("Unknown copula type:", copula_type))
    }
    
    log_likelihood <- logLik(copula_fit)
    n <- nrow(data)
    p <- length(copula_fit@estimate)
    aic <- -2 * log_likelihood + 2 * p
    bic <- -2 * log_likelihood + log(n) * p
    
    return(c(LogLikelihood = log_likelihood, AIC = aic, BIC = bic))
    
  }, error = function(e) {
    warning(paste("Error fitting copula of type", copula_type, ":", e$message))
    return(c(LogLikelihood = NA, AIC = NA, BIC = NA))
  })
}

compare_copulas <- function(stocks, train_start, train_end) {
  data <- get_stock_data(stocks, train_start, train_end)
  if (ncol(data) < 2) {
    stop("Not enough columns in data for copula fitting.")
  }
  copulas <- c("Student-t", "Clayton", "Gumbel")
  results <- sapply(copulas, function(copula) {
    fit_and_evaluate_copula(data, copula)
  }, simplify = "data.frame")
  colnames(results) <- copulas
  return(results)
}

# Define UI
ui <- fluidPage(
  titlePanel("Copula-based Pair Trading Strategy"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("stock1", "Stock 1", value = "AAPL"),
      textInput("stock2", "Stock 2", value = "GOOG"),
      dateRangeInput("train_period", "Training Period", 
                     start = "2015-01-01", end = "2020-01-01"),
      dateRangeInput("test_period", "Testing Period", 
                     start = "2021-01-01", end = "2023-12-31"),
      numericInput("p1", "Threshold Probability 1", 0.05, min = 0, max = 1, step = 0.01),
      numericInput("p2", "Threshold Probability 2", 0.95, min = 0, max = 1, step = 0.01),
      numericInput("fixed_cost", "Fixed Transaction Cost", 10, min = 0, step = 0.1),
      numericInput("percentage_cost", "Percentage Transaction Cost", 0.001, min = 0, max = 1, step = 0.0001),
      actionButton("run_analysis", "Run Analysis")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Results",
                 h3("Analysis Results"),
                 verbatimTextOutput("results_text"),
                 plotOutput("returns_plot")
        ),
        tabPanel("Data",
                 h3("Stock Data"),
                 plotOutput("stock_plot1"),
                 plotOutput("stock_plot2")
        ),
        tabPanel("Copula Comparison",
                 h3("Copula Comparison Results"),
                 tableOutput("copula_comparison")
        )
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  results <- reactiveVal(NULL)
  copula_results <- reactiveVal(NULL)
  
  observeEvent(input$run_analysis, {
    stocks <- c(input$stock1, input$stock2)
    train_start <- input$train_period[1]
    train_end <- input$train_period[2]
    test_start <- input$test_period[1]
    test_end <- input$test_period[2]
    p1 <- input$p1
    p2 <- input$p2
    fixed_cost <- input$fixed_cost
    percentage_cost <- input$percentage_cost
    
    tryCatch({
      copula_comparison <- compare_copulas(stocks, train_start, train_end)
      copula_results(copula_comparison)
      
      res <- copula_conditional(stocks, train_start, train_end, test_start, test_end, p1, p2, fixed_cost, percentage_cost)
      
      results(list(
        final_capital = res$money,
        volatility = res$volatility,
        best_copula = names(which.min(copula_comparison["AIC",])),
        equity = res$equity
      ))
      
    }, error = function(e) {
      results(list(error = paste("Error in analysis:", e$message)))
    })
  })
  
  output$results_text <- renderPrint({
    req(results())
    if (!is.null(results()$error)) {
      cat(results()$error)
    } else {
      cat("Final Capital: $", round(results()$final_capital, 2), "\n")
      cat("Volatility of Returns: ", round(results()$volatility, 4), "\n")
      cat("Best Copula Model: ", results()$best_copula, "\n")
    }
  })
  
  output$returns_plot <- renderPlot({
    req(results())
    if (is.null(results()$error) && length(results()$equity) > 0) {
      plot(1:length(results()$equity), results()$equity, type = "l", 
           xlab = "Time", ylab = "Equity",
           main = "Equity Curve")
    }
  })
  
  output$stock_plot1 <- renderPlot({
    req(input$stock1, input$train_period, input$test_period)
    stock1 <- getSymbols(input$stock1, from = input$train_period[1], to = input$test_period[2], auto.assign = FALSE)
    plot(Ad(stock1), main = paste(input$stock1, "Adjusted Close"), type = "l")
  })
  
  output$stock_plot2 <- renderPlot({
    req(input$stock2, input$train_period, input$test_period)
    stock2 <- getSymbols(input$stock2, from = input$train_period[1], to = input$test_period[2], auto.assign = FALSE)
    plot(Ad(stock2), main = paste(input$stock2, "Adjusted Close"), type = "l")
  })
  
  output$copula_comparison <- renderTable({
    req(copula_results())
    comparison_df <- as.data.frame(t(copula_results()))
    comparison_df$Copula <- rownames(comparison_df)
    rownames(comparison_df) <- NULL
    comparison_df[, c("Copula", "LogLikelihood", "AIC", "BIC")]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
