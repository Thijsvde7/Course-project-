library(shiny)
library(tidyverse)

mtcars2 <- mtcars |> 
  mutate(vs = as.factor(vs),
         am = as.factor(am),
         gear = as.factor(gear),
         carb = as.factor(carb),
         cyl = as.factor(cyl))

numeric_mtcars <- names(mtcars2)[sapply(mtcars2, is.numeric)]

ui <- fluidPage(
  titlePanel("Regression model with different predictors"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("predictors", "Choose explanatory variables", choices = names(mtcars2)),
      radioButtons("dependent_variable", "Choose dependent variable", choices = numeric_mtcars),
      h6("Factor variables can not be selected as dependent variables"),
      checkboxInput("interaction", "Add interaction terms"),
      submitButton("Generate the model")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Summarys statistics", br(), h3("Summary statistics of selected model"),
                              verbatimTextOutput("summary")),
                  tabPanel("Plots", br(), h3("Plots of the selected model"),
                              plotOutput("plots")),
                  tabPanel("Linear relationship", br(), h3("Plot of the selected model"),
                              plotOutput("plot"))
      )
    )
  )
)


server <- function(input, output, session) {
  model <- reactive({
    req(input$predictors, input$dependent_variable)
    validate(need(!input$dependent_variable %in% input$predictors, 
           "Dependent variable can not be in the predictors"))
    separator <- if(input$interaction) " * " else " + "
    formula <- paste(input$dependent_variable, "~", paste(input$predictors, collapse = separator))
    formula <- as.formula(formula)
    lm(formula, data = mtcars2)
  })
  output$summary <- renderPrint({summary(model())
  })
  output$plots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(model())
  })
  output$plot <- renderPlot({
    req(model()) 
    validate(need(length(input$predictors) == 1, 
           "Plot can only be made with one predictor"))
    plot_formula <- as.formula(paste(input$dependent_variable, "~", input$predictors))
    plot(plot_formula, data = mtcars,
         xlab = input$predictors, 
         ylab = input$dependent_variable,
         main = paste("Relation between", input$predictors, "and", input$dependent_variable),
         pch = 19, col = "darkblue")
    abline(model(), col = "red", lwd = 2)
  })
}

shinyApp(ui = ui, server = server)



