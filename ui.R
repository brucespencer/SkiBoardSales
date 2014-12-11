require(shiny)
# Define UI for the Web App
shinyUI(fluidPage(
                                        # Application title
    titlePanel("Predict Ski and Snowboard Sales"),
                                        # bins
    sidebarLayout(
        sidebarPanel(
            numericInput("npredictions", "Number of Months to Predict", 12, min=1, max=24),
            submitButton()
        ),
                                        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("salesPlot"),
            textOutput("SearchTerms")
      )
    )
))
