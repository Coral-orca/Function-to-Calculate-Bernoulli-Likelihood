# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(haven)

# Example data for default plotting
example <- data.frame("N(0, 1)" = rnorm(100, 0, 1), "R(0, 1)" = runif(100, 0, 1), "N(1,2)" = rnorm(100, 1, 2), "R(1, 2)" = runif(100, 1, 2))

# Define UI
ui <- fluidPage(
  titlePanel("ExcelesqueR"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: File upload
      fileInput("file", "Choose Data File", accept = c(".csv", ".xlsx", ".dta")),
      
      # Input: Number of variables to plot
      sliderInput("numVariables", "Number of Variables to Plot", 
                  min = 1, max = 2, value = 1, step = 1, 
                  ticks = FALSE), 
      
      # Dynamic UI elements
      uiOutput("variableSelectionX"),
      uiOutput("variableSelectionY"),
      uiOutput("plotTypeInput"),
      
      # Input: Theme selection
      selectInput("plotTheme", "Select Plot Theme", 
                  choices = c("theme_grey", "theme_bw", "theme_minimal", "theme_classic", "theme_linedraw", "theme_light", "theme_dark", "theme_void"),
                  selected = "theme_grey"),
      
      uiOutput("pointStyleInput"),
      
      # Conditional input for scatter plots and regression lines
      conditionalPanel(
        condition = "input.plotType == 'scatter'",
        checkboxInput("addLinearReg", "Add Linear Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addLinearReg",
          numericInput("regLineWidthLinear", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorLinear", "Regression Line Color:", value = "#3366cc"),
          selectInput("regLineTypeLinear", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addLogisticReg", "Add Logistic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addLogisticReg",
          numericInput("regLineWidthLogistic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorLogistic", "Regression Line Color:", value = "#3366cc"),
          selectInput("regLineTypeLogistic", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addQuadraticReg", "Add Quadratic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addQuadraticReg",
          numericInput("regLineWidthQuadratic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorQuadratic", "Regression Line Color:", value = "#3366cc"),
          selectInput("regLineTypeQuadratic", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addPolyReg", "Add Fractional Polynomial Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addPolyReg",
          numericInput("regLineWidthPoly", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorPoly", "Regression Line Color:", value = "#3366cc"),
          selectInput("regLineTypePoly", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        )
      ),
      
      # Conditional input for scatter plots
      conditionalPanel(
        condition = "input.plotType == 'scatter'",
        checkboxInput("addJitter", "Add Jitter?", value = FALSE),
        numericInput("pointSize", "Point Size:", value = 1),
        textInput("pointColor", "Point Color:", value = "#f88379")
      ),
      
      # Conditional input for line plots
      conditionalPanel(
        condition = "input.plotType == 'line'",
        numericInput("lineWidth", "Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
        textInput("lineColor", "Line Color:", value = "#f88379")
      ),
      
      # Conditional input for histograms and density plots
      conditionalPanel(
        condition = "input.plotType == 'histogram' || input.plotType == 'density'",
        textInput("fillColor", "Fill Color:", value = "#f88379"),
        sliderInput("alpha", "Alpha:", min = 0, max = 1, step = 0.1, value = 0.7)
      ),
      
      # Input: Plot title
      textInput("plotTitle", "Plot Title:", value = "Plot Title"),
      
      # Conditional input for histograms
      conditionalPanel(
        condition = "input.plotType == 'histogram'",
        sliderInput("binWidth", "Bin Width", min = 1, max = 30, value = 15, step = 1)
      ),
      
      # Input: Download button
      downloadButton("downloadPlot", "Download Plot")
    ),
    
    # Output: Main panel for displaying the plot
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Reactive expression for reading data from the uploaded file
  data <- reactive({
    inFile <- input$file
    if (is.null(inFile)) return(example)
    
    # Checking the file extension and reading accordingly
    if (tools::file_ext(inFile$name) %in% c("csv", "xlsx")) {
      read.table(inFile$datapath, header = TRUE, sep = ",")
    } else if (tools::file_ext(inFile$name) == "dta") {
      read_dta(inFile$datapath)
    } else {
      stop("Unsupported file format")
    }
  })
  
  # Dynamic UI element: Select variable for X-axis
  output$variableSelectionX <- renderUI({
    variables <- names(data())
    if (input$numVariables == 2) {
      selectInput("selectedVariableX", "Select Variable for X-axis", 
                  choices = variables, 
                  selected = variables[1])
    } else {
      selectInput("selectedVariableX", "Select Variable", 
                  choices = variables, 
                  selected = variables[1])
    }
  })
  
  # Dynamic UI element: Select variable for Y-axis (only for 2-variable plots)
  output$variableSelectionY <- renderUI({
    if (input$numVariables == 2) {
      variables <- names(data())
      selectInput("selectedVariableY", "Select Variable for Y-axis", 
                  choices = variables, 
                  selected = variables[2])
    }
  })
  
  # Dynamic UI element: Select plot type
  output$plotTypeInput <- renderUI({
    if (input$numVariables == 2) {
      selectInput("plotType", "Select Plot Type", choices = c("scatter", "line"))
    } else {
      selectInput("plotType", "Select Plot Type", choices = c("histogram", "density"))
    }
  })
  
  # Dynamic UI element: Select point or line style
  output$pointStyleInput <- renderUI({
    if (input$numVariables == 2 && input$plotType %in% c("line", "scatter")) {
      if (input$plotType == "line") {
        selectInput("lineStyle", "Line Style", 
                    choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                    selected = "solid")
      } else if (input$plotType == "scatter") {
        selectInput("pointStyle", "Point Style", 
                    choices = c("circle", "square", "triangle", "diamond", "cross", "plus", "asterisk"),
                    selected = "circle")
      }
    }
  })
  
  # Output: Render the plot based on user inputs
  output$plot <- renderPlot({
    req(data())
    
    if (input$numVariables == 2) {
      selected_vars <- data() %>% select(input$selectedVariableX, input$selectedVariableY)
    } else {
      selected_vars <- data() %>% select(input$selectedVariableX)
    }
    
    if (input$plotType %in% c("scatter")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_point(shape = input$pointStyle, size = input$pointSize, 
                   color = input$pointColor, position = if (input$addJitter) "jitter" else "identity") +
        ggtitle(input$plotTitle) +
        get(input$plotTheme)()  # Apply selected theme
      
      # Add regression lines based on checkboxes
      if (input$addLinearReg) {
        plot <- plot + geom_smooth(method = "lm", se = FALSE, 
                                   color = input$regLineColorLinear, linetype = input$regLineTypeLinear, size = input$regLineWidthLinear)
      }
      if (input$addLogisticReg) {
        plot <- plot + geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, 
                                   color = input$regLineColorLogistic, linetype = input$regLineTypeLogistic, size = input$regLineWidthLogistic)
      }
      if (input$addQuadraticReg) {
        plot <- plot + geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, 
                                   color = input$regLineColorQuadratic, linetype = input$regLineTypeQuadratic, size = input$regLineWidthQuadratic)
      }
      if (input$addPolyReg) {
        plot <- plot + geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, 
                                   color = input$regLineColorPoly, linetype = input$regLineTypePoly, size = input$regLineWidthPoly)
      }
      
      print(plot)
    }
    else if (input$plotType %in% c("line")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_line(linetype = input$lineStyle, size = input$lineWidth,  # Added linewidth
                  color = input$lineColor) +
        ggtitle(input$plotTitle) +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
    else if (input$plotType %in% c("histogram")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_histogram(fill = input$fillColor, bins = input$binWidth,
                       alpha = input$alpha) +
        ggtitle(input$plotTitle) +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
    else if (input$plotType %in% c("density")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_density(fill = input$fillColor, alpha = input$alpha) +
        ggtitle(input$plotTitle) +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
  })
  
  # Observer: Change input options based on plot type
  observe({
    if (!is.null(input$plotType) && input$plotType %in% c("histogram", "density")) {
      updateNumericInput(session, "pointSize", value = NULL, min = NULL, max = NULL, step = NULL)
      updateSelectInput(session, "pointStyle", choices = NULL, selected = NULL)
      updateNumericInput(session, "lineWidth", value = NULL, min = NULL, max = NULL, step = NULL)
    } else {
      updateNumericInput(session, "pointSize", value = 1, min = 0, max = Inf, step = 0.1)
      updateSelectInput(session, "pointStyle", choices = c("circle", "square", "triangle", "diamond", "cross", "plus", "asterisk"), selected = "circle")
      updateNumericInput(session, "lineWidth", value = 1, min = 0.1, max = 5, step = 0.1)
    }
  })
  
  # Observer: Save the plot on button click
  observeEvent(input$downloadPlot, {
    tryCatch({
      ggsave("output_plot.png", plot = output$plot())
      showModal(modalDialog("Plot saved as output_plot.png"))
    }, error = function(e) {
      showModal(modalDialog("Error saving plot. Please try again.", title = "Error"))
    })
  })
}

# Run the app
shinyApp(ui, server)
