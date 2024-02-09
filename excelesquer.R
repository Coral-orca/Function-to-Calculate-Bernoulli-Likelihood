library(shiny)
library(dplyr)
library(haven)
library(ggplot2)

# Example data for default plotting
example <- data.frame("B(10, 0.5)" = rbinom(10, 100, 0.5),
                      "N(0,1)" = rnorm(100, 0, 1), 
                      "N(1,2)" = rnorm(100, 1, 2), 
                      "N(10,5)" = rnorm(100, 10, 5), 
                      "N(-10,5)" = rnorm(100, -10, 5), 
                      "R(0,1)" = runif(100, 0, 1), 
                      "R(1,2)" = runif(100, 1, 2), 
                      "R(0,10)" = runif(100, 0, 10), 
                      "R(-10,0)" = runif(100, -10, 0)
)

colnames(example) <- c("B(10, 0.5)", 
                       "N(0, 1)", 
                       "N(1, 2)", 
                       "N(10,5)", 
                       "N(-10,5)", 
                       "R(0,1)", 
                       "R(1,2)", 
                       "R(0,10)", 
                       "R(-10,0)")

# Define UI
ui <- fluidPage(
  titlePanel("ExcelesqueR"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: File upload
      fileInput("file", "Choose Data File", accept = c(".csv", ".xlsx", ".dta")),
      
      # Input: Number of variables to plot
      sliderInput("numVariables", "Number of Variables to Plot", 
                  min = 1, max = 2, value = 1, step = 0.01, 
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
          textInput("regLineColorLinear", "Regression Line Color:", value = "#ff0000"),
          selectInput("regLineTypeLinear", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addLogisticReg", "Add Logistic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addLogisticReg",
          numericInput("regLineWidthLogistic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorLogistic", "Regression Line Color:", value = "#fff000"),
          selectInput("regLineTypeLogistic", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addQuadraticReg", "Add Quadratic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addQuadraticReg",
          numericInput("regLineWidthQuadratic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorQuadratic", "Regression Line Color:", value = "#00ff00"),
          selectInput("regLineTypeQuadratic", "Regression Line Type", choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), selected = "solid")
        ),
        
        checkboxInput("addPolyReg", "Add Fractional Polynomial Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addPolyReg",
          numericInput("regLineWidthPoly", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorPoly", "Regression Line Color:", value = "#0000ff"),
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
      
      # Conditional input for histograms
      conditionalPanel(
        condition = "input.plotType == 'histogram'",
        sliderInput("binWidth", "Bin Width", min = 1, max = 30, value = 15, step = 1)
      ),
      
      # Conditional input for box plots
      conditionalPanel(
        condition = "input.plotType == 'boxplot' & input.numVariables == '1'",
        textInput("boxFillColor", "Box Fill Color:", value = "#f88379"),
        textInput("boxColor", "Outline Color:", value = "#000000"),
        textInput("boxOutlierColor", "Outlier Color:", value = "#f88379"),
      ),
      
      # Conditional input for box plots
      conditionalPanel(
        condition = "input.plotType == 'boxplot' & input.numVariables == '2'",
        textInput("vioFillCol", "Select Violin Colour", value = "#f88379"),
        textInput("boxColor2", "Outline Color:", value = "#ffffff"),
        sliderInput("boxWidth2", "Box Width:", min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("vioWidth", "Violin Width:", min = 0, max = 2, value = 1, step = 0.1)
      ),
      
      # Input: Axes labels
      # X-axis
      textInput("xTitle", "X-axis Label:", value = "X-axis Label"),
      # Y-axis
      textInput("yTitle", "Y-axis Label:", value = "Y-axis Label"),
      
      # Input: Plot title
      textInput("plotTitle", "Plot Title:", value = "Plot Title"),
      
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
      selectInput("plotType", "Select Plot Type", choices = c("scatter", "line", "boxplot"))
    } else {
      selectInput("plotType", "Select Plot Type", choices = c("histogram", "density", "boxplot"))
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
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX, input$selectedVariableY)
    } else {
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX)
    }
    
    if (input$plotType %in% c("scatter")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_point(shape = input$pointStyle, size = input$pointSize, 
                   color = input$pointColor, position = if (input$addJitter) "jitter" else "identity") +
        geom_vline(aes(xintercept = mean(!!sym(input$selectedVariableX))), colour = "#000000", linetype = "dotted", size = 0.7) +
        geom_hline(aes(yintercept = mean(!!sym(input$selectedVariableY))), colour = "#000000", linetype = "dotted", size = 0.7) +
        annotate("text", x = mean(selected_vars[[input$selectedVariableX]]), y = min(selected_vars[[input$selectedVariableY]]), 
                 label = paste(round(mean(selected_vars[[input$selectedVariableX]]), 3)), vjust = 1.5, hjust = -0.5, angle = 90) +
        annotate("text", x = min(selected_vars[[input$selectedVariableX]]), y = mean(selected_vars[[input$selectedVariableY]]), 
                 label = paste(round(mean(selected_vars[[input$selectedVariableY]]), 3)), vjust = 1.5, hjust = -0.5) +
        labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
        get(input$plotTheme)()  # Apply selected theme
      
      # Add regression lines based on checkboxes
      if (input$addLinearReg) {
        plot <- plot + geom_smooth(method = "lm", 
                                   se = FALSE, 
                                   aes(color = "Linear"), 
                                   linetype = input$regLineTypeLinear, 
                                   size = input$regLineWidthLinear)
      }
      if (input$addLogisticReg) {
        plot <- plot + geom_smooth(method = "glm", 
                                   method.args = list(family = "binomial"), 
                                   se = FALSE, 
                                   aes(color = "Logistic"), 
                                   linetype = input$regLineTypeLogistic, 
                                   size = input$regLineWidthLogistic)
      }
      if (input$addQuadraticReg) {
        plot <- plot + geom_smooth(method = "lm", 
                                   formula = y ~ poly(x, 2), 
                                   se = FALSE, 
                                   aes(color = "Quadratic"), 
                                   linetype = input$regLineTypeQuadratic, 
                                   size = input$regLineWidthQuadratic)
      }
      if (input$addPolyReg) {
        plot <- plot + geom_smooth(method = "lm", 
                                   formula = y ~ poly(x, 3), 
                                   se = FALSE, 
                                   aes(color = "Fractional Polynomial"), 
                                   linetype = input$regLineTypePoly, 
                                   size = input$regLineWidthPoly)
      }
      # Add a legend
      plot <- plot + scale_color_manual(values = c("Linear" = input$regLineColorLinear,
                                                   "Logistic" = input$regLineColorLogistic,
                                                   "Quadratic" = input$regLineColorQuadratic,
                                                   "Fractional Polynomial" = input$regLineColorPoly),
                                        name = "Regression Line")
      print(plot)
    }
    else if (input$plotType %in% c("line")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_line(linetype = input$lineStyle, size = input$lineWidth,  # Added linewidth
                  color = input$lineColor) +
        labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
    else if (input$plotType %in% c("histogram")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_histogram(fill = input$fillColor, bins = input$binWidth,
                       alpha = input$alpha) +
        labs(title = input$plotTitle, x = input$xTitle, y = "Frequency") +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
    else if (input$plotType %in% c("density")) {
      plot <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_density(fill = input$fillColor, alpha = input$alpha) +
        labs(title = input$plotTitle, x = input$xTitle, y = "Density") +
        geom_vline(aes(xintercept = mean(!!sym(input$selectedVariableX))), colour = "#000000", linetype = "dotted", size = 0.7) +
        annotate("text", x = mean(selected_vars[[input$selectedVariableX]]), y = 0, 
                 label = paste(round(mean(selected_vars[[input$selectedVariableX]]), 3)), vjust = 1.5, hjust = -0.5, angle = 90) +
        get(input$plotTheme)()  # Apply selected theme
      print(plot)
    }
    else if (input$plotType %in% c("boxplot")) {
      if (input$numVariables == 1) {
        plot <- ggplot(selected_vars, aes(y = !!sym(input$selectedVariableX))) +
          geom_boxplot(fill = input$boxFillColor, color = input$boxColor, outlier.color = input$boxOutlierColor, width = 0.5) +
          labs(title = input$plotTitle, y = input$yTitle) +
          get(input$plotTheme)()  # Apply selected theme
        print(plot)
      } else if (input$numVariables == 2) {
        plot <- ggplot(selected_vars, aes(x = as.factor(!!sym(input$selectedVariableX)), y = !!sym(input$selectedVariableY))) +
          geom_violin(fill = input$vioFillCol, color = input$boxColor2, width = input$vioWidth) +
          geom_boxplot(fill = input$vioFillCol, color = input$boxColor2, outlier.color = input$boxColor2, width = input$boxWidth2) +
          labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
          get(input$plotTheme)()  # Apply selected theme
        print(plot)
      }
    }
  })
  
  # Observer: Change input options based on plot type
  observe({
    if (!is.null(input$plotType) && input$plotType %in% c("histogram", "density", "boxplot")) {
      updateNumericInput(session, "pointSize", value = NULL, min = NULL, max = NULL, step = NULL)
      updateSelectInput(session, "pointStyle", choices = NULL, selected = NULL)
      updateNumericInput(session, "lineWidth", value = NULL, min = NULL, max = NULL, step = NULL)
    } else {
      updateNumericInput(session, "pointSize", value = 1, min = 0, max = Inf, step = 0.1)
      updateSelectInput(session, "pointStyle", choices = c("circle", "square", "triangle", "diamond", "cross", "plus", "asterisk"), selected = "circle")
      updateNumericInput(session, "lineWidth", value = 1, min = 0.1, max = 5, step = 0.1)
    }
  })
  
  observe({
    # Check if the slider is left in the middle
    if(input$numVariables %% 1 != 0) {
      # If left in the middle, round it to the nearest whole number
      updateSliderInput(session, "numVariables", value = round(input$numVariables))
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
