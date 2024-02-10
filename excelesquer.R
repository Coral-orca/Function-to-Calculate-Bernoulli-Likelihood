library(shiny)
library(dplyr)
library(haven)
library(ggplot2)
library(ggthemes)
library(Rlab)

# Example data for default plotting
example <- data.frame("Bin(10, 0.5)" = rbinom(10, 100, 0.5),
                      "Ber(0.5)" = rbern(100, 0.5),
                      "N(0,1)" = rnorm(100, 0, 1), 
                      "N(1,2)" = rnorm(100, 1, 2), 
                      "N(10,5)" = rnorm(100, 10, 5), 
                      "N(-10,5)" = rnorm(100, -10, 5), 
                      "U(0,1)" = runif(100, 0, 1), 
                      "U(1,2)" = runif(100, 1, 2), 
                      "U(0,10)" = runif(100, 0, 10), 
                      "U(-10,0)" = runif(100, -10, 0)
)

colnames(example) <- c("Bin(10, 0.5)", 
                       "Ber(0.5)", 
                       "N(0, 1)", 
                       "N(1, 2)", 
                       "N(10,5)", 
                       "N(-10,5)", 
                       "U(0,1)", 
                       "U(1,2)", 
                       "U(0,10)", 
                       "U(-10,0)")

# Define UI
ui <- fluidPage(
  titlePanel("ExcelesqueR"),
  
  sidebarLayout(
    sidebarPanel(
      # Input: File upload
      fileInput("file", "Choose Data File:", accept = c(".csv", ".xlsx", ".dta")),
      
      # Input: Number of variables to plot
      sliderInput("numVariables", "Number of Variables to Plot:", 
                  min = 1, max = 4, value = 1, step = 1, 
                  ticks = TRUE), 
      
      # Dynamic UI elements
      uiOutput("variableSelectionX"),
      uiOutput("variableSelectionY"),
      uiOutput("variableSelectionZ"),
      uiOutput("variableSelectionW"),
      uiOutput("plotTypeInput"),
      
      # Input: Theme selection
      selectInput("plotTheme", "Select Plot Theme:", 
                  choices = c("theme_grey", 
                              "theme_economist",
                              "theme_tufte",
                              "theme_base",
                              "theme_cal",
                              "theme_clean",
                              "theme_excel",
                              "theme_excel_new",
                              "theme_few",
                              "theme_fivethirtyeight",
                              "theme_foundation",
                              "theme_gdocs",
                              "theme_igray",
                              "theme_map",
                              "theme_pander",
                              "theme_par",
                              "theme_solarized",
                              "theme_solid",
                              "theme_bw", 
                              "theme_hc", 
                              "theme_stata",
                              "theme_minimal", 
                              "theme_classic", 
                              "theme_linedraw", 
                              "theme_light", 
                              "theme_dark", 
                              "theme_void"),
                  selected = "theme_grey"),
      
      uiOutput("pointStyleInput"),
      
      # Conditional input for scatter plots and regression lines
      conditionalPanel(
        condition = "input.plotType == 'scatter' & input.numVariables == '2'",
        checkboxInput("addLinearReg", "Add Linear Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addLinearReg",
          sliderInput("regLineWidthLinear", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorLinear", "Regression Line Colour:", value = "#ff0000"),
          sliderInput("regLineTypeLinear", "Regression Line Type:", value = 1, min = 1, max = 6, step = 1)
        ),
        
        checkboxInput("addLogisticReg", "Add Logistic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addLogisticReg",
          sliderInput("regLineWidthLogistic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorLogistic", "Regression Line Colour:", value = "#fff000"),
          sliderInput("regLineTypeLogistic", "Regression Line Type:", value = 1, min = 1, max = 6, step = 1)
        ),
        
        checkboxInput("addQuadraticReg", "Add Quadratic Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addQuadraticReg",
          sliderInput("regLineWidthQuadratic", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorQuadratic", "Regression Line Colour:", value = "#00ff00"),
          sliderInput("regLineTypeQuadratic", "Regression Line Type:", value = 1, min = 1, max = 6, step = 1)
        ),
        
        checkboxInput("addPolyReg", "Add Fractional Polynomial Regression", value = FALSE),
        conditionalPanel(
          condition = "input.addPolyReg",
          sliderInput("regLineWidthPoly", "Regression Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
          textInput("regLineColorPoly", "Regression Line Colour:", value = "#0000ff"),
          sliderInput("regLineTypePoly", "Regression Line Type:", value = 1, min = 1, max = 6, step = 1)
        )
      ),
      
      # Conditional input for scatter plots
      conditionalPanel(
        condition = "input.plotType == 'scatter' & input.numVariables == '2'",
        checkboxInput("addMeans2", "Add Mean Lines", value = TRUE),
        checkboxInput("addJitter2", "Add Jitter", value = FALSE),
        sliderInput("pointSize2", "Point Size:", value = 3, min = 0.1, max = 10, step = 0.1, ticks = TRUE),
        textInput("pointColor2", "Point Colour:", value = "#f88379")
      ),
      
      # Conditional input for scatter plots
      conditionalPanel(
        condition = "input.plotType == 'scatter' & input.numVariables == '3'",
        checkboxInput("addJitter3", "Add Jitter", value = FALSE),
        textInput("gradTitle3", "Gradient Label:", value = "Gradient"),
        sliderInput("pointSize3", "Point Size:", value = 3, min = 0.1, max = 10, step = 0.1, ticks = TRUE),
        textInput("gradLow3", "Gradient Low:", value = "#abcdef"),
        textInput("gradHigh3", "Gradient High:", value = "#123456")
      ),
     
      # Conditional input for scatter plots
      conditionalPanel(
        condition = "input.plotType == 'scatter' & input.numVariables == '4'",
        checkboxInput("addJitter4", "Add Jitter", value = FALSE),
        textInput("gradTitle4", "Gradient Label:", value = "Gradient"),
        textInput("sizeTitle4", "Size Label:", value = "Size"),
        textInput("gradLow4", "Gradient Low:", value = "#abcdef"),
        textInput("gradHigh4", "Gradient High:", value = "#123456")
      ),
      
      # Conditional input for line plots
      conditionalPanel(
        condition = "input.plotType == 'line'",
        sliderInput("lineWidth", "Line Width:", value = 1, min = 0.1, max = 5, step = 0.1),
        textInput("lineColor", "Line Colour:", value = "#f88379")
      ),
      
      # Conditional input for histograms and density plots
      conditionalPanel(
        condition = "input.plotType == 'histogram' | input.plotType == 'density'",
        checkboxInput("addMeans", "Add Mean Line", value = TRUE),
        textInput("fillColor", "Fill Colour:", value = "#f88379"),
        sliderInput("alpha", "Alpha:", min = 0, max = 1, step = 0.1, value = 0.7)
      ),
      
      # Conditional input for histograms
      conditionalPanel(
        condition = "input.plotType == 'histogram'",
        sliderInput("binWidth", "Bin Width:", min = 1, max = 30, value = 15, step = 1)
      ),
      
      # Conditional input for box plots
      conditionalPanel(
        condition = "input.plotType == 'boxplot' & input.numVariables == '1'",
        textInput("boxFillColor", "Box Fill Colour:", value = "#f88379"),
        textInput("boxColor", "Outline Colour:", value = "#000000"),
        textInput("boxOutlierColor", "Outlier Colour:", value = "#f88379"),
      ),
      
      # Conditional input for box plots
      conditionalPanel(
        condition = "input.plotType == 'boxplot' & input.numVariables == '2'",
        textInput("vioFillCol", "Select Violin Colour:", value = "#f88379"),
        textInput("boxColor2", "Outline Colour:", value = "#ffffff"),
        sliderInput("boxWidth2", "Box Width:", min = 0, max = 1, value = 0.1, step = 0.1),
        sliderInput("vioWidth", "Violin Width:", min = 0, max = 2, value = 1, step = 0.1)
      ),
      
      # Input: Axes labels
      # X-axis
      textInput("xTitle", "X-axis Label:", value = "X-axis Label"),
      # Y-axis
      conditionalPanel(
        condition = "input.numVariables == '2' | input.numVariables == '3' | input.numVariables == '4'",
        textInput("yTitle", "Y-axis Label:", value = "Y-axis Label")
        ),
      
      # Input: Plot title
      textInput("plotTitle", "Plot Title:", value = "Plot Title"),
    ),
    
    # Output: Main panel for displaying the plot
    mainPanel(
      plotOutput("plt")
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
    if (input$numVariables == 4) {
      selectInput("selectedVariableX", "Select X-axis Variable:", 
                  choices = variables, 
                  selected = variables[1])
    } else if (input$numVariables == 3) {
      selectInput("selectedVariableX", "Select X-axis Variable:", 
                  choices = variables, 
                  selected = variables[1])
    } else if (input$numVariables == 2) {
      selectInput("selectedVariableX", "Select X-axis Variable:", 
                  choices = variables, 
                  selected = variables[1])
    } else {
      selectInput("selectedVariableX", "Select Variable:", 
                  choices = variables, 
                  selected = variables[1])
    }
  })
  
  # Dynamic UI element: Select variable for Y-axis (only for 2-variable plots)
  output$variableSelectionY <- renderUI({
    if (input$numVariables == 4) {
      variables <- names(data())
      selectInput("selectedVariableY", "Select Y-axis Variable:", 
                  choices = variables, 
                  selected = variables[2])
    }else if (input$numVariables == 3) {
      variables <- names(data())
      selectInput("selectedVariableY", "Select Y-axis Variable:", 
                  choices = variables, 
                  selected = variables[2])
    } else if (input$numVariables == 2){
      variables <- names(data())
      selectInput("selectedVariableY", "Select Y-axis Variable:", 
                  choices = variables, 
                  selected = variables[2])
    }
  })
  
  output$variableSelectionZ <- renderUI({
    if (input$numVariables == 4) {
      variables <- names(data())
      selectInput("selectedVariableZ", "Select Gradient Variable:", 
                  choices = variables, 
                  selected = variables[3])
    } else if (input$numVariables == 3) {
      variables <- names(data())
      selectInput("selectedVariableZ", "Select Gradient Variable:", 
                  choices = variables, 
                  selected = variables[3])
    }
  })
  
  output$variableSelectionW <- renderUI({
    if (input$numVariables == 4) {
      variables <- names(data())
      selectInput("selectedVariableW", "Select Size Variable:", 
                  choices = variables, 
                  selected = variables[4])
    }
  })
  
  # Dynamic UI element: Select plot type
  output$plotTypeInput <- renderUI({
    if (input$numVariables == 4) {
      selectInput("plotType", "Select Plot Type:", choices = c("scatter"))
    } else if (input$numVariables == 3) {
      selectInput("plotType", "Select Plot Type:", choices = c("scatter"))
    } else if (input$numVariables == 2) {
      selectInput("plotType", "Select Plot Type:", choices = c("scatter", "line", "boxplot"))
    } else {
      selectInput("plotType", "Select Plot Type:", choices = c("histogram", "density", "boxplot"))
    }
  })
  
  # Dynamic UI element: Select point or line style
  output$pointStyleInput <- renderUI({
    if (input$plotType %in% c("line", "scatter")) {
      if (input$plotType == "line") {
        sliderInput("lineStyle", "Line Style:", value = 1, min = 1, max = 6, step = 1)
      } else if (input$plotType == "scatter") {
        sliderInput("pointStyle", "Point Style:", 
                    min = 0, max = 18, value = 16, step = 1, ticks = TRUE
                    )
      }
    }
  })
  
  # Output: Render the plot based on user inputs
  output$plt <- renderPlot({
    req(data())
    if (input$numVariables == 4) {
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX, input$selectedVariableY, input$selectedVariableZ, input$selectedVariableW)
    }else if (input$numVariables == 3) {
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX, input$selectedVariableY, input$selectedVariableZ)
    } else if (input$numVariables == 2) {
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX, input$selectedVariableY)
    } else {
      selected_vars <- data() %>% dplyr::select(input$selectedVariableX)
    }
    
    if (input$plotType %in% c("scatter")) {
      if (input$numVariables == 2) {
      plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_point(shape = input$pointStyle, size = input$pointSize2, 
                   color = input$pointColor2, position = if (input$addJitter2) "jitter" else "identity") +
        labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
        get(input$plotTheme)()  # Apply selected theme
      
      # Add mean lines based on checkbox
      if (input$addMeans2) {
        plt <- plt + geom_vline(aes(xintercept = mean(!!sym(input$selectedVariableX))), colour = "#000000", linetype = "dotted", size = 0.7) +
          geom_hline(aes(yintercept = mean(!!sym(input$selectedVariableY))), colour = "#000000", linetype = "dotted", size = 0.7) +
          annotate("text", x = mean(selected_vars[[input$selectedVariableX]]), y = min(selected_vars[[input$selectedVariableY]]), 
                   label = paste0("Mean: ", round(mean(selected_vars[[input$selectedVariableX]]), 3), " (SD ", round(sqrt(var(selected_vars[[input$selectedVariableX]])), 3), ")"), 
                   vjust = 1.5, hjust = -0.05, angle = 90) +
          annotate("text", x = min(selected_vars[[input$selectedVariableX]]), y = mean(selected_vars[[input$selectedVariableY]]), 
                   label = paste0("Mean: ", round(mean(selected_vars[[input$selectedVariableY]]), 3), " (SD ", round(sqrt(var(selected_vars[[input$selectedVariableY]])), 3), ")"), 
                   vjust = 1.5, hjust = -0.05)
      }

      # Add regression lines based on checkboxes
      if (input$addLinearReg) {
        plt <- plt + geom_smooth(method = "lm", 
                                   se = FALSE, 
                                   aes(color = "Linear"), 
                                   linetype = input$regLineTypeLinear, 
                                   size = input$regLineWidthLinear)
      }
      if (input$addLogisticReg) {
        plt <- plt + geom_smooth(method = "glm", 
                                   method.args = list(family = "binomial"), 
                                   se = FALSE, 
                                   aes(color = "Logistic"), 
                                   linetype = input$regLineTypeLogistic, 
                                   size = input$regLineWidthLogistic)
      }
      if (input$addQuadraticReg) {
        plt <- plt + geom_smooth(method = "lm", 
                                   formula = y ~ poly(x, 2), 
                                   se = FALSE, 
                                   aes(color = "Quadratic"), 
                                   linetype = input$regLineTypeQuadratic, 
                                   size = input$regLineWidthQuadratic)
      }
      if (input$addPolyReg) {
        plt <- plt + geom_smooth(method = "lm", 
                                   formula = y ~ poly(x, 3), 
                                   se = FALSE, 
                                   aes(color = "Fractional Polynomial"), 
                                   linetype = input$regLineTypePoly, 
                                   size = input$regLineWidthPoly)
      }
      # Add a legend
      plt <- plt + scale_color_manual(values = c("Linear" = input$regLineColorLinear,
                                                   "Logistic" = input$regLineColorLogistic,
                                                   "Quadratic" = input$regLineColorQuadratic,
                                                   "Fractional Polynomial" = input$regLineColorPoly),
                                        name = "Regression Line")
      } else if (input$numVariables == 3) {
        plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                          y = !!sym(input$selectedVariableY),
                                          color = !!sym(input$selectedVariableZ))) +
          geom_point(shape = input$pointStyle, size = input$pointSize3, 
                     position = if (input$addJitter3) "jitter" else "identity") +
          labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle, color = input$gradTitle3) +
          get(input$plotTheme)()  # Apply selected theme
        
        plt <- plt  + scale_color_gradient(low = input$gradLow3, high = input$gradHigh3)
      } else if (input$numVariables == 4) {
        plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                          y = !!sym(input$selectedVariableY),
                                          color = !!sym(input$selectedVariableZ), size = !!sym(input$selectedVariableW))) +
          geom_point(shape = input$pointStyle, 
                     position = if (input$addJitter4) "jitter" else "identity") +
          labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle, color = input$gradTitle4, size = input$sizeTitle4) +
          get(input$plotTheme)()  # Apply selected theme
        
        plt <- plt  + scale_color_gradient(low = input$gradLow4, high = input$gradHigh4) +
          scale_size_continuous()
      }
      print(plt)
    }
    else if (input$plotType %in% c("line")) {
      plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX), 
                                        y = !!sym(input$selectedVariableY))) +
        geom_line(linetype = input$lineStyle, size = input$lineWidth,  # Added linewidth
                  color = input$lineColor) +
        labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
        get(input$plotTheme)()  # Apply selected theme
      print(plt)
    }
    else if (input$plotType %in% c("histogram")) {
      plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_histogram(fill = input$fillColor, bins = input$binWidth,
                       alpha = input$alpha) +
        labs(title = input$plotTitle, x = input$xTitle, y = "Frequency") +
        get(input$plotTheme)()  # Apply selected theme
      
      # Add mean line based on checkbox
      if (input$addMeans) {
        plt <- plt + geom_vline(aes(xintercept = mean(!!sym(input$selectedVariableX))), colour = "#000000", linetype = "dotted", size = 0.7) +
          annotate("text", x = mean(selected_vars[[input$selectedVariableX]]), y = 0, 
                   label = paste0("Mean: ", round(mean(selected_vars[[input$selectedVariableX]]), 3), " (SD ", round(sqrt(var(selected_vars[[input$selectedVariableX]])), 3), ")"), 
                   vjust = 1.5, hjust = -0.05, angle = 90)
      }
      print(plt)
    }
    else if (input$plotType %in% c("density")) {
      plt <- ggplot(selected_vars, aes(x = !!sym(input$selectedVariableX))) +
        geom_density(fill = input$fillColor, alpha = input$alpha) +
        labs(title = input$plotTitle, x = input$xTitle, y = "Density") +
        get(input$plotTheme)()  # Apply selected theme
      
      # Add mean line based on checkbox
      if (input$addMeans) {
        plt <- plt + geom_vline(aes(xintercept = mean(!!sym(input$selectedVariableX))), colour = "#000000", linetype = "dotted", size = 0.7) +
          annotate("text", x = mean(selected_vars[[input$selectedVariableX]]), y = 0, 
                   label = paste0("Mean: ", round(mean(selected_vars[[input$selectedVariableX]]), 3), " (SD ", round(sqrt(var(selected_vars[[input$selectedVariableX]])), 3), ")"), 
                   vjust = 1.5, hjust = -0.05, angle = 90)
      }
      print(plt)
    }
    else if (input$plotType %in% c("boxplot")) {
      if (input$numVariables == 1) {
        plt <- ggplot(selected_vars, aes(y = !!sym(input$selectedVariableX))) +
          geom_boxplot(fill = input$boxFillColor, color = input$boxColor, outlier.color = input$boxOutlierColor, width = 0.5) +
          labs(title = input$plotTitle, y = input$yTitle) +
          get(input$plotTheme)()  # Apply selected theme
        print(plt)
      } else if (input$numVariables == 2) {
        plt <- ggplot(selected_vars, aes(x = as.factor(!!sym(input$selectedVariableX)), y = !!sym(input$selectedVariableY))) +
          geom_violin(fill = input$vioFillCol, color = input$boxColor2, width = input$vioWidth) +
          geom_boxplot(fill = input$vioFillCol, color = input$boxColor2, outlier.color = input$boxColor2, width = input$boxWidth2) +
          labs(title = input$plotTitle, x = input$xTitle, y = input$yTitle) +
          get(input$plotTheme)()  # Apply selected theme
        print(plt)
      }
    }
  })
  
  # Observer: Change input options based on plot type
  observe({
    if (!is.null(input$plotType) && input$plotType %in% c("histogram", "density", "boxplot")) {
      updateSliderInput(session, "pointSize", value = NULL, min = NULL, max = NULL, step = NULL)
      updateSliderInput(session, "pointStyle", value = NULL, min = NULL, max = NULL, step = NULL)
      updateSliderInput(session, "lineWidth", value = NULL, min = NULL, max = NULL, step = NULL)
    } else {
      updateSliderInput(session, "pointSize", value = 3, min = 0.1, max = 10, step = 0.1)
      updateSliderInput(session, "pointStyle", value = 16, min = 0, max = 18, step = 1)
      updateSliderInput(session, "lineWidth", value = 1, min = 0.1, max = 5, step = 0.1)
    }
  })
}

# Run the app
shinyApp(ui, server)
