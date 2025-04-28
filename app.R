library(shiny)
library(tidyverse)
library(DT)
library(broom)

## Read in Data ------------------
energy_year <- read_rds("data/energy_year.rds")

## Transform Data to Factors
energy_year <- energy_year |>
  mutate(Ward = factor(Ward),
         Report_Year = factor(Report_Year),
         Type_SS = factor(Type_SS),
         Type_EPA = factor(Type_EPA),
         Metered_Energy = factor(Metered_Energy),
         Metered_Water = factor(Metered_Water))

energy_year <- energy_year |>
  mutate(Era = case_when(
    Built < 1900 ~ "Pre-1900",
    Built < 1951 ~ "Early-Mid 20th",
    Built < 2000 ~ "Late 20th",
    Built < 2011 ~ "Aughts",
    .default = "Teens and later") 
  ) |>
  relocate(Era, .after = Built)


## Create t.test function with tibble output
summarize_data <- function(vec, value) {
  tidy(t.test(vec, mu = value)) |>
    select("p.value", "estimate", "conf.low", "conf.high") |>
    rename("P-value" = "p.value",
           "Estimate" = "estimate",
           "95% Lower" = "conf.low",
           "95% Upper" = "conf.high")
}

years_vec <- seq(from = 2012, to = 2022, by = 1)


## Begin User Interface Section ----------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ###
      ### Enter Inputs after this line 
      ###
      varSelectInput("var1", "Variable?", data = energy_year, selected = "Energy_Star_Score"),
      checkboxGroupInput("year", "Which Report Year(s)?", choices = years_vec, selected = 2022),
      checkboxInput("flip", "Flip coordinates on Factors?"),
      checkboxInput("log1", "Log Transform?"),
      sliderInput("num1", "Number of Histogram Bins?", value = 40, min = 1, max = 100),
      numericInput("num2", "Null Value", value = 0, min = 0, max = max(energy_year$Electricity_Grid_Usage)),
      varSelectInput("var2", "X Variable?", data = energy_year, selected = "Source_EUI"),
      checkboxInput("log2", "Log Transform?"),
      varSelectInput("var3", "Y Variable?", data = energy_year, selected = "Site_EUI"),
      checkboxInput("log3", "Log Transform?"),
      checkboxInput("smooth", "Fit OLS Model?")
    ), 
    
    mainPanel(
      plotOutput("plot1"),
      tableOutput("static"),
      plotOutput("plot2"),
      verbatimTextOutput("summary"),
      dataTableOutput("dynamic")
    ) #mainPanel
  ) #sidebarLayout
) #fluidpage

server <- function(input, output, session) {
  
  ## Create single variable plot
  output$plot1 <- renderPlot({
    df <- energy_year |>
      filter(Report_Year %in% input$year)
    
    if (is.numeric(df[[input$var1]])) {
      pl <- ggplot(df, aes(x = !!(input$var1))) +
        geom_histogram(bins = input$num1) +
        facet_wrap(~Report_Year)
      if (input$log1) {
        pl <- pl + scale_x_log10()
      }
      
    } else {
      pl <- ggplot(df, aes(x = !!(input$var1))) +
        geom_bar() +
        facet_wrap(~Report_Year)
      if (input$flip) {
        pl <- pl + coord_flip()
      }
    }
    pl
  })
  ## Create base plot
  
  
  ## Check for other inputs and adjust base plot   
  
  
  ## Run t.test
  ## Inside the render function, create a temporary data frame 
  ## of just the selected variable for the report years and no 0 values
  ## and save it.
  
  
  output$static <- renderTable({
    df <- energy_year |>
      filter(Report_Year %in% input$year,
             !(!!input$var1 %in% c(NA, 0))) |>
      select(input$var1)
    
    ## Check for log and then run t.test of transformed data (or not) 
    ## using function from business logic section   
    
    if (input$log1) {
      df <- log(df)
    }
    
    summarize_data(df, input$num2)
  })

  ## Create 2 Variable Plots
  ## Inside the render function, create the base plot
  ## with data from the selected years
  
  
  ## Add Geom based on class of x and y inputs
  ## Create flag variables for is.numeric for x and y
  ## Replace ... with appropriate variables
  # isnx <- is.numeric(...)
  # isny <- is.numeric(...)
  
  ## Use flag variables to test what type of data has been selected
  ## and then add the correct geoms, log scales, and labels 
  
  output$plot2 <- renderPlot({
    df <- energy_year |>
      filter(Report_Year %in% input$year)
    
    
    isnx <- is.numeric(df[[input$var2]])
    isny <- is.numeric(df[[input$var3]])
    
    p <- ggplot(df, aes(x = !!(input$var2), y = !!(input$var3), color = as.factor(Report_Year))) +
      labs(color = "Report Year")
    
    
    if (isnx & isny) {
      p <- p + geom_point()
      if (input$log2) {
        p <- p + scale_x_log10()
      }
      if (input$log3) {
        p <- p + scale_y_log10()
      }
      if (input$smooth) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "blue")
      }
      
    } else if (isnx) {
      p <- p + geom_boxplot(aes(group = !!(input$var3)))
      if (input$log2) {
        p <- p + scale_x_log10()
      }
      
    } else if (isny) {
      p <- p + geom_boxplot(aes(group = !!(input$var2)))
      if (input$log3) {
        p <- p + scale_y_log10()
      }
      
    } else {
      p <- p + geom_jitter(width = 0.2, height = 0.2)
    }
    p
  })
  
  ## Create Linear Model Output
  ## Inside the render function, check if the OLS is selected
  ## If so, create a temporary data frame with data from the selected years
  output$summary <- renderPrint({
    if (input$smooth) {
      df <- energy_year |>
        filter(Report_Year %in% input$year)
    }
    ## Check if either variable needs to be transformed and 
    ## then transform the data as required for the Linear Model and create output
    ## 
    
    df <- df |>
      mutate(
        xvar = if (input$log2) log(!!(input$var2)) else !!(input$var2),
        yvar = if (input$log3) log(!!(input$var3)) else !!(input$var3)
      )
    
    model <- lm(yvar ~ xvar, data = df)
    summary(model)
  })
  
  ## Create output table for all data with page length 20
  output$dynamic <- renderDT({
    datatable(energy_year, options = list(pageLength = 20))
  })
  
}# server

shinyApp(ui, server)