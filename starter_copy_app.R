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



## Create `Built`
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


###
### Enter Business Logic before this line
###

###
## Begin User Interface Section ----------------
ui <- fluidPage(
sidebarLayout(
sidebarPanel(
###
### Enter Inputs after this line 
###
  varSelectInput("var1", "Select a variable", data = energy_year, selected = "Energy_Star_Score"),
  checkboxGroupInput("year", "Select one or more report years of interest", choices = years_vec, selected = 2022),
  checkboxInput("flip", "Flip coordinates?"),
  checkboxInput("log1", "Log transform axis and data?"),
  sliderInput("num1", "Number of histogram bins", value = 40, min = 1, max = 100),
  numericInput("num2", "Null value for true population mean", value = 0, min = 0, max = max(energy_year$Electricity_Grid_Usage)),
  varSelectInput("var2", "Select a variable for x-axis", data = energy_year, selected = "Source_EUI"),
  checkboxInput("log2", "Log transform variable for x-axis?"),
  varSelectInput("var3", "Select a variable for y-axis", data = energy_year, selected = "Site_EUI"),
  checkboxInput("log3", "Log transform variable for y-axis?"),
  checkboxInput("smooth", "Add linear model smoother line?")
  
  
  
  

  
  
  
  
###
### Enter inputs before this line 
###
), # sidebarpanel
mainPanel(

###
### Enter Outputs After this line
###
###
  plotOutput("plot"),
  tableOutput("static"),
  plotOutput("bivariate"),
  tableOutput("summary"),
  dataTableOutput("dynamic")

  
  
  

###  
### Enter Outputs Before this line
###
) #mainPanel
) #sidebarLayout
) #fluidpage

server <- function(input, output, session) {
  
###  
### Enter Server Code After this line
###

  data_filtered <- reactive({
    energy_year |> filter(Report_Year %in% input$year)
  })
  
## Create single variable plot
  
## Create base plot
  output$plot <- renderPlot({
    df <- data_filtered()
    
    var1 <- (input$var1)
    
    if (is.numeric(df[[input$var1]])) {
      p <- ggplot(df, aes(x = !!var1)) +
        geom_histogram(bins = input$num1) +
        facet_wrap(~Report_Year)
      if (input$log1) {
        p <- p + scale_x_log10()
      }
    } else {
      p <- ggplot(df, aes(x = !!var1)) +
        geom_bar() +
        facet_wrap(~Report_Year)
      if (input$flip) {
        p <- p + coord_flip()
      }
    }
    p
  })

## Check for other inputs and adjust base plot   
  
  
## Run t.test
## Inside the render function, create a temporary data frame 
## of just the selected variable for the report years and no 0 values
## and save it.
  
  
  output$static <- renderTable({
    df <- data_filtered()
    
    if (!is.numeric(df[[input$var1]])) {
      return(NULL)
    }
    
    vec <- df[[input$var1]]
    vec <- vec[!is.na(vec)]
    
    if (input$log1) {
      vec <- log(vec)
    }
    
    summarize_data(vec, input$num2)
  })
  
  

## Check for log and then run t.test of transformed data (or not) 
## using function from busines logic section   


  

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
 
# if (isnx & isny) { # Are both x and y numeric
#    
# 
#    if (.   ) { # log transform x Axis?
#       
#    } #end if log x
# 
#    if () { # log transform y Axis?
# 
#      
#    } # end if log y
# 
#    if () { # Add OLS smoother?
# 
#    }
#  # end if both numeric
# 
#   } else if (isnx) {  # if x is numeric and y is not
#     
# 
#     if () {# Is x logged
#     
#     } #end if log x (on the y axis)
# 
#   } else if (isny) {# if y is numeric and x is not
#       
#     if () {  # Is y logged
#      
#     } #end if log x (on the y axis)
# 
# } else { # neither x or y are numeric
# 
# } #end if
 
  output$bivariate <- renderPlot({
    df <- data_filtered()
    
    var_x <- (input$var2)
    var_y <- (input$var3)
    
    isnx <- is.numeric(df[[input$var2]])
    isny <- is.numeric(df[[input$var3]])
    
    p <- ggplot(df, aes(x = !!var_x, y = !!var_y, color = Report_Year))
    
    if (isnx & isny) {
      p <- p + geom_point()
      if (input$log2) {
        p <- p + scale_x_log10()
      }
      if (input$log3) {
        p <- p + scale_y_log10()
      }
      if (input$smooth) {
        p <- p + geom_smooth(method = "lm", se = FALSE)
      }
      
    } else if (isnx) {
      p <- p + geom_boxplot(aes(group = !!var_y))
      if (input$log2) {
        p <- p + scale_x_log10()
      }
      
    } else if (isny) {
      p <- p + geom_boxplot(aes(group = !!var_x))
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


## Check if either variable needs to be transformed and 
## then transform the data as required for the Linear Model and create output
## 
      
  output$summary <- renderTable({
    df <- data_filtered()
    
    if (!is.numeric(df[[input$var2]]) & !is.numeric(df[[input$var3]])) {
      return(NULL)
    }
    
    df <- df %>% drop_na(!!(input$var2), !!(input$var3))
    
    if (input$log2) {
      df <- df %>% mutate(xvar = log(!!(input$var2)))
    } else {
      df <- df %>% mutate(xvar = !!(input$var2))
    }
    
    if (input$log3) {
      df <- df %>% mutate(yvar = log(!!(input$var3)))
    } else {
      df <- df %>% mutate(yvar = !!(input$var3))
    }
    
    model <- lm(yvar ~ xvar, data = df)
    broom::tidy(model)
  })





## Create output table for all data with page length 20
  output$dynamic <- renderDT({
    datatable(energy_year, options = list(pageLength = 20))
  })

### Enter Server code above this line
}# server

shinyApp(ui, server)
