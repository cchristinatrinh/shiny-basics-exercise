library(shiny)
library(tidyverse)

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
energy_year |>
  mutate(Era = case_when(
    Built < 1900 ~ "Pre-1900",
    Built < 1951 ~ "Early-Mid 20th",
    Built < 2000 ~ "Late 20th",
    Built < 2011 ~ "Aughts",
    .default = "Teens and later") 
  ) |>
  relocate(Era, .after = Built)
                    



## Create t.test function with tibble output



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

  
  
  
  
  
  
  
  
###
### Enter inputs before this line 
###
), # sidebarpanel
mainPanel(

###
### Enter Outputs After this line
###
###
  

  
  
  

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

## Create single variable plot
## Create base plot
  

## Check for other inputs and adjust base plot   
  
  
## Run t.test
## Inside the render function, create a temporary data frame 
## of just the selected variable for the report years and no 0 values
## and save it.
  
  
  
  
  

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
 


## Create Linear Model Output
## Inside the render function, check if the OLS is selected
## If so, create a temporary data frame with data from the selected years


## Check if either variable needs to be transformed and 
## then transform the data as required for the Linear Model and create output
## 
      






## Create output table for all data with page length 20


### Enter Server code above this line
}# server

shinyApp(ui, server)
