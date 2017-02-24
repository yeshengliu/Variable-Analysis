#Set current working directory to the clustering folder
setwd("//pbosfile04/shared2/Advanced Analytics/99 - Users/06 - Jai/LTC Wellness/App")
getwd()


options(warn=0)
options(shiny.maxRequestSize=30*1024^2)


library(devtools)
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(lazyeval)
library(rsconnect)
library(reshape2)
library(RColorBrewer)
library(plotly)
library(RWeka)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(stringr)
library(scales)
library(qdap)
library(magrittr)
library(DT)
library(rpart)
library(rpart.plot)

header <- dashboardHeader(
  title = "Variable Importance"
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "menu_dashboard", icon = icon("dashboard")),
    menuItem("Target", tabName = "menu_target", icon = icon("asterisk")),
    menuItem("Decision Tree", tabName = "menu_decisiontree", icon = icon("tree"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(
      tabName = "menu_dashboard",
      fluidRow(
        column(width = 3,
               box(title = "Upload",
                   status = "warning",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   collapsed = FALSE,
                   fileInput(                      # csv File Upload Control
                     inputId = "fileUploaded",
                     label = "Browse csv file to upload:",
                     accept = c(".csv")
                   ),
                   radioButtons(
                     inputId = "variableManualSelect",
                     label = "Variable Selection Type:",
                     choices = c("All" = FALSE,
                                 "Manual Select" = TRUE),
                     selected = TRUE
                   ),
                   conditionalPanel(
                     condition = "input.variableManualSelect == 'TRUE'",
                     selectInput(
                       inputId = "selectVariable",
                       label = "Select Variables:",
                       choices = NULL,
                       multiple = TRUE
                     )
                   ),
                   selectInput(
                     inputId = "selectColnumeric",
                     label = "Identify Categorical Variables:",
                     choices = NULL,
                     multiple = TRUE
                   ),
                   selectInput(
                     inputId = "selectColfactor",
                     label = "Identify Numerical Variables:",
                     choices = NULL,
                     multiple = TRUE
                   )
               )
        ),
        column(width = 9,
               box(title = "Raw Data",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   dataTableOutput("raw.data")                 # Display the whole dataset
               ),
               box(title = "Filtered Data",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   dataTableOutput("raw.data.subset")          # Display table will action interactively with selected columns      
               )
        )
      )
    ),
    tabItem(
      tabName = "menu_target",
      fluidRow(
        column(width = 9,
               box(title = "Summary",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   collapsed = FALSE,
                   valueBoxOutput("value.target.freq"),        # Display number of rows selected
                   valueBoxOutput("value.total.freq")          # Display total number of rows
               ),
               box(title = "Target Data Plot",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   collapsed = TRUE,
                   uiOutput("plot.target")              # Display scatter plot of data
               )
        ),
        column(width = 3,
               box(title = "Target Variables:",
                   status = "warning",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   collapsed = FALSE,
                   selectInput(
                     inputId = "selectVariabletarget", 
                     label = "Choose Target Variable:",
                     choices = NULL,
                     multiple = TRUE
                   )
               ),
               box(title = "Numerical Variable Options:",
                   status = "warning",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   collapsed = FALSE,
                   uiOutput("filter.option")
               ),
               box(title = "Value Filters:",
                   status = "warning",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = FALSE,
                   collapsed = FALSE,
                   uiOutput("clustercondition")
               )
        )
      )
    ),
    tabItem(
      tabName = "menu_decisiontree",
      fluidRow(
        column(width = 12,
               box(title = "Desicion Tree: Plot",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   collapsed = FALSE,
                   plotOutput("tree_plot")                 # Display table will action interactively with selected columns                   )
               ),
               box(title = "Desicion Tree: Variable Importance",
                   status = "success",
                   width = NULL,
                   solidHeader = FALSE,
                   collapsible = TRUE,
                   collapsed = FALSE,
                   plotlyOutput("tree_var_imp")                 # Display table will action interactively with selected columns                   )
               )
        )
      )
    )
  )
)

dashboardPage(skin = "green",
              header, sidebar, body)
