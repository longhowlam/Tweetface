
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)
library(httr)
library(curl)

# fluidRow(
#   column(6, DT::dataTableOutput('x1')),
#   column(6, plotOutput('x2', height = 500))
# )

shinyUI(fluidPage(
  tags$head(includeScript("google-analytics.js")),

  # Application title
  h3("Twitter faces...."),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 3,
                 textInput("zoeksleutel","Twitter search string", "#selfie #brunette"),
                 textInput("zoeksleutel2","Twitter screen name ", "BarackObama"),
                 textInput("hyperlink","hyperlink to a picture", "https://upload.wikimedia.org/wikipedia/commons/thumb/a/ad/Angelina_Jolie_2_June_2014_%28cropped%29.jpg/220px-Angelina_Jolie_2_June_2014_%28cropped%29.jpg"),
                 checkboxInput("findsim", "Find similar actor / actress", value=FALSE)
                 
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Intro", htmlOutput("inleiding")),
                
                tabPanel("Tweets with images", 
                  fluidRow(
                    column(6, htmlOutput("Beschrijving")),
                    column(6,DT::dataTableOutput('Tweets'))
                  )  
                 ),
                
                tabPanel("Image analysis of a screen name", htmlOutput("User")),
                
                tabPanel("Image Analysis of link", htmlOutput("LinkToImage"))
               )
             
    )
  )))

