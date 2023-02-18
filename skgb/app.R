#### Skill-based grading ####
## Yaqian Huang; 10/28/2022; huangyq@ucla.edu ##

# load data and libraries ####
library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(plotly) # For interactive plots

# install.packages("shiny")
# install.packages("rsconnect")  # For publishing apps online
# if want plotly; change ui to plotlyOutput(); server to renderPlotly()

shinyApp(ui = ui, server = server)

# Define each panel, element ####

# Intro tab: how to use the app ----
intro_tab <- tabPanel(
  title="Instructions", # tab name
  
  # Page title
  titlePanel("How to use this app"),
  
  img(src = "[img source]"),
  
  p("[Summary text for page]"),
  p(a(href = "[url]", "[Link text]"))
)

# Second tab: check own performance ####
# Sidebar panel for inputs
sidebar_content <- sidebarPanel(
  
  # Input: dropdown for the kinds of skills
  selectInput(inputId = "skills", # to match server output color
              label = "Select a skill",
              # Create the choices that can be selected. e.g. Display "A" and link to value "a"
              choices = c("1.1 DefineSymbol"="1.1",
                          "1.2 GiveSymbol"="1.2",
                          "2.1 TranscribeMulti"="2.1",
                          "2.2 TranscribeC"="2.2",
                          "2.3 TranscribeV"="2.3",
                          "2.4 TranscribeTone"="2.4",
                          "2.5 TranscribeKnown"="2.5",
                          "3.1 ReadVocalTract"="3.1",
                          "3.2 GesturalScore"="3.2",
                          "4.1 SegmentSpectro"="4.1",
                          "4.2 ReadSpectro"="4.2",
                          "4.3 IdentifyDisplay"="4.3",
                          "4.4 ReadFrequency"="4.4",
                          "4.5 HarmonicsFormants"="4.5",
                          "5 Apply"="5"), 
              selected = "1.1", # from the set of choice values
              
  )
)
  
  # Main panel for displaying outputs
main_content <- mainPanel(
    
    # Output: slidebar shown on a total bar of X times needed by a skill
    plotOutput(outputId = "distPlot")
)
  
second_tab <- tabPanel(
  "Progress",
  # Page title
  titlePanel("Skills mastered"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
        sidebar_content, main_content # use the pre-defined panels on this page
  )
)



# Third tab: compare to avg performance ####
# Sidebar panel for inputs
sidebar_content_avg <- sidebarPanel(
  
  # Input: dropdown for the kinds of skills
  selectInput(inputId = "skills", # maybe the name of the column of the number of skills
              label = "Skills",
              # Create the choices that can be selected. e.g. Display "A" and link to value "a"
              choices = c("1.1 DefineSymbol"="1.1",
                          "1.2 GiveSymbol"="1.2",
                          "2.1 TranscribeMulti"="2.1",
                          "2.2 TranscribeC"="2.2",
                          "2.3 TranscribeV"="2.3",
                          "2.4 TranscribeTone"="2.4",
                          "2.5 TranscribeKnown"="2.5",
                          "3.1 ReadVocalTract"="3.1",
                          "3.2 GesturalScore"="3.2",
                          "4.1 SegmentSpectro"="4.1",
                          "4.2 ReadSpectro"="4.2",
                          "4.3 IdentifyDisplay"="4.3",
                          "4.4 ReadFrequency"="4.4",
                          "4.5 HarmonicsFormants"="4.5",
                          "5 Apply"="5"), 
              selected = "1.1", # from the set of choice values
              
  ))

# Main panel for displaying outputs
main_content_avg <- mainPanel(
  
  #### thoughts ####
  # always show class avg performance in the background when a skill is selected
    # will be cumulative based on all quizzes of a particular skill; update weekly
  # Output: show all other students' performance as scatter points; or avg as one single point/line
  # Or just one tab: showing all the scatter points; self's point; total points to be completed
  # histogram will be good: showing how many students reach a particular score; also plot the total score as the limit of the y axis
  # simplest: have two dropdowns: 1. choose self or class avg; 2. which skill
  
  ## how to
  # display a particular student's score with privacy
    # assign a random id at the beginning
    # linking student id with the app: maybe need working with canvas
  # show expected num of times needed to mastery for each different skill
    # ideally on the same page
    # can be a separate table on the other tab
    # a second tab showing skills acquired
      # renderTable; renderText
  
  # discrepancy on excel sheet: displayed scores, but need to show #time acquired
  # >15 = acquired; 1 = acquired one time
  # highlighting a maximum score
  
  plotOutput(outputId = "distPlot")
)

third_tab <- tabPanel(
  "Class",
  titlePanel("What is the avg score in the class?"),
  sidebarLayout(
    sidebar_content_avg, main_content_avg
  )
)


# Define UI for app ####
ui <- fluidPage(
  intro_tab,
  second_tab,
  third_tab
)
ui <- navbarPage(
  "Skill-based grading",
  intro_tab,
  second_tab,
  third_tab
)

# set up server ####
server <- function(input, output) {
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    # color match inputId
    # ggplot(aes(x= , y= , color=.data[[input$skills]], ))+geom_bar()/geom_line
    
    x    <- faithful$waiting
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist(x, breaks = bins, col = "#75AADB", border = "white",
         xlab = "Waiting time to next eruption (in mins)",
         main = "Histogram of waiting times")
    
  })
  
}
