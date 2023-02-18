#### Skill-based grading ####
## Yaqian Huang; 10/28/2022; huangyq@ucla.edu ##


# install.packages("shiny")
# install.packages("rsconnect")  # For publishing apps online
# if want plotly; change ui to plotlyOutput(); server to renderPlotly()

# load data and libraries ####
library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(tidyr)
library(plotly) # For interactive plots
library(reshape2)

################DATA PART##################################################
# must modify #
# load df & drop nas of first column
#data <- read.csv('ForKie_20F-LING103-Grades.csv')%>%drop_na(c(1))

# drop NA from the first col - usually student ID
data <- read.csv('2023-02-17T1435_Grades-23W-LING-103-LEC-1.csv')%>%drop_na(c(1))

data <- data%>%filter(Section != '23W-LING-103-LEC-1')
data <- slice(data, 2:(n()-1))

# this is for adding a bar for the maximum skill
# may not be realistic because skills are updating each week, not all skills available early on
# if want: add a if condition for placeholding the unavailable skills in the for loop
## if is null, then add this row, and assign 0 to values
## if not null, then continue

# for now: manually adding the unavailale skills and assign '0' so that the dropdown has all skills
data$'2.4.TranscribeTone' <- '0'
data$'3.2.GesturalScore' <- '0'

# associate with ID with a random key assigned to students beforehand ####
# or import from somewhere in an order matching student id

# keys <- c("")
# 
# df$keys <- keys

data['key'] <- c('yowie',
                 'serow',
                 'yelting',
                 'kyloe',
                 'rach',
                 'petrel',
                 'francolin',
                 'amberjack',
                 'tamaraw',
                 'kouprey',
                 'gelada',
                 'kleenebok',
                 'lechwe',
                 'yakow',
                 'oropendola',
                 'lutung',
                 'agouti',
                 'thamin',
                 'adzebill',
                 'takin',
                 'raad',
                 'addax',
                 'jiboya',
                 'cusk',
                 'goral',
                 'dabchick',
                 'prion',
                 'kiang',
                 'leopon',
                 'wapiti',
                 'ipiti',
                 'dobsonfly',
                 'borzoi',
                 'quokka',
                 'blenny',
                 'shearling',
                 'scanderoon',
                 'klipspringer',
                 'caple')

# transpose the df into long format
#data.long <- gather(data[c(-1)],item,score,`Quiz.Quiz1...1.1.DefineSymbol..Real.`:`Course.total..Real.`,factor_key=T)

#data.long <- gather(data[-c(1:5)],item,score,`Quiz1...1.1.DefineSymbol..1354238.`:`Unposted.Final.Grade`,factor_key=T)

# modify according to the colnames that R read in
# use
colnames(data)
# use the 1st col of quiz and last col before 'key'
data.long <- gather(data[-c(1:5)],item,score,`Quiz1...1.1.DefineSymbol..1354238.`:`3.2.GesturalScore`,factor_key=T)

# # alternative
# test <- melt(data[c(-2)],id.vars=c('ID.number'))

# generate a new data frame with groups by skill name
# don't count in total if there's one
skillnum <- c('1.1.DefineSymbol',
              '1.2.GiveSymbol',
              '2.1.TranscribeMulti',
              '2.2.TranscribeC',
              '2.3.TranscribeV',
              '2.4.TranscribeTone',
              '2.5.TranscribeKnown',
              '3.1.ReadVocalTract',
              '3.2.GesturalScore',
              '4.1.SegmentSpectro',
              '4.2.ReadSpectro',
              '4.3.IdentifyDisplay',
              '4.4.ReadFrequency',
              '4.5.HarmonicsFormants',
              '5.Apply')

# CORE FUNCTION

tempsum=NULL
# ,!grepl('total',data.long$item) not using total
for (num in skillnum) {
  temp <- data.long%>%mutate('skill'=num)%>%filter(grepl(num,data.long$item))%>%
    group_by(key)%>%mutate("num"=sum(as.numeric(score),na.rm = T))%>%select(-c(2,3))%>%
    dplyr::slice(1:length(unique(key)))
  tempsum <- rbind(temp,tempsum)
}

df <- tempsum

# extract skill number & label as separate cols
df$skillnum <- gsub('[[:alpha:]]', '', df$skill)
df$label <- gsub('[.0-9]+', '', df$skill)
# [^[:alpha:]]+
  
# # if using total as the only column
# df <- data.long%>%filter(grepl('total',data.long$item))
# # in server: use %in% to match skill name instead of ==

# match with specific #times of mastery to skills
df <- df%>%arrange(skill)%>%mutate(highlight = c("15","10","25","10","5","5","30","5","5","7","7","2","2",
                       "2","5"))
df$highlight <- as.numeric(df$highlight)



################APP PART##################################################
# do not modify unless specified #

# Define each panel, element ####

# Intro tab: how to use the app ----
intro_tab <- tabPanel(
  title="Instructions", # tab name
  
  # Page title
  titlePanel("How to use this app"),
  
  #img(src = "[img source]"),
  br(),
  
  p("Enter your student key to access your grades."),
  
  br(),
  
  p("Your grade is highlighted in", span("purple",style='color:magenta'), 
    "dots with scores in", span("blue text",style='color:blue'), "overlaid.",
    br(),
    "The", span("gray",style='color:gray'), "dots are distributions of the grades as a class.", br(),
    "The star indicates the required # of times towards mastery of a skill."),
  br(),
  
  p(a(href = "[https://sites.google.com/ucsd.edu/yaqianhuang/home]", 
      "Designed by © Yaqian Huang 2/17/2023")))

# Second tab: check own performance ####
# Sidebar panel for inputs
sidebar_content <- sidebarPanel(
  
  textInput(inputId = "id", label = h3("Student key"), value = "Enter your key..."),
  
  # Input: dropdown for the kinds of skills
  # May change if different skills BUT must change the skillnum in DATA too
  
  selectInput(inputId = "skills", # to match server output color
              label = "Select a skill",
              # Create the choices that can be selected. e.g. Display "A" and link to value "a"
              choices = c("1.1 DefineSymbol"="1.1.",
                          "1.2 GiveSymbol"="1.2.",
                          "2.1 TranscribeMulti"="2.1.",
                          "2.2 TranscribeC"="2.2.",
                          "2.3 TranscribeV"="2.3.",
                          "2.4 TranscribeTone"="2.4.",
                          "2.5 TranscribeKnown"="2.5.",
                          "3.1 ReadVocalTract"="3.1.",
                          "3.2 GesturalScore"="3.2.",
                          "4.1 SegmentSpectro"="4.1.",
                          "4.2 ReadSpectro"="4.2.",
                          "4.3 IdentifyDisplay"="4.3.",
                          "4.4 ReadFrequency"="4.4.",
                          "4.5 HarmonicsFormants"="4.5.",
                          "5 Apply"="5."), 
              selected = "NA", # from the set of choice values
              
  )
  
  # hr(),
  # fluidRow(column(3, verbatimTextOutput("value")))
)
  
  # Main panel for displaying outputs
main_content <- mainPanel(
    
    # Output: scale shown on a X out of total times needed by a skill
    # highlight maximum, self score
    # plot others as scatter plot (or as average)
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



# Third tab: show skills mastery appendix ####
# Main panel for displaying table
main_content_app <- mainPanel(
  
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
  
  tableOutput(outputId = "appendix")
)


third_tab <- tabPanel(
  "Reference",
  titlePanel("Skill mastery appendix"),
 main_content_app

)


# Define UI for app ####
# ui <- fluidPage(
#   intro_tab,
#   second_tab,
#   third_tab
# )
ui <- navbarPage(
  "Skill-based grading",
  intro_tab,
  second_tab,
  third_tab
)

# set up server ####
# server <- function(input, output) {
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
#   output$distPlot <- renderPlot({
#     
#     # filter selected skill
#     # filter student id/key/text
#     # y = cumulative skill score (easier thru excel);
#       # otherwise: sum scores of a particular col containing # skill (e.g.,1.1)
#     # scatter plot or average
#     # color match studentID or doesn't need color
#     # highlight ind student; others gray; maximum black
#     # ggplot(aes(x= , y= , color=.data[[input$skills]], ))+geom_bar()/geom_line
#     
#     plotstyle = list(
#       theme_light(base_size=32),
#       theme(panel.border = element_rect(color = 'black',fill=NA))
#     )
#     ggplot(df, aes(x = skill,y=num)) +
#       geom_point(fill = "grey",
#                      data = df%>%filter(skill%in%input$skills),
#                      colour = "grey",size=3)+
#       geom_point(fill = "red",
#                  data = df%>%filter(`ID.number` == input$id),
#                  colour = "red",size=10)+
#       geom_label(data = df%>%filter(`ID.number` == input$id,skill%in%input$skills),
#                  aes(label=num),size=8,color='blue',nudge_x = .3)+
#       labs(y='# Times demonstrated')+plotstyle
#       
#   })
#  
#   output$appendix <- renderTable(list(skills = c("1.1 DefineSymbol","1.2 GiveSymbol",
#                                                                 "2.1 TranscribeMulti",
#                                                                 "2.2 TranscribeC",
#                                                                 "2.3 TranscribeV",
#                                                                 "2.4 TranscribeTone",
#                                                                 "2.5 TranscribeKnown",
#                                                                 "3.1 ReadVocalTract",
#                                                                 "3.2 GesturalScore",
#                                                                 "4.1 SegmentSpectro",
#                                                                 "4.2 ReadSpectro",
#                                                                 "4.3 IdentifyDisplay",
#                                                                 "4.4 ReadFrequency",
#                                                                 "4.5 HarmonicsFormants",
#                                                                 "5 Apply"),
#                                       `#Time to mastery` = c("15","10","25","10","5","5","30","5","5","7","7","2","2",
#                                                         "2","5"))
#                                    
#                                  ) 
#   #output$value <- renderPrint({ input$text })
# }

server <- function(input, output) {
  
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    # filter selected skill
    # filter student id/key/text
    # y = cumulative skill score (easier thru excel);
    # otherwise: sum scores of a particular col containing # skill (e.g.,1.1)
    # scatter plot or average
    # color match studentID or doesn't need color
    # highlight ind student; others gray; maximum black
    # ggplot(aes(x= , y= , color=.data[[input$skills]], ))+geom_bar()/geom_line
    
    #May change according to preference
    
    plotstyle = list(
      theme_light(base_size=24),
      theme(panel.border = element_rect(color = 'black',fill=NA),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 20),
            axis.title.x = element_text(size=24))
    )
    ggplot(df, aes(y = skill,x=num)) +
      #geom_segment(aes(x=as.numeric(highlight),y=0,xend=as.numeric(highlight)+2,yend=0)) +
      geom_point(#data = df%>%filter(`key` == input$id),
                 fill = "slateblue",
                 #data = df%>%filter(skillnum%in%input$skills),
                 colour = "slateblue",size=6,shape=8,
                 aes(y=skill,x=highlight))+
      geom_point(fill = "grey",
                 data = df%>%filter(skillnum%in%input$skills),
                 colour = "grey",size=3)+
      geom_point(fill = "plum2",
                 data = df%>%filter(`key` == input$id),
                 colour = "plum2",size=8)+
      geom_text(data = df%>%filter(`key` == input$id,skillnum%in%input$skills),
                 aes(label=num),size=6,color='navy',fontface='bold')+
      # geom_label(data = df%>%filter(`key` == input$id),
      #            fill = "cyan",
      #            #data = df%>%filter(skillnum%in%input$skills),
      #            colour = "cyan",size=5,#shape=8,
      #            aes(label=highlight))+
      
      #,vjust = 2+0.1*as.numeric(num))+
      labs(x='# Times demonstrated')+plotstyle
    
  }) 
  
  #May change according to different requirements for a particular course
  output$appendix <- renderTable(list(skills = c("1.1 DefineSymbol","1.2 GiveSymbol",
                                                 "2.1 TranscribeMulti",
                                                 "2.2 TranscribeC",
                                                 "2.3 TranscribeV",
                                                 "2.4 TranscribeTone",
                                                 "2.5 TranscribeKnown",
                                                 "3.1 ReadVocalTract",
                                                 "3.2 GesturalScore",
                                                 "4.1 SegmentSpectro",
                                                 "4.2 ReadSpectro",
                                                 "4.3 IdentifyDisplay",
                                                 "4.4 ReadFrequency",
                                                 "4.5 HarmonicsFormants",
                                                 "5 Apply"),
                                      `#Time to mastery` = c("15","10","25","10","5","5","30","5","5","7","7","2","2",
                                                             "2","5"))
                                 
  ) 
  #output$value <- renderPrint({ input$text })
}
  
#### always last
shinyApp(ui = ui, server = server)
