#### Skill-based grading for 103 Intro to Phonetics ####
## Yaqian Huang; originated 10/28/2022; huangyq@ucla.edu ##
## last version: 6/9/2023 ##


# install.packages("shiny")
# install.packages("rsconnect")  # For publishing apps online

# load data and libraries ####
library(shiny)
library(ggplot2)  # For creating pretty plots
library(dplyr)  # For filtering and manipulating data
library(tidyr)
library(reshape2)

################PREP PART##################################################
# You will need:
# 1. csv from BruinLearn with all the grades
# 2. skills.csv which has random student keys matching their names in an alphabetical order
# 3. studentIDs.csv which has skills and # of times of mastery
# See template files in this folder

################DATA PART##################################################
# uncomment when in debug mode:
# setwd("Your working directory")
# for example:
# setwd("/Users/Yaqian/Library/CloudStorage/GoogleDrive-huangyq@g.ucla.edu/My Drive/p-lab/skgb/demo")

# MUST modify when prompted#
# Download from BruinLearn -> Grades -> Export...
# Load csv with grades
# Replace with your filename:
data <- read.csv('ng-2023-03-16T1433_Grades-23W-LING-103-LEC-1.csv')

# Filter enrolled students (excluding auditing, grads, etc.)
# multiple ways: but enrolled students will be in a Discussion Section

data$Section # check which sections are in your course
# Replace with the sections in your course:
data <- data%>%filter(Section %in% c('23W-LING-103-DIS-1A','23W-LING-103-DIS-1B',
                                     '23W-LING-103-DIS-1C','23W-LING-103-DIS-1D'))

# prepare skills.csv with all the skills, and # of times of mastery
# DO NOT modify the following codes, modify the skills.csv sheet
# Load csv with skills

skills <- read.csv('skills.csv')
skills$skill # check this

# NOTE: you have to have the . after the skill num, so 1.1., not 1.1
# be consistent with the skill in the quiz name on BruinLearn

# as skills are updating each week, not all skills are available early on
# the following function checks whether the current week has all the skills
# if not, it will create a column of that skill and assign a 0 to the values

checkPartialColumnNames <- function(vector, dataframe) {
  for (element in vector) {
    match_found <- FALSE
    
    for (column_name in colnames(dataframe)) {
      if (grepl(element, column_name)) {
        match_found <- TRUE
        break
      }
    }
    
    if (!match_found) {
      dataframe[[element]] <- 0
    }
  }
  
  return(dataframe)
}

data <- checkPartialColumnNames(skills$skill,data)

# prepare studentIDs.csv which has random keys assigned to students beforehand ####
# DO NOT modify the following codes, modify the studentIDs.csv sheet
# import this csv with student keys in an alphabetical order matching student names

student_key <- read.csv('studentIDs.csv')

# a function check if student names exist in data, add the key, 
# if not, remove that student and then add the key

merge_sheets <- function(sheet1, sheet2) {
  # Check if both sheets have the "Student" column
  if (!("Student" %in% colnames(sheet1)) || !("Student" %in% colnames(sheet2))) {
    stop("Both sheets must have a 'Student' column.")
  }
  
  # Find student names in sheet2 that are not present in sheet1
  missing_names <- setdiff(sheet2$Student, sheet1$Student)
  
  # Remove rows with missing names in sheet2
  sheet2_filtered <- sheet2[!sheet2$Student %in% missing_names, ]
  
  # Merge the two sheets based on the "Student" column
  merged <- merge(sheet1, sheet2_filtered, by = "Student", all.x = TRUE)
  
  # Rename the key column
  colnames(merged)[ncol(merged)] <- "key"
  
  return(merged)
}

data <- merge_sheets(data, student_key)


# Transpose the df into long format
# MODIFY when prompted, according to the colnames that R read in
# use
colnames(data)

# inspect the dataset: there are usually 5 columns of meta-data in the beginning to remove
# with this template, only 2 cols because I already removed the student contacts and IDs
data.long <- gather(data[-c(1:2)],item,score,colnames(data)[3]:colnames(data)[153],factor_key=T)

# you should select from the 1st col of data after removing the meta data to the last col before the 'key'
# Replace the `second-to-last col indx` with a real number based on your data in line 123
# and then Uncomment it:
# data.long <- gather(data[-c(1:5)],item,score,colnames(data)[6]:colnames(data)[`second-to-last col indx`],factor_key=T)

# Once you uncomment line 123 (delete # before the code), Turn line 118 into a comment (type # before the code)


# CORE FUNCTION
# IDEA: generate a new data frame with groups by skill name

tempsum=NULL
for (num in skills$skill) {
  temp <- data.long%>%mutate('skill'=num)%>%filter(grepl(num,data.long$item))%>%
    group_by(key)%>%mutate("num"=sum(as.numeric(score),na.rm = T))%>%select(-c(2,3))%>%
    # may need to change what to de-select, rm any cols between 'key' and 'skill'
    dplyr::slice(1:length(unique(key)))
  tempsum <- rbind(temp,tempsum)
}

df <- tempsum

# extract skill number & label as separate cols
df$skillnum <- gsub('[[:alpha:]]', '', df$skill)
df$label <- gsub('[.0-9]+', '', df$skill)
  
# add a column named "highlight" for specific #times of mastery of particular skills
df <- df%>%arrange(skill)%>%mutate(highlight = as.numeric(skills$times))

# may want to add a percentage
# note percentage is a character variable with %, not a numeric variable.
df$percentage <- ifelse(df$num*100/df$highlight>100,'100%',paste0(round(df$num*100/df$highlight,2), '%'))


################APP PART##################################################
# DO NOT modify unless specified #

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
      "Designed by © Yaqian Huang 2/17/2023, modified 6/9/2023")),
  br(),
  
  p("Grades up to date 3/16/2022")) # may want to change dates every time updating####


# Second tab: check own performance ####
# Sidebar panel for inputs
sidebar_content <- sidebarPanel(
  
  textInput(inputId = "id", label = h3("Student key"), value = "Enter your key..."),
  
  # Input: dropdown for the kinds of skills
  # MODIFY: The skills need to be exactly the same as the skills$skill vector in DATA part ####
  
  selectInput(inputId = "skills", # to match server output
              label = "Select a skill",
              # Create the choices that can be selected.
              # follow the format: "Skill1 = SkillNumber1"
              choices = c("1.1.DefineSymbol"="1.1.",
                          "1.2.GiveSymbol"="1.2.",
                          "2.1.TranscribeMulti"="2.1.",
                          "2.2.TranscribeC"="2.2.",
                          "2.3.TranscribeV"="2.3.",
                          "2.4.TranscribeTone"="2.4.",
                          "2.5.TranscribeKnown"="2.5.",
                          "3.1.ReadVocalTract"="3.1.",
                          "3.2.GesturalScore"="3.2.",
                          "4.1.SegmentSpectro"="4.1.",
                          "4.2.ReadSpectro"="4.2.",
                          "4.3.IdentifyDisplay"="4.3.",
                          "4.4.ReadFrequency"="4.4.",
                          "4.5.HarmonicsFormants"="4.5.",
                          "5.Apply"="5."), 
              selected = "NA", # from the set of choice values
              
  )

)
  
  # Main panel for displaying outputs
main_content <- mainPanel(
    
    plotOutput(outputId = "scatterPlot")
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
  
  tableOutput(outputId = "appendix")
)


third_tab <- tabPanel(
  "Reference",
  titlePanel("Skill mastery appendix"),
 main_content_app

)


# Define UI for app ####

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
  output$scatterPlot <- renderPlot({
    
    # options:
    # filter selected skill
    # filter student id/key/text
    # display cumulative skill score or percentage

    # Maybe MODIFY: change according to preference
    
    plotstyle = list(
      theme_light(base_size=24),
      theme(panel.border = element_rect(color = 'black',fill=NA),
            axis.title.y = element_blank(),
            axis.text.y = element_text(size = 20),
            axis.title.x = element_text(size=24))
    )
    ggplot(df%>%filter(key!='goral'), aes(y = skill,x=num)) +
      # always displaying the # of times of mastery of all skills as stars
      geom_point(fill = "slateblue",
                 colour = "slateblue",size=6,shape=8,  # stipulating color, size, shape of the points
                 aes(y=skill,x=highlight))+
      # displaying everyone's grade in this class of all skills as grey dots
      geom_point(fill = "grey",
                 data = df%>%filter(key!='goral'),
                 colour = "grey",size=3)+
      # displaying the grades of a particular student searched by key as plum dots
      geom_point(fill = "plum2",
                 data = df%>%filter(`key` == input$id,skillnum%in%input$skills),
                 colour = "plum2",size=8)+
      geom_text(data = df%>%filter(`key` == input$id,skillnum%in%input$skills),
                aes(label=percentage),size=6,color='navy',fontface='bold')+
      # if you want to show everything all at once, delete the selectInput code 196-217,
      # and replace codes in 299-303 with the following codes 306-310:
      # geom_point(fill = "plum2",
      #            data = df%>%filter(`key` == input$id),
      #            colour = "plum2",size=8)+
      # geom_text(data = df%>%filter(`key` == input$id),
      #            aes(label=percentage),size=6,color='navy',fontface='bold')+
      
      # you can choose to display exact grades or percentage by changing "label=X"
      # for example, you can display exact grades using "label=num"
      labs(x='# Times demonstrated')+plotstyle
    
  }) 
  
  # again, make this consistent with skills else where for a particular course
  # reuse the table in skills.csv
  output$appendix <- renderTable(skills)
  
}
  
#### always last
shinyApp(ui = ui, server = server)
