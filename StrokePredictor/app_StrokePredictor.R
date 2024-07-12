# load packages
library(tibble)
library(shiny)
library(shinythemes)
library(workflows)
library(recipes)
library(markdown)
library(ranger)

# Read in the RF model
model <- readRDS("model.rds")
####################################
# User interface                   #
####################################

ui <- fluidPage(
  theme = shinytheme("superhero"),
  navbarPage(
    "Stroke Predictor:",
    tabPanel("Home",
             sidebarPanel(
               tags$h3("Patient Data:"),
               selectInput("age_group",
                           "Age Group",
                           choices = c('0-5y', '6-18y', '19-45y', '46-64y', '65-75y', '76+')),
               selectInput("bmi_group",
                           "BMI Class",
                           choices = c("underweight", "normal", "overweight", "class I obesity", "class II obesity", "class III obesity")),
               selectInput("glucose_group",
                           "GLucose Levels",
                           choices = c("Normal", "Prediabetic", "Diabetic")),
               radioButtons("hypertension",
                            label = "Hpertension (yes = 1, no = 0)",
                            choices = c("1", "0")),
               radioButtons("heart_disease",
                            label = "Heart Disease (yes = 1, no = 0)",
                            choices = c("1", "0")),
               actionButton("submitbutton", 
                            "Submit", 
                            class = "btn btn-primary")),
             
             mainPanel(tags$label(h3('Status/Output')), # Status/Output Text Box
                       verbatimTextOutput('contents'),
                       tableOutput('tabledata')) # Prediction results table))
    ),
    tabPanel("About", 
             titlePanel("About"), 
             div(includeMarkdown("aboutApp.md"), 
                 align="justify")
)))

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # tribble
    df <- tribble( ~age_group, ~bmi_group, ~glucose_group, ~hypertension, ~heart_disease,
                           input$age_group, input$bmi_group, input$glucose_group, input$hypertension, input$heart_disease)
  
    Output <- data.frame(Prediction=predict(model,df), round(predict(model,df,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete ( 0 = no risk of stroke; 1 = at risk of stroke)") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)