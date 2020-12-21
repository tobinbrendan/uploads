#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define UI for dataset viewer app ----
ui <- fluidPage(
    
    # App title ----
    titlePanel("Patient Data"),

        # Well panel for patient id ----
        wellPanel(
            
            # Input: Text for inputting patient ID ----
            textInput(inputId = "patName",
                      label = "Patient ID:",
                      value = "Input Patient ID"),
            br(),
            p(strong("Some IDs for testing purposes:")),
            p("76982e06-f8b8-4509-9ca3-65a99c8650fe"),
            p("d49f748f-928d-40e8-92c8-73e4c5679711")
        ),
        
        # Sidebar panel for data selection ----
        sidebarPanel(
            
            # Input: Selector for choosing dataset ----
            radioButtons(inputId = "dataset",
                        label = "Choose a dataset:",
                        choices = c("Allergies", "Careplan", "Conditions",
                                    "Devices", "Encounters", "Imaging Studies",
                                    "Immunizations", "Medications", "Observations",
                                    "Payer Transitions", "Procedures")),
            width = 2
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            tabsetPanel(type = "tabs",
                        tabPanel("Table", tableOutput("view")),
                        tabPanel("Summary", verbatimTextOutput("summary"))
            )
        )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
    
    # Return the requested dataset
    # Includes possible expansions ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Allergies" = allergies,
               "Careplan" = careplans,
               "Conditions" = conditions,
               "Devices" = devices,
               "Encounters" = encounters,
               "Imaging Studies" = imaging_studies,
               "Immunizations" = immunizations,
               "Medications" = medications,
               "Observations" = observations,
               #"Organizations" = organizations,
               #"Patients" = patients,
               "Payer Transitions" = payer_transitions,
               #"Payers" = payers,
               "Procedures" = procedures,
               #"Providers" = providers,
        )
    })
        
        # Show the selected dataset for only the given patient
        # Filters for the patient then removes patient title for redundancy ----
        output$view <- renderTable({
            select(filter(datasetInput(), PATIENT == input$patName), -PATIENT)
        })
        
        # Generate a summary of the dataset ----
        output$summary <- renderPrint({
            filtData <- select(filter(datasetInput(), PATIENT == input$patName),
                              -PATIENT)
            summary(filtData)
        })

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
