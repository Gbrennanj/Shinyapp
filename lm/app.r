#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("BSGP LM Dasboard"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(

            
            # Input: Select a file ----
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Checkbox if file has header ----
            checkboxInput("header", "Header", TRUE),
            
            # Input: Select separator ----
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            # Input: Select quotes ----
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            # Input: Select number of rows to display ----
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),
             tags$hr(),
            actionButton("go", "Plot Linear Model"), 
            
         ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("origPlot"),
           plotOutput("lmPlot"),
           tableOutput("contents")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    lmdata <- reactiveValues()

    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })

    observeEvent(input$go,{
        update_lm()
    }) 

    update_lm <- function(){
        lmdata$model <- lm(y~x, data = dataInput())
        lmdata$rsq <- summary(lmdata$model)$r.squared
        lmdata$slope <- summary(lmdata$model)$coef[2]
        lmdata$intercept <- summary(lmdata$model)$coef[1]
    }
    
    output$origPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, xlab="X", ylab="Y")
        
    })
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, xlab="X", ylab="Y")
        abline(lmdata$model) 
        legend("topleft",legend=paste("R2 is", format(lmdata$rsq,digits=3)))
        legend("topright", legend=paste("intercept is", format(lmdata$intercept,digits=3)))
        legend("bottomright", legend=paste("slope is", format(lmdata$slope,digits=3)))
    })

    output
    
    
    output$contents <- renderTable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
