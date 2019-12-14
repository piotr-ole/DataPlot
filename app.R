library(shiny)




ui <- fluidPage(
    titlePanel("Hello Shiny!"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose a CSV file"),
            checkboxInput("heat", "Heatmap", c("heatmap", "barchart", "scatterplot")),
            uiOutput("col"),
            uiOutput("size"),
            
        ),
        mainPanel(
            #plotOutput(outputId = "distPlot")
            
        )
    )
)

server <- function(input, output) {

    
    output$plotSpecification <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            return({selectInput("col", "Color variable", c("None", colnames(read.csv(f$datapath))));
            selectInput("col", "Color variable", c("None", colnames(read.csv(f$datapath)))) })
        }
    })
    
}

shinyApp(ui = ui, server = server)



