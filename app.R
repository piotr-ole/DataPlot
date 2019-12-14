library(shiny)
library(dplyr)
library(ggthemes)



ui <- fluidPage(
    titlePanel("Hello Shiny!"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Choose a CSV file"),
            checkboxInput(inputId = "isHeat", label = "Heatmap"),
            checkboxInput(inputId = "isBar", label = "Barchart"),
            checkboxInput(inputId = "isScat", label = "Scatterplot"),
            #uiOutput("heatmap"),
            conditionalPanel(
                condition = "input.isScat == true",
                htmlOutput("scatterHeader"),
                uiOutput("scatterUI")
            )
            #uiOutput("barchart"),
        ),
        mainPanel(
            plotOutput("scatterPlot")
        )
    )   
)

server <- function(input, output) {

    output$scatterHeader <- renderUI(HTML("<br><h4><b>Scatter plot</b></h4><br>"))
    
    output$scatterUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            list(
                selectInput("xScat", "x-axis variable", c("None", colnames(read.csv(f$datapath)))),
                selectInput("yScat", "y-axis variable", c("None", colnames(read.csv(f$datapath)))),
                selectInput("colScat", "Color variable", c("None", colnames(read.csv(f$datapath)))),
                selectInput("sizeScat", "Size variable", c("None", colnames(read.csv(f$datapath)))),
                actionButton("buttonScat", "Generate plot")
            )
        }
    })
    observeEvent(input$buttonScat, {  
    output$scatterPlot <- renderPlot({
        if (input$isScat == TRUE) {
            if (!is.null(input$file)) {
                f <- input$file
                dat <- read.csv(f$datapath, stringsAsFactors = TRUE)
                dat <- dat %>% 
                    mutate(color_ = rep("#2ca25f", nrow(dat))) %>%
                    mutate(size_ = rep(1, nrow(dat)))
                ggplot(data =  dat, 
                       aes(x = eval(sym(input$xScat)),
                           y = eval(sym(input$yScat)),
                           col = eval(set_value(input$colScat, color_)),
                           size = eval(set_value(input$sizeScat, size_))
                       )) + 
                    geom_point() + 
                    theme_fivethirtyeight() +
                    ggtitle(paste0("Scatter plot ", input$yScat, "~", input$xScat)) +
                    xlab(input$xScat) +
                    ylab(input$yScat) +
                    theme(plot.title = element_text(hjust = 0.5), 
                          axis.title = element_text())
            }
        }
    })
    })
    
}

shinyApp(ui = ui, server = server)

set_value <- function(value, standard) {
    if (value != 'None') {
        return(sym(value))
    }
    standard
}
