library(shiny)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(shinycssloaders)
library(shinyhelper)
#devtools::install_github('raredd/rawr')
library(rawr)

ui <- fluidPage(
    titlePanel("Plot your data!"),
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
            ),
            #uiOutput("barchart"),
            conditionalPanel(
                condition = "input.isBar == true",
                htmlOutput("barHeader"),
                uiOutput("barUI")
            )
        ),
        mainPanel(
            plotOutput("scatterPlot",
                       dblclick = "plot1_dblclick",
                       brush = brushOpts(
                           id = "plot1_brush",
                           resetOnNew = TRUE
                       )) %>% 
                helper(type = "markdown", content = "Scatterplot", title = "Scatterplot") %>%
                withSpinner(color="#0dc5c1"),
            plotOutput("barPlot",
                       dblclick = "plot1_dblclick",
                       brush = brushOpts(
                           id = "plot2_brush",
                           resetOnNew = TRUE
                       ))# %>% 
                #helper(type = "markdown", content = "Scatterplot", title = "Scatterplot") %>%
                #withSpinner(color="#0dc5c1")
            
        )
    )   
)

server <- function(input, output) {
    
    observe_helpers()
    
    ranges <- reactiveValues(x = NULL, y = NULL )
    
    # Scatterplot
    output$scatterHeader <- renderUI(HTML("<br><h4><b>Scatter plot</b></h4><br>"))
    
    output$scatterUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui('Scat', rep(TRUE, 5), f$datapath)
        }
    })
    
    output$barHeader <- renderUI(HTML("<br><h4><b>Bar plot</b></h4><br>"))
    
    output$barUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui('Bar', c(TRUE,TRUE,TRUE,TRUE,FALSE), f$datapath)
        }
    })
    
    observeEvent(input$buttonScat, {  
    output$scatterPlot <- renderPlot({
        if (input$isScat == TRUE) {
            if (!is.null(input$file)) {
                # data processing
                f <- input$file
                dat <- read.csv(f$datapath, stringsAsFactors = TRUE)
                dat <- dat %>% 
                    mutate(color_ = rep("#2ca25f", nrow(dat))) %>%
                    mutate(size_ = rep(1, nrow(dat)))
                # custom legend
                legend <- get_legend(input$colScat, input$sizeScat, input$fillScat)
                my_aes <- get_aes(input)
                #browser()
                ggplot(data =  dat, 
                       mapping = my_aes
                       ) + 
                    geom_point() + 
                    theme_fivethirtyeight() +
                    ggtitle(paste0("Scatter plot ", input$yScat, "~", input$xScat)) +
                    xlab(input$xScat) +
                    ylab(input$yScat) +
                    theme(plot.title = element_text(hjust = 0.5), 
                          axis.title = element_text(),
                          panel.background = element_blank(),
                          plot.background = element_blank(),
                          legend.background = element_blank()) +
                    coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
                    guides(color = guide_legend(title = legend$color),
                           size = guide_legend(title = legend$size),
                           fill = guide_legend(title = legend$fill))
                
            }
        }
    })
    })
    
    
    observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
}

get_legend <- function(colValue, sizeValue, fillValue) {
    if (colValue != 'None') color <- colValue
    else color <- ""
    if (sizeValue != 'None') size <- sizeValue
    else size <- ""
    if (fillValue != 'None') fill <- fillValue
    else fill <- ""
    list(color = color, size = size, fill = fill)
}

set_value <- function(value, standard) {
    if (value != 'None') {
        return(sym(value))
    }
    standard
}

get_aes <- function(input) {
    names = c("xScat", "yScat", "colScat", "sizeScat", "fillScat")
    mappings <- c("x", "y", "col", "size", "fill")
    my_aes <- list()
    for (name in names) {
        if (input[[name]] != 'None') {
            if (name %in% c('xScat', 'yScat')) {
                my_aes <- c(my_aes, sprintf("eval(sym(input$%s))", name))
            } else {
                my_aes <- c(my_aes, sprintf("eval(set_value(input$%s, NULL))", name))
            }
        }
        else my_aes <- c(my_aes, 'None')
    }
    #browser()
    names(my_aes) <- mappings
    my_aes <- my_aes[if_else(my_aes != 'None', TRUE, FALSE)]
    do.call(aes_string, my_aes)
}

create_ui <- function(shortname = "Scat", attributes_logicals, data_file) {
    # logicals vector of true false for (x,y,col, fill, size)
    attrs <- c("x", "y", "col", "fill" ,"size")
    descriptions <- c("x-axis variable", "y-axis variable", "Color variable",
                      "Fill variable", "Size variable")
    good_attrs <- attrs[attributes_logicals]
    good_descr <- descriptions[attributes_logicals]
    
    options <- colnames(read.csv(data_file))
    
    l <- lapply(seq(good_attrs), function(i, good_descr, good_attrs, shortname, options) { 
        selectInput(
            paste0(good_attrs[i], shortname, collapse = ''),
            good_descr[i],
            c('None', options)
            ) }, good_descr, good_attrs, shortname, options)
    
    l <- merge_lists(l, actionButton(paste0("button", shortname), "Generate plot"))
    l
}

shinyApp(ui = ui, server = server)

