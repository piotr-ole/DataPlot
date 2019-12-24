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
            checkboxInput(inputId = "isScat", label = "Scatterplot"),
            checkboxInput(inputId = "isBar", label = "Barchart"),
            checkboxInput(inputId = "isHeat", label = "Heatmap"),
            # scatterplot
            conditionalPanel(
                condition = "input.isScat == true",
                htmlOutput("scatterHeader"),
                uiOutput("scatterUI")
            ),
            # barchart
            conditionalPanel(
                condition = "input.isBar == true",
                htmlOutput("barHeader"),
                uiOutput("barUI")
            ),
            # heatmap
            conditionalPanel(
                condition = "input.isHeat == true",
                htmlOutput("heatHeader"),
                uiOutput("heatUI")
            )
            
        ),
        mainPanel(
            conditionalPanel(
                condition = "input.isScat == true",
                plotOutput("scatterPlot",
                           dblclick = "plot1_dblclick",
                           brush = brushOpts(
                               id = "plot1_brush",
                               resetOnNew = TRUE
                           )) %>% 
                    helper(type = "markdown", content = "Scatterplot", title = "Scatterplot")
                ),
            conditionalPanel(
                condition = "input.isBar == true",
                plotOutput("barPlot",
                           dblclick = "plot2_dblclick",
                           brush = brushOpts(
                               id = "plot2_brush",
                               resetOnNew = TRUE
                           ))
            ),
            conditionalPanel(
                condition = "input.isHeat == true",
                plotOutput("heatPlot",
                           dblclick = "plot3_dblclick",
                           brush = brushOpts(
                               id = "plot3_brush",
                               resetOnNew = TRUE
                           ))
                )# %>% 
                #helper(type = "markdown", content = "Scatterplot", title = "Scatterplot") %>%
                #withSpinner(color="#0dc5c1")
            
        )
    )   
)

server <- function(input, output) {
    
    observe_helpers()
    
    ranges <- reactiveValues(x = NULL, y = NULL )
    
    # Scatterplot UI
    output$scatterHeader <- renderUI(HTML("<br><h4><b>Scatter plot</b></h4><br>"))
    
    output$scatterUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui('Scat', rep(TRUE, 5), f$datapath)
        }
    })
    
    # barplot UI
    output$barHeader <- renderUI(HTML("<br><h4><b>Bar plot</b></h4><br>"))
    
    output$barUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui('Bar', c(TRUE,TRUE,TRUE,TRUE,TRUE), f$datapath)
        }
    })
    
    # heatmap UI
    output$heatHeader <- renderUI(HTML("<br><h4><b>Heatmap plot</b></h4><br>"))
    
    output$heatUI <- renderUI({
        if (!is.null(input$file)) {
            f <- input$file
            create_ui('Heat', c(TRUE,TRUE,TRUE,TRUE,TRUE), f$datapath)
        }
    })
    
    observeEvent(input$buttonScat, {  
    output$scatterPlot <- renderPlot({
        if (input$isScat == TRUE) {
            if (!is.null(input$file)) {
                # data processing
                f <- input$file
                create_plot(input, f$datapath, 'Scat', 'Scatterplot', 'geom_point' ,ranges)
            }
        }
    })
    })
    
    observeEvent(input$buttonBar, {
    output$barPlot <- renderPlot({
        if (input$isBar == TRUE) {
            if (!is.null(input$file)) {
                #browser()
                # data processing
                f <- input$file
                dat <- read.csv(f$datapath, stringsAsFactors = TRUE)
                # custom legend
                legend <- get_legend(input, 'Bar')
                my_aes <- get_aes(input, 'Bar')
                ggplot(data =  dat, 
                       mapping = my_aes
                ) + 
                    geom_bar() + 
                    theme_fivethirtyeight() +
                    ggtitle("Barplot")+
                    xlab(input$xBar) +
                    ylab(input$yBar) +
                    theme(plot.title = element_text(hjust = 0.5), 
                          axis.title = element_text(),
                          panel.background = element_blank(),
                          plot.background = element_blank(),
                          legend.background = element_blank()) +
                    #coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE) +
                    guides(color = guide_legend(title = legend$color),
                           size = guide_legend(title = legend$size),
                           fill = guide_legend(title = legend$fill))
                
            }
        }
    })
    })
    
    observeEvent(input$buttonHeat, {
        output$heatPlot <- renderPlot({
            if (input$isHeat == TRUE) {
                if (!is.null(input$file)) {
                    #browser()
                    # data processing
                    f <- input$file
                    dat <- read.csv(f$datapath, stringsAsFactors = TRUE)
                    # custom legend
                    legend <- get_legend(input, 'Heat')
                    my_aes <- get_aes(input, 'Heat')
                    ggplot(data =  dat, 
                           mapping = my_aes
                    ) + 
                        geom_tile() + 
                        theme_fivethirtyeight() +
                        ggtitle("Heatmap")+
                        xlab(input$xHeat) +
                        ylab(input$yHeat) +
                        theme(plot.title = element_text(hjust = 0.5), 
                              axis.title = element_text(),
                              panel.background = element_blank(),
                              plot.background = element_blank(),
                              legend.background = element_blank()) +
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


get_legend <- function(input, shortname) {
    colValue <- input[[v('col', shortname)]]
    sizeValue <- input[[v('size', shortname)]]
    fillValue <- input[[v('fill', shortname)]]
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

get_aes <- function(input, shortname) {
    mappings <- c("x", "y", "col", "size", "fill")
    names <- paste0(mappings, shortname)
    my_aes <- list()
    for (name in names) {
        #browser()
        if (input[[name]] != 'None') {
            if (name %in% names[1:2]) {
                my_aes <- c(my_aes, sprintf("eval(sym(input$%s))", name))
            } else {
                my_aes <- c(my_aes, sprintf("eval(set_value(input$%s, NULL))", name))
            }
        }
        else my_aes <- c(my_aes, 'None')
    }
    names(my_aes) <- mappings
    my_aes <- my_aes[if_else(my_aes != 'None', TRUE, FALSE)]
    do.call(aes_string, my_aes)
}

create_plot <- function(input, datapath, shortname, title, plot_type, ranges) {
    dat <- read.csv(datapath, stringsAsFactors = TRUE)
    # custom legend
    legend <- get_legend(input, shortname)
    my_aes <- get_aes(input, shortname)
    #browser()
    ggplot(data =  dat, 
           mapping = my_aes
    ) + 
        get(plot_type)() +
        theme_fivethirtyeight() +
        ggtitle(title) +
        xlab(input[[v("x", shortname)]]) +
        ylab(input[[v("x", shortname)]]) +
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

v <- function(style, shortname) {
    paste0(style, shortname)
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

merge_lists <- function(l1, l2) {
    c(l1, list(l2))
}

shinyApp(ui = ui, server = server)

