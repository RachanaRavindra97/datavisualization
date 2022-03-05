library(shiny)
library(shinydashboard)
library(ggplot2)
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(title = "Cancer Data Visualisation", titleWidth = 900),#gives a title to the dashboard header.
    dashboardSidebar(disable = TRUE),# the sidebar was disabled, as it was not needed.
    dashboardBody(
        #makes 4 tabs, 3 for table and one for chart which has the ploted graph.
        tabsetPanel(id = "tabset", type = 'pills', tabPanel(title = "Charts", tabPanel(title = "Charts", plotOutput("plot1", click = "plot_click"), verbatimTextOutput("info"))), tabPanel(title = "Table", DT::dataTableOutput('table1')), tabPanel(title = "Table", DT::dataTableOutput('table2')), tabPanel(title = "Table", DT::dataTableOutput('table3')), tabPanel(title = "About", textOutput('text1'))),
        #verbatimTextOutput("info"). gives information, from the data set, when a pointed is clicked on the graph.
        
                ),
)


server <- function(input, output) {
    about <- readLines(("about.txt" ))
    #read the about txt file, and print the lines on to the dashboard.
    output$text1 <- renderText({
      paste(about)
    })
    
    dat_1 <- read.csv("Mock_data_1.csv", header = TRUE, sep = ",")
    # Render imported data (CSV file) as table
    output$table1 <- DT::renderDataTable({
        dat_1
    })
    dat_2 <- read.csv("Mock_data_2.csv", header = TRUE, sep = ",")
    # Render imported data (CSV file) as table
    output$table2 <- DT::renderDataTable({
        
        dat_2
    })
    
    dat_3 <- read.csv("Mock_data_3.csv", header = TRUE, sep = ",")
    # Render imported data (CSV file) as table
    output$table3 <- DT::renderDataTable({
        
        dat_3 
    })
    
    dat_plot <- read.csv("mock_data_plot.csv", header = TRUE, sep = ",")
    output$plot1 <- renderPlot({
        #plots the dataset data_plot with x axis as cancer_type and y as weight.
        #geom_point determines the aesthetics of the points on the graph.
        #labs gives the lables to the graph:- title, subtitle.
        #theam - determines the aesthetics of the lables and the identifiers of x and y axis.
        ggplot(dat_plot, aes(x=cancer_type, y=weight)) + geom_point(size = 1.2, color = "#000099") + labs(title = "Cancer Data Set",subtitle = "Factors that might have an effect on the cause of cancer") + theme(plot.title = element_text(color = "#CC0000", size = 20, face = "bold", hjust = 0.5),plot.subtitle = element_text(size = 13, face = "bold", hjust = 0.5), axis.title.x = element_text(color = "#CC0000", size = 16, face = "bold"), axis.title.y = element_text(color = "#CC0000", size = 16, face = "bold"))
    })
    output$info <- renderPrint({
        # ggplot2 plots the graph of the data set that is passed through.
        # threshold: A maxmimum distance to the click points.
        # maxpoints: maximum number of rows to return.
        nearPoints(dat_plot, input$plot_click, threshold = 100, maxpoints = 1)
    })
}

shinyApp(ui, server)
