library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Sample Size Allocation for Population Mean"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("k", "Number of Subgroups", value = 3, min = 1, max = 5),
      
      numericInput("var1", "Variance - Subgroup 1", 4),
      numericInput("var2", "Variance - Subgroup 2", 6),
      numericInput("var3", "Variance - Subgroup 3", 8),
      
      numericInput("cost1", "Cost - Subgroup 1", 3),
      numericInput("cost2", "Cost - Subgroup 2", 5),
      numericInput("cost3", "Cost - Subgroup 3", 7)
    ),
    
    mainPanel(
      tableOutput("table"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  get_data <- reactive({
    k <- input$k
    
    variance <- c(input$var1, input$var2, input$var3)[1:k]
    cost <- c(input$cost1, input$cost2, input$cost3)[1:k]
    
    allocation <- variance / cost
    n_i <- round(100 * allocation / sum(allocation))
    
    data.frame(
      Subgroup = paste("Group", 1:k),
      Variance = variance,
      Cost = cost,
      Sample_Size = n_i
    )
  })
  
  output$table <- renderTable({
    get_data()
  })
  
  output$plot <- renderPlot({
    df <- get_data()
    ggplot(df, aes(x = Subgroup, y = Sample_Size)) +
      geom_bar(stat = "identity") +
      xlab("Subgroup") +
      ylab("Sample Size") +
      ggtitle("Sample Size Allocation Across Subgroups")
  })
}

shinyApp(ui = ui, server = server)
