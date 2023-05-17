library(MASS)
library(shiny)
library(ggbiplot)
library(ggplot2)
library(plotly)
library(FactoMineR)
library(factoextra)

ui <- navbarPage(
  "111971006 - HW4",
  tabPanel("PCA", id = "pca_tab",
           fluidRow(
             column(
               width = 6,
               sidebarPanel(
                 width = 12,
                 numericInput("ncomp1", "First PCA :", min = 1, max = 4, value = 1),
                 numericInput("ncomp2", "Second PCA：", min = 1, max = 4, value = 2),
                 actionButton("calculate", "Calculate PCA")
               )
             ),
             column(
               width = 6,
               mainPanel(
                 h3("Explained Variance (PCA)"),
                 tableOutput("explained_variance_pca")
               )
             )
           ),
           mainPanel(
             plotlyOutput("pca_plot", width = "800px", height = "600px"),
             h3("PCA SUMMARY"),
             tableOutput("pca_summary"),
             h3("ROTATION TABLE"),
             tableOutput("rotation_table")
           )
  ),
  tabPanel("CA", id = "ca_tab",
           fluidRow(
             column(
               width = 5,
               sliderInput("point_num", "Number of Points:", min = 1, max = 150, value = 120)
               # numericInput("point_num", "Number of Points:", min = 1, max = 150, value = 50)
             ),
             column(
               width = 7,
               h3("Explained Variance (CA)"),
               tableOutput("explained_variance_ca")
             )
           ),
           mainPanel(
             plotOutput(outputId = "ca_plot",width = "800px", height = "600px")
           )
  )
)

server <- function(input, output, session) {
  # PCA Result Summary
  output$pca_summary <- renderTable({
    pca <- pca_data()$pca
    importance <- data.frame(
      "Standard deviation" = pca$sdev,
      "Proportion of Variance" = pca$sdev^2 / sum(pca$sdev^2),
      "Cumulative Proportion" = cumsum(pca$sdev^2 / sum(pca$sdev^2))
    )
    row.names(importance) <- paste0("PC", 1:length(pca$sdev))
    importance
  })
  
  # Rotation Table
  output$rotation_table <- renderTable({
    pca <- pca_data()$pca
    rotation <- pca$rotation
    row.names(rotation) <- names(iris)[1:4]
    rotation
  })
  
  
  output$iris_table <- renderTable({
    head(iris, 10)
  })
  
  
  # Reactive values to store the selected PCA components
  selected_pca <- reactiveValues(ncomp1 = 1, ncomp2 = 2)
  
  # Update selected PCA components when Calculate button is clicked
  observeEvent(input$calculate, {
    if (input$ncomp1 == input$ncomp2) {
      showNotification("The selected PCA must be different.", type = "error")
      return()
    }
    
    selected_pca$ncomp1 <- input$ncomp1
    selected_pca$ncomp2 <- input$ncomp2
  })
  
  # Perform PCA and create ggplot
  pca_data <- reactive({
    log.ir <- log(iris[, 1:4])
    ir.species <- iris[, 5]
    
    ir.pca <- prcomp(log.ir, center = TRUE, scale. = TRUE)
    
    return(list(pca = ir.pca, species = ir.species))
  })
  
  # Render PCA plot
  output$pca_plot <- renderPlotly({
    pca <- pca_data()$pca
    species <- pca_data()$species
    
    g <- ggbiplot(pca, obs.scale = 1, var.scale = 1, groups = species,
                  choices = c(selected_pca$ncomp1, selected_pca$ncomp2))
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    
    ggplotly(g)
  })
  
  # Calculate explained variance
  explained_variance <- reactive({
    pca <- pca_data()$pca
    variance <- pca$sdev^2
    explained_var <- variance / sum(variance) * 100
    data.frame(Principal_Component = paste0("PC", 1:length(explained_var)),
               Explained_Variance = round(explained_var, 2))
  })
  
  # Render explained variance table
  output$explained_variance_pca <- renderTable({
    explained_variance()
  })
  
  # CA
  # Perform CA analysis
  ca_data <- reactive({
    data <- as.data.frame(iris[, 1:4])
    ca_result <- CA(data, graph = FALSE)
    return(ca_result)
  })
  
  # Reactive value to track the selected number of points
  selected_points <- reactive({
    input$point_num
  })
  
  # Render CA plot
  output$ca_plot <- renderPlot({
    ca_result <- ca_data()
    
    # Convert eigenvalues to numeric
    eig_val1 <- as.numeric(ca_result$eig[1, 2])
    eig_val2 <- as.numeric(ca_result$eig[2, 2])
    
    result <- ca_result
    plot(result$row$coord[1:selected_points(), 1], result$row$coord[1:selected_points(), 2],
         col = iris$Species[1:selected_points()], pch = 16,
         xlab = paste("Dim1 (", round(result$eig[1, "percentage of variance"], 1), "%)", sep = ""),
         ylab = paste("Dim2 (", round(result$eig[2, "percentage of variance"], 1), "%)", sep = ""))
    
    # Add crosshair dashed lines
    abline(v = 0, lty = "dashed")
    abline(h = 0, lty = "dashed")
    
  })
  # Render explained variance table for CA
  output$explained_variance_ca <- renderTable({
    ca_result <- ca_data()
    
    eig_values <- round(ca_result$eig[, "eigenvalue"], 3)
    eig_percent <- round(ca_result$eig[, "percentage of variance"], 3)
    cumulative_percent <- cumsum(eig_percent)
    
    data.frame(
      Dimension = paste0("Dim.", 1:length(eig_values)),
      Variance = eig_values,
      "% of var." = eig_percent,
      "Cumulative % of var." = cumulative_percent
    )
  })
}

shinyApp(ui, server)
