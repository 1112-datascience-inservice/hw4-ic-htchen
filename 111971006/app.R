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
             plotlyOutput("pca_plot", width = "800px", height = "600px")
           )
  ),
  tabPanel("CA", id = "ca_tab",
           mainPanel(
             plotOutput(outputId = "ca_plot")
           )
  )
)

server <- function(input, output, session) {
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
  
  # Render CA plot
  output$ca_plot <- renderPlot({
    ca_result <- ca_data()
    
    
    # Convert eigenvalues to numeric
    eig_val1 <- as.numeric(ca_result$eig[1, 2])
    eig_val2 <- as.numeric(ca_result$eig[2, 2])
    
    result <- CA(iris[, -5], graph = FALSE)
    plot(result$row$coord[, 1], result$row$coord[, 2],
         col = iris$Species, pch = 16,
         xlab = paste("Dim1 (", round(result$eig[1, "percentage of variance"], 1), "%)", sep = ""),
         ylab = paste("Dim2 (", round(result$eig[2, "percentage of variance"], 1), "%)", sep = ""))
  })
  
}

shinyApp(ui, server)
