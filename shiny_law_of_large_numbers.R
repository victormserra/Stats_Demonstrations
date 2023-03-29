library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)

# Define UI for application
ui <- fluidPage(
  
  theme = shinythemes::shinytheme('readable'),

    # Application title
    titlePanel(h1("Demonstração da Lei dos Grandes Números", align = 'center')),

    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "n", 
                        label = "Tamanho Amostral (n)",
                        value = 20,
                        step = 50,
                        min = 1,
                        max = 5000,
                        ),
            
            # Define um botão que adiciona 100 ao tamanho amostral
            actionButton("add50", "Adicionar 100")
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotMoeda", height = '600px', width = '1000px')
        )
    )
)

# Define server logic
server <- function(input, output, session){
  
  observeEvent(input$add50, {
    current_value <- as.numeric(input$n)
    new_value <- current_value + 100
    updateSliderInput(session, "n", value = new_value)
  })
  
  moeda = reactive({
    data.frame(moeda = sample(x = c('Cara', 'Coroa'),
                                    size = input$n,
                                    replace = TRUE,
                                    prob = c(0.5,0.5))) %>% 
    group_by(moeda) %>%
    tally() %>%
    mutate(Prob = round(n / sum(n), 3))
    })
  
  output$plotMoeda = renderPlot({
    ggplot(data = moeda(), aes(x = moeda, y = Prob, fill = moeda )) +
    geom_col(color = 'black') +
    scale_fill_brewer(palette = 'Dark2') +
    geom_text(aes(label = Prob), vjust = -0.5, size = 5) +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 15),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 13)) +
    labs(x = 'Moeda', 
         y = 'Probabilidade',
         title = "Probabilidade de Cara ou Coroa Após n Lances de Dado")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
