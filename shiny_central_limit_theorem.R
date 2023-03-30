library(shiny)
library(ggplot2)
library(RColorBrewer)
library(shinyWidgets)

# Define UI for application
ui <- fluidPage(
  
  theme = shinythemes::shinytheme('readable'),

    # Application title
    titlePanel(h1("Demonstração do Teorema Central do Limite", align = 'center')),

    # Sidebar with a slider input 
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId = "n", 
                        label = "Tamanho de Cada Amostra (n) ",
                        value = 5,
                        step = 5,
                        min = 1,
                        max = 300,
                        ),
            
            # Define um botão que adiciona 100 ao tamanho amostral
            actionButton("add", "Adicionar 10"),
            
            sliderInput(inputId = "q", 
                        label = "Quantidade de Médias",
                        value = 1000,
                        step = 50,
                        min = 1,
                        max = 10000,
            ),
            
            actionButton("add2", "Adicionar 100"),
        
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plotCLT", height = '600px', width = '1000px')
        )
    )
)

# Define server logic
server <- function(input, output, session){
  
  observeEvent(input$add, {
    current_value <- as.numeric(input$n)
    new_value <- current_value + 10
    updateSliderInput(session, "n", value = new_value)
  })
  
  observeEvent(input$add2, {
    current_value <- as.numeric(input$q)
    new_value <- current_value + 100
    updateSliderInput(session, "q", value = new_value)
  })
  
  população = rchisq(100000, df = 1)
  
  clt = reactive({
  # Define o tamanho amostral n
  n = input$n
  # Define a quantitdade de médias
  q = input$q
  # Inicia um vetor de médias
  médias = numeric(0)
  
  # Concretiza o CLT 
  for(i in 1:q){médias[i] =  mean(sample(população, size = n, replace = FALSE))}
  
  # Gera um dataframe (o ggplot demanda um dataframe para fazer gráficos)
  data.frame(médias = médias)
  })
  
  output$plotCLT = renderPlot({
    ggplot(clt(), aes(x = médias)) +
      geom_histogram(aes(y = ..density..), bins = 50, fill = 'red4', color = 'white') +
      stat_density(geom = 'line', lwd = 1.2, color = 'black') +
      theme(legend.position = 'none',
            plot.title = element_text(hjust = 0.5, size = 15),
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 13)) +
      labs(x = 'x',
           y = '',
           title = "População consiste em 50.000 observações com distribuição qui-quadrado(1)")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
