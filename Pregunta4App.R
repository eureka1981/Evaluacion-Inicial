library(dplyr)
library(ggplot2)
library(vroom)
library(forcats)
library(purrr)
library(shiny)

injuries <- vroom::vroom("~/Prueba/injuries.tsv")
## Le agrego esta línea para convertirla en base de datos
injuries <-as.data.frame(injuries )

library(readxl)
products <- read_excel("~/Prueba/products.xlsx")
products

population <- read_excel("~/Prueba/population.xlsx")
population


prod_codes <- setNames(products$prod_code, products$title)


ui <- fluidPage(
  fluidRow(
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, products$title),
                       width = "100%"
           )
    ),
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location"))
  ),
  fluidRow(
    column(12, plotOutput("age_sex"))
  )
)



server <- function(input, output, session) {
  
  reactive({req(input$code)})
  
  selected <- reactive(injuries %>% filter(prod_code == input$code))
  
  
  
  location1 <-selected %>%
    count(location, wt = weight, sort = TRUE)
  
  
  body_part1 <-selected %>%
    count(body_part, wt = weight, sort = TRUE)
  
  
  diag1<-selected %>%
    count (diag, wt = weight, sort = TRUE)
  
  summary1 <- selected %>%
    count(age, sex, wt = weight) %>%
    left_join(population, by = c("age", "sex")) %>%
    mutate(rate = n / population * 1e4)
  
  
  rate<-summary1 %>%
    ggplot(aes(age, rate, colour = sex)) +
    geom_line(na.rm = TRUE) +
    labs(y = "Lesiones por cada 10.000 personas")
  
  
  
  estimado<- summary1 %>%
    ggplot(aes(age, n, colour = sex)) +
    geom_line() +
    labs(y = "Número estimado de lesiones")
  
  
  
  
  
  
  output$diag <- renderTable({
    diag1
  }, width = "100%")
  output$body_part <- renderTable({
    body_part1
  }, width = "100%")
  output$location <- renderTable({
    location1
  }, width = "100%")
  
  summary <- reactive({
    
  })
  
  output$age_sex <- renderPlot({
    if(input$y == "rate"){
      rate
    } else {
      estimado
    }
  })
  
}

shinyApp(ui, server)
