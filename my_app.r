library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Modelling!"),
  sidebarLayout(
    sidebarPanel(selectInput("model", "Select a model",
                 choices = c("between groups", "within groups"),
                 selected = "between groups")
  ),
  mainPanel(plotOutput("model"),
            verbatimTextOutput("coefficients")
)))


server <- function (input, output) {
  output$model <- renderPlot({
    if(input$model == "between groups") {
      m <- lm(Sepal.Length ~ Petal.Length, iris)
      ggplot(iris, (aes(x=Petal.Length, y=Sepal.Length))) + 
        geom_point(shape=20) + 
        geom_smooth(method=lm) + 
        theme(legend.position="none")+
        theme_minimal()
    } else {
      m <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Length|Species, iris)
      ggplot(iris, aes(x=Petal.Length, y=Sepal.Length, color=Species)) + 
        geom_point(shape=20) + 
        geom_smooth(method=lm) + 
        theme(legend.position="none")+
        theme_minimal()
    }
  })
  output$coefficients <- renderPrint({
    if (input$model == "between groups") {
      m <- lm(Sepal.Length ~ Petal.Length, iris)
      coef(m)
    } else {
      m <- lme4::lmer(Sepal.Length ~ Petal.Length + Petal.Length| Species, data = iris)
      coef(m)$Species
    }
  })
}
shinyApp(ui=ui, server=server)
