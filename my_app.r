library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Modelling!"),
  sidebarLayout(
    sidebarPanel(selectInput("model", "Select a model",
                 choices = c("normal", "multilevel"),
                 selected = "normal")
  ),
  mainPanel(plotOutput("model"),
            verbatimTextOutput("coefficients")
)))


server <- function (input, output) {
  output$model <- renderPlot({
    if(input$model == "normal") {
      m <- lm(Sepal.Length ~ Petal.Length, iris)
      ggplot(iris, (aes(x=Petal.Length, y=Sepal.Length))) + 
        geom_point(shape=20) + 
        geom_smooth(method=lm) + 
        theme(legend.position="none")+
        theme_minimal()
    } else {
      m <- lme4::lmer(Sepal.Length ~ Petal.Length + 1|Species, iris)
      ggplot(iris, aes(x=Petal.Length, y=Sepal.Length, color=Species)) + 
        geom_point(shape=20) + 
        geom_smooth(method=lm) + 
        theme(legend.position="none")+
       # geom_abline(slope=coef(m)$Species[,1], intercept=coef(m)$Species[,2])+
        theme_minimal()
    }
  })
  output$coefficients <- renderPrint({
    if (input$model == "normal") {
      m <- lm(Sepal.Length ~ Petal.Length, iris)
      coef(m)
    } else {
      m <- lme4::lmer(Sepal.Length ~ Petal.Length + 1| Species, data = iris)
      coef(m)$Species
    }
  })
}
shinyApp(ui=ui, server=server)
