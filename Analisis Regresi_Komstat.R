library(shiny)
library(shinythemes)

x1=c(3,4,6,4,3,5,2,5,7,1)
x2=c(3,4,6,4,3,5,2,5,7,1)
y=c(6,8,9,5,3,7,1,7,9,4)
reg=lm(y~x)
summary(reg)


anareg<-fluidPage(
  theme=shinytheme("cerulean"),
  titlePanel(title="REGRESI LINEAR SEDERHANA DENGAN R SHINY") ,
  sidebarLayout(
    sidebarPanel(
      textInput("x","x",value="90,100,90,80,87,75"),
      textInput("y","y",value="950,1100,850,750,950,775"),
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("PLOT", plotOutput("plot")),
        tabPanel("SUMMARY",
                 verbatimTextOutput("summary")),
        tabPanel("TABEL",tableOutput("tabel"))
    )
    ))
  )

ayorun<-function(input,output){
  ekstrak<-function(text){
    text<-gsub(" ","",text)
    split<-strsplit(text,",",fixed=FALSE)[[1]]
    as.numeric(split)
  }
  output$summary<-renderPrint({
    y<-ekstrak(input$y)
    x<-ekstrak(input$x)
    fit<-lm(y~x)
    summary(fit)
  })
}

shinyApp(ui=anareg,server=ayorun)
