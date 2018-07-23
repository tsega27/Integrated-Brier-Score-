# Integrated-Brier-Score-
Web based Integrated Brier Score (IBR)
library(shiny)
library(pec)
library(rms)
library(ggplot2)
library(ggrepel)
library(gridExtra)

set.seed(123456)
x <- (1:25)


#Defining User Interface

ui <- fluidPage( 
  # Application title
  
  titlePanel("Generate Data of IBS (Integrated Brier Score) as a function of Time"),
  #  input for number of Size, Mean and Standard Deviton
  
  numericInput("Num", label = ("Enter sample size"), value=1000),
  numericInput("mean", label = ("Enter sample mean"), value= 120),
  numericInput("sd", label = ("Enter standard devition"), value=10),
  sliderInput(inputId="r", 
              label = "Choose a rate ", 
              value= 0.5, min=0.01, max=0.99), 
  
  # Show a plot of the generated distribution
  plotOutput("rate")    
)


#Defining Server

server <- function(input, output) 
{ 
  
  output$rate <-renderPlot( 
    {
      numb<-input$Num
      mean <-input$mean
      sd<-input$sd
      
      c1<-rexp(numb,rate=.25) # Generate Censoring time
      x1<-rnorm(numb,mean, sd) # Generate Covariate Independent of Failure Time
      
      
      ibsfun<-function(r1){
        t1<-rexp(numb,rate=r1) #Generate Failure time: compare rate=.01, vs .6
        
        y1<-pmin(t1,c1) # Deriving observed time
        medtime<-median(y1)
        delta1<-as.numeric(t1<c1) # Event indicator
        
     
        survdata<-data.frame(cbind(t1,c1,y1,delta1,x1))
        f3<- coxph(Surv(y1,delta1)~x1,data=survdata)
        
         pf <- pec(list(f3),formula=Surv(y1,delta1)~x1,data=survdata)
        
        ibs<-crps(pf,times=medtime)[2,1] # Calculate Integrated Brier Score for Model
        
        cindex<-summary(f3)$concordance[1] #Calculate Harrell's Concordance Index
        pvalue<-summary(f3)$waldtest[3]
        prederrograph<-cbind(x.time=pf$time,y.prederror=pf$AppErr$coxph)
        return(prederrograph)
      }
      
      plot(ibsfun(input$r),  xlab= "Time", ylab="Prediction Error", main= "Relationship Between Brier Score and Event Rates")
      output$numb <- renderPrint({ input$Num })
      output$mean <- renderPrint({ input$mean })
      output$sd <- renderPrint({ input$sd })
    })
}
shinyApp(server=server, ui=ui)
