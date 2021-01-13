### Pension Wealth Model v1.0
## by Anil Niraula
# Data: pension database + manual

rm(list=ls())
###Load/install packages
#R.Version()
#https://github.com/ReasonFoundation/pensionviewr
#Create token -> usethis::edit_r_environ() -> restart -> Sys.getenv("GITHUB_PAT")
#install.packages('devtools')
#library(devtools)
#devtools::install_github("ReasonFoundation/reasontheme",force = TRUE)
#devtools::install_github("ReasonFoundation/pensionviewr", force = TRUE)
library(reasontheme)
library(pensionviewr)
library(ggplot2)
library(tidyverse)
library(tseries)
library(data.table)
library(readr)
library(rsconnect)
library(dplyr)
library(plyr)
library(reasontheme)
library(pensionviewr)
#library(janitor)
library(tidyverse)
library(plyr)
#library(openxlsx)
library(tseries)
#library(ggplot2)
library(data.table)
library(openxlsx)
#library(readr)
library(rsconnect)
library(base64enc)
#Shiny-----------
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(repmis)
#library(shinyFiles)
library(DT)
library(plotly)
library(shiny)

##### Pulling available data from the database

##### Final Average Salary (FAS)

#### components: 
#Starting Salary, 
#Wage Inflation/Payroll Growth Assumption
#Merit/Promotion Growth Rate
#Smoothing of FAS (in Years)

## Total Salary Growth Rate(t-1) = Payroll Growth %(t-1) + Merit Growth %(t-1)
## Salary(t) = Salary(t-1) * (1 + Total Salary Growth Rate(t-1))
## Final Average Salary(t) + Average(Salary(t-SmoothedYears):Salary(t-1))

#SHINY
ui <- fluidPage(
  titlePanel("Pension Wealth Model"),
  # CODE BELOW: Add select inputs on state and plan_names to choose between different pension plans in Reason database
  theme = shinythemes::shinytheme("spacelab"),
  sidebarLayout(
    sidebarPanel(
      img(src = base64enc::dataURI(file = "https://raw.githubusercontent.com/ReasonFoundation/databaseR/master/apps/reason_logo.png"), width = 200, height = 50),
      br(),
      br(),
      #ADD slider input to choose year range
      sliderInput('salary', 'Starting Salary ($ thousands)', min = 20, max = 50, value = 30, step = 2.5),
      selectInput("salary.growth", "Salary Growth Rate (%)", choices = c(2, 2.5, 3, 3.5,4), 
                  selected = 3),
      selectInput("smooth", "FAS Smoothing (Years)", choices = c(3, 4, 5), 
                  selected = 5),      
      em("NOTES: "),
      br(),
      em("This app shows Final Average Salary projections."),
      br(),
      br(),
      textOutput('plot_2019Updates'),
      # Button
    ),
    mainPanel(
      ###Remove error messages
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),
      tabsetPanel(
        tabPanel("FAS", plotly::plotlyOutput("plot_Filtered"))
      )
    )
  )
)

#wrap around secure_app fora password protection
##########################
######Shiny app[server] -------------------------------------------------

server <- function(input, output, session){
  
  #shinymanager
 
  
  note_text <- paste0("This shiny app allows you to browse through Reason database by", sep="\n", "\n",
                      "selecting a state & pension plan.", sep="\n",
                      "Go to 'Table' & 'Columns' tabs to see data for chosen plan (to save use download button). ", sep="\n",
                      "For more granular data choose 'Filtered' optioin", sep="\n",
                      "(available for all state & some municipal plans).", sep="\n",
                      "Go to 'UAL' & 'Inv.Returns/ADEC' tabs for some historical graphs.", sep = "\n")
  
  
 
  
  PlanData <- reactive({
    
    data <- pullStateData(2001)
    data <- filterData(data, 2017)
    data <- data %>% select(year, plan_name, state, arr,wage_inflation,payroll_growth_assumption,ee_nc_pct)
    data <- data %>% filter(plan_name == "New Mexico Educational Retirement Board")
    data.nm.erb <- as.data.table(data)
    data.nm.erb <- data.nm.erb[year == 2019] 
    
    data.nm.erb$arr <- last(na.omit(data$arr))
    data.nm.erb$wage_inflation <- last(na.omit(data$wage_inflation))
    data.nm.erb$payroll_growth_assumption <- last(na.omit(data$payroll_growth_assumption))
    data.nm.erb$ee_nc_pct <- last(na.omit(data$ee_nc_pct))
    
    #####  Adding manual assumptions
    
    manual <- data.table("vesting" = 5, "smooth" = input$smooth, "hire.age" = 25, "max.yos" = 56, "max.age" = 80,
                         "reduced.yos" = NA, "reduced.age" = NA, "reduced.yos.age" = 80, "payroll.growth" = 0.03,
                         "normal.yos" = 30, "normal.age" = 67, "normal.yos.age" = 80, "normal.yos.age.min" = 65,
                         "start.wage" = input$salary, "fas" = 5)
    
    data.nm.erb$arr <- 0.07
    data.nm.erb$payroll_growth_assumption<- as.numeric(input$salary.growth)/100
    data.nm.erb <- cbind(data.nm.erb, manual)
    #View(data.nm.erb)
    #View(data.nm.erb$arr)
    
    #####  Project Payroll
    
    payroll <- matrix(NA,manual$max.yos,5) 
    colnames(payroll) <- c("YOS", "AGE", "SALARY", "FAS", "MERIT")
    
    payroll[,1] <- seq(0, manual$max.yos-1, by = 1)
    payroll[,2] <- seq(data.nm.erb$hire.age, (data.nm.erb$hire.age+manual$max.yos-1), by = 1)
    
    
    payroll[1,3] <- as.numeric(data.nm.erb$start.wage)*1000
    payroll[1,2] <- as.numeric(data.nm.erb$hire.age)
    data.nm.erb$payroll_growth_assumption <- as.numeric(data.nm.erb$payroll_growth_assumption)
    payroll <- as.data.frame(payroll)
    
    payroll[1:15,5]<- c(7.00,3.50,2.75,2.25,1.75,1.50,1.25,1.00,0.75,0.50,0.25,0.25,0.25,0.25,0.25)
    payroll[16:manual$max.yos,5]<- c(0)
    
    ## Calculate Payroll growth & populate YOS & AGE columns
    
    for(i in (2:manual$max.yos)){
      payroll[i,3] <- payroll[i-1,3]*(1+(data.nm.erb$payroll_growth_assumption + (payroll[i,5]/100)))
    }
    #View(payroll)
    
    #### Final Average Salary ####
    
   smooth <- data.nm.erb$smooth
    
    payroll[(as.numeric(smooth)+1),4] <- mean(payroll[1:smooth,3])
    
    ## Calculate Final Average Salary
    
    for(i in (smooth:(manual$max.yos-2))){
      payroll[i+2,4] <- mean(payroll[(i-3):(i+1),3])
    }
    
    payroll
  })
  
  ##Create a reactive datapull object to use for shiny graphics later
  

  #Create interactive plot
  output$plot_Filtered <- plotly::renderPlotly({
    
    graph <- data.frame(PlanData())
    #View(graph$YOS)
    #Call on a reactive data that was filtered above
    
    #View(UAL)
    #Graphics manual: https://bbc.github.io/rcookbook/
    #https://github.com/ReasonFoundation/pensionviewr/blob/master/README.Rmd
    
    p <- ggplot() +
      ggtitle(label = paste0("Final Average Salary"))+
      
      geom_line(data=graph, aes(x=YOS, y=FAS, 
                              color="Final Average Salary"),
                size = 1.00)+
      
    scale_colour_manual(values=c(palette_reason$Orange))+
      scale_y_continuous(labels = function(x) paste0("$",x), name = "")+
      scale_x_continuous(labels = function(x) paste0(x, ""), name = "Years of Service")+
      theme_bw()
    #Annotating ending UAL
   
    p <- ggplotly(p)
    p
  })
  
}
#rsconnect::appDependencies()
shinyApp(ui = ui, server = server)