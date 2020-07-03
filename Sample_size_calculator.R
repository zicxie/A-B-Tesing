

library(shiny)
library(dplyr)
library(ggplot2)

ui <- navbarPage("Sample Size Calculator",
                 
  tabPanel("Rate-Type-calculation",
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput("p1",'Proportion in Control Group (Baseline Convertion Rate)',value = 5, min=0, max=100), 
      helpText("Ex: Enter 5 meaning the origininal conversion rate is 5%"),
      numericInput("p2",'Desired Improved Percentage',value = 10, min=0, max=100),
      helpText("What's minimun lift that can be regarded as significant"),
      helpText("Ex: Enter 10 meaning at least 10% lift is required"),
      numericInput("power",'Power of your Experiment',value = 80, min=0, max=100),
      helpText("The percentage rate to detect the difference (lift) between the groups while it exists"),
      
      
      radioButtons("CI",'Confidence Level',choices = c('80%','85%','90%','95%'), selected = '95%'),
      helpText("The probability of rejecting the difference while it does NOT exist")
    
      )
    ,
    mainPanel(
      numericInput('groups','Number of groups', value = 2, min = 0, max = 100),
      helpText("Total number of groups in our experiment"),
      helpText("Ex: if there is one sample of control group, two samples of treatment group, enter 3"),
      tabsetPanel(
        tabPanel("Sample-size Summary",
                 strong("Required Sample Size Per Group:"),
                 textOutput("samplesize"),
                 br(),
                 strong("Total Required Samples:"),
                 textOutput("totalsize")),
        tabPanel("Required Duration",
                 helpText("Instruction: What amount of traffic can you arrange to this experiment?"),
                 radioButtons("period", "Enter your period of traffic", choices = c('Daily','Weekly','Monthly'), selected = 'Daily', inline = T),
                 numericInput('traffic', "Enter your traffic per period:", value = 1000, min = 0),
                 helpText("Ex: if you can collect 1000 samples per day to the experiment, select 'Daily' and enter 1000 in the above box."),
                 br(),
                 strong("Results:"),
                 "You need at least",textOutput('duration',inline = T),textOutput('period_selection',inline = T),'to collect all groups of your samples.'
                 ),
        tabPanel("Required Traffic",
                 helpText("Instruction: How long can you spent on collecting the samples?"),
                 numericInput('length', "Enter you sample collection period-length:", value = 5, min = 0),
                 radioButtons("unit", "Enter your period of traffic", choices = c('Day(s)','Week(s)','Month(s)'), selected = 'Day(s)', inline = T),
                 br(),
                 strong("Results:"),
                 "You need at least arrange",textOutput('traffic',inline = T),'samples per',textOutput('unit_selection',inline = T),'to collect all groups of your samples in your specified period.'
                 )
              )
      )
    )
  ),
  tabPanel("Number-Type Caculation",
           
           sidebarPanel(
             numericInput("delta",'Desired Absolute Improvement',value = 10, min=0),
             helpText("What's the minimum lift you want your measurement to reach"),
             helpText("Ex: I want the mean timespent/user on our game to at least improve from 10 to 15, enter 5 here."),
             numericInput("sd",'Standard Deviation',value = 10, min=0),
             helpText("What's the population standard deviation of your measurement"),
             numericInput("power_mean",'Power of your Experiment',value = 80, min=0, max=100),
             helpText("The percentage rate to detect the difference (lift) between the groups while it exists"),
             radioButtons("CI_mean",'Confidence Level',choices = c('80%','85%','90%','95%'), selected = '95%')),
             helpText("The probability of rejecting the difference while it does NOT exist"),
           
           mainPanel(
             numericInput('groups_mean','Number of groups', value = 2, min = 0, max = 100),
             helpText("Total number of groups in our experiment"),
             helpText("Ex: if there is one sample of control group, two samples of treatment group, enter 3"),
               tabsetPanel(
               tabPanel("Sample-size Summary",
                        strong("Required Sample Size Per Group:"),
                        textOutput("samplesize_mean"),
                        br(),
                        strong("Total Required Samples:"),
                        textOutput("totalsize_mean")
                        ),
               tabPanel("Required Duration",
                        helpText("Instruction: What amount of traffic can you arrange to this experiment?"),
                        radioButtons("period_mean", "Enter your period of traffic", choices = c('Daily','Weekly','Monthly'), selected = 'Daily', inline = T),
                        numericInput('traffic_mean', "Enter your traffic per period:", value = 1000, min = 0),
                        helpText("Ex: if you can collect 1000 samples per day to the experiment, select 'Daily' and enter 1000 in the above box."),
                        br(),
                        strong("Results:"),
                        "You need at least",textOutput('duration_mean',inline = T),textOutput('period_selection_mean',inline = T),'to collect all groups of your samples.'
                        ),
               tabPanel("Required Traffic",
                        helpText("Instruction: How long can you spent on collecting the samples?"),
                        numericInput('length_mean', "Enter you sample collection period-length:", value = 5, min = 0),
                        radioButtons("unit_mean", "Enter your period of traffic", choices = c('Day(s)','Week(s)','Month(s)'), selected = 'Day(s)', inline = T),
                        br(),
                        strong("Results:"),
                        "You need at least arrange",textOutput('traffic_mean',inline = T),'samples per',textOutput('unit_selection_mean',inline = T),'to collect all groups of your samples in your specified period.'
                       )
             )
           )
)
)


server <- function(input, output) {
#  targetpower = input$powerinput
  
  output$samplesize = renderText({
    prop1 = input$p1*0.01
    prop2 = input$p1*(1+(input$p2*0.01))*0.01
    Alpha = switch(input$CI, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.prop.test(p1=prop1, p2=prop2, power=0.8, alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    size
    })

  output$totalsize = renderText({
    prop1 = input$p1*0.01
    prop2 = input$p1*(1+(input$p2*0.01))*0.01
    Alpha = switch(input$CI, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.prop.test(p1=prop1, p2=prop2, power=0.8, alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = as.character(input$groups*as.numeric(size))
    total_size
  })
  
  output$duration = renderText({
    prop1 = input$p1*0.01
    prop2 = input$p1*(1+(input$p2*0.01))*0.01
    Alpha = switch(input$CI, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.prop.test(p1=prop1, p2=prop2, power=0.8, alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = input$groups*as.numeric(size)
    traffic = input$traffic
    duration = ceiling(total_size/traffic) 
  })
  
  output$period_selection = renderText({
    periods = switch(input$period, 'Daily'='days', 'Weekly'='weeks','Monthly'='months')
    periods  
    })
  
  output$unit_selection = renderText({
    units = switch(input$unit, 'Day(s)'='day', 'Week(s)'='week','Month(s)'='month')
    units})
  
  output$traffic = renderText({
    prop1 = input$p1*0.01
    prop2 = input$p1*(1+(input$p2*0.01))*0.01
    Alpha = switch(input$CI, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.prop.test(p1=prop1, p2=prop2, power=0.8, alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = input$groups*as.numeric(size)
    traffic = ceiling(total_size/input$length)
    traffic
  })
  
  
  output$samplesize_mean = renderText({
    diff = input$delta
    sd_mean = input$sd
    Alpha = switch(input$CI_mean, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.t.test(delta=diff, sd=sd_mean, power=0.8, type = 'two.sample', alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    size
  })
  
  output$totalsize_mean = renderText({
    diff = input$delta
    sd_mean = input$sd
    Alpha = switch(input$CI_mean, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.t.test(delta=diff, sd=sd_mean, power=0.8, type = 'two.sample', alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = as.character(input$groups_mean*as.numeric(size))
    total_size
  }) 
  
  output$duration_mean = renderText({
    diff = input$delta
    sd_mean = input$sd
    Alpha = switch(input$CI_mean, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.t.test(delta=diff, sd=sd_mean, power=0.8, type = 'two.sample', alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = input$groups_mean*as.numeric(size)
    duration = ceiling(total_size/input$traffic_mean) 
    duration
  })
  
  output$period_selection_mean = renderText({
    periods = switch(input$period_mean, 'Daily'='days', 'Weekly'='weeks','Monthly'='months')
    periods  
  })

  output$unit_selection_mean = renderText({
    units = switch(input$unit_mean, 'Day(s)'='day', 'Week(s)'='week','Month(s)'='month')
    units})
  
  output$traffic_mean = renderText({
    diff = input$delta
    sd_mean = input$sd
    Alpha = switch(input$CI_mean, '80%'=0.20, '85%'=0.15, '90%'=0.10, '95%'=0.05)
    Results= power.t.test(delta=diff, sd=sd_mean, power=0.8, type = 'two.sample', alternative = 'two.sided', sig.level = Alpha)
    size = as.character(ceiling(Results$n))
    total_size = input$groups_mean*as.numeric(size)
    traffic = ceiling(total_size/input$length_mean)
    traffic
  }
    
  )
    
  }

shinyApp(ui = ui, server = server)