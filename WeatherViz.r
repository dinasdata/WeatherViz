# Code written by ANDRIANAMBININTSOA Dina Larry Ismael
# Weather Data Viz app 
library("dplyr")
library("readxl")
library("ggplot2")
library("lubridate")
library("shiny")
library("tseries")
library("forecast")

#data prepraration
monthly = function(data){data%>%
mutate(month = month(.data[["date"]]),year = year(.data[["date"]]))%>%
group_by(month,year)%>%
summarize(RR = sum(.data[["RR"]]),MSLP = mean(.data[["MSLP"]]),FF10 = mean(as.integer(.data[["FF10"]])),
FF200 = mean(as.integer(.data[["FF200"]])),FF700 = mean(as.integer(.data[["FF700"]])),FF850 = mean(as.integer(.data[["FF850"]])),RH500 = mean(as.integer(.data[["RH500"]])),RH700 = mean(as.integer(.data[["RH700"]])),Tmin = mean(.data[["Tmin"]]),Tmax = mean(.data[["Tmax"]]),Tmoy = mean(.data[["Tmoy"]]))
}

pere_year = function(data){data%>%
mutate(year = year(date))%>%
group_by(year)%>%
summarize(RR = sum(.data[["RR"]]),MSLP = mean(.data[["MSLP"]]),FF10 = mean(as.integer(.data[["FF10"]])),
FF200 = mean(as.integer(.data[["FF200"]])),FF700 = mean(as.integer(.data[["FF700"]])),FF850 = mean(as.integer(.data[["FF850"]])),RH500 = mean(as.integer(.data[["RH500"]])),RH700 = mean(as.integer(.data[["RH700"]])),Tmin = mean(.data[["Tmin"]]),Tmax = mean(.data[["Tmax"]]),Tmoy = mean(.data[["Tmoy"]]))
}

#App preparation
ui = fluidPage(title = "WeatherViz",theme = bslib::bs_theme(bootswatch = "darkly"),
navbarPage(title = "WeatherViz",
tabPanel("Visualization",
sidebarLayout(
mainPanel(width = 9,
    fluidRow(
     column(6,plotOutput("plot1"),textOutput("summarie"),verbatimTextOutput("infos")),
     column(6,plotOutput("plot2",hover = "hover_click"),textOutput("coord"),verbatimTextOutput("hover2"))),
    )
,
sidebarPanel(width = 3,
    fileInput("data","Upload data",accept = c(".csv",".xlsx")),
    selectInput("cols","Select parameters",choices = NULL),
    actionButton("btn","Refresh columns",class = "btn-success"),
    selectInput("colors","Choose colors",choices = c("#2997ab","#2cdf85","black","#db5e73ff","grey")),
    uiOutput("slider1"),
    uiOutput("slider2") 
    ))
    ),
tabPanel("Forecasting",
sidebarLayout(
    mainPanel(width = 10, fluidRow(
     column(6,plotOutput("plot3"),verbatimTextOutput("predicted"),textOutput("Monthly_eval"),tableOutput("Evaluation")),
     column(6,plotOutput("plot4"),verbatimTextOutput("predicted2"),textOutput("Annual_eval"),tableOutput("Evaluation2")))),
    sidebarPanel(width = 2,
    selectInput("cols2","Select parameters",choices = NULL),
    sliderInput("month","Choose month",max = 12,min = 1,value = 6))
))))
   

server = function(input,output,session){
    df2 = reactive({read_excel(input$data$datapath)})
    df_year = reactive({pere_year(df2())})
    df_month = reactive({monthly(df2())})
    month_summary = reactive({summary(df_month()[[input$cols]])})
observeEvent(input$btn,{updateSelectInput(inputId = "cols",choices = colnames(df2())[2:12])
updateSelectInput(inputId = "cols2",choices = colnames(df2())[2:12])})
output$slider1 = renderUI({
    req(input$cols)
    sliderInput("s1","Filter monthly values",max = max(df_month()[[input$cols]]),min = min(df_month()[[input$cols]]),value = max(df_month()[[input$cols]]))
})
output$slider2 = renderUI({
    req(input$cols)
    sliderInput("s2","Filter monthly values",max = max(df_year()[[input$cols]]),min = min(df_year()[[input$cols]]),value = max(df_year()[[input$cols]]))
})
df_month2 = reactive({
    req(input$s1)
    df_month()%>%
    filter(.data[[input$cols]] <= input$s1)
})
df_year2 = reactive({
    req(input$s2)
    df_year()%>%
    filter(.data[[input$cols]]<= input$s2)
})
df_month3 = reactive({
    df2()%>%
    filter(month(.data[["date"]]) == input$month)
})
df_month4 = reactive({
    pere_year(df_month3())
})
month1 = reactive({
ts1 = ts(df_month4()[[input$cols2]],frequency = 1)
})
month2 = reactive({
fit = auto.arima(month1())
})
month_predicted = reactive({
    forecasted1 = forecast(month2(),h = 1)
})
output$plot3 = renderPlot({
    autoplot(month_predicted())+autolayer(fitted(month_predicted()))+theme_light()+ggtitle("Observed vs Fitted per month")+
    labs(x = "sequence",y = input$cols2)
})
year1 = reactive({
    ts1 = ts(df_year2()[[input$cols2]],frequency = 1)
})
year2 = reactive({
    ts2 = auto.arima(year1())
})
year_predicted = reactive({
    predicted = forecast(year2(),h = 1)
})
output$plot4 = renderPlot({
    autoplot(year_predicted())+autolayer(fitted(year_predicted()))+theme_light()+ggtitle("Observe vs Fitted per year")+
    labs(x = "sequence",y = input$cols2)
})
output$predicted = renderPrint({cat("Predicted ",input$cols," for next year","on the month n° ",input$month,":",month_predicted()$mean)})
output$Monthly_eval = renderText({paste("Model evaluation:")})
output$Evaluation = renderTable({accuracy(month_predicted())})
output$predicted2 = renderPrint({cat("Predicted ",input$cols," for next year :",year_predicted()$mean)})
output$Annual_eval = renderText({paste("Model evaluation:")})
output$Evaluation2 = renderTable({accuracy(year_predicted())})
output$plot1 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$s1)
    req(input$colors)
    ggplot(data = df_month2())+
    geom_point(mapping = aes(x = .data[["year"]],y = .data[[input$cols]]),color = input$colors)+
    facet_wrap(~month(.data[["month"]],label = TRUE,abbr = FALSE))+
    labs(x = "date",y = input$cols)+
    theme_light()
})
output$plot2 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$s2)
    req(input$colors)
    ggplot(data = df_year2())+
    geom_point(mapping = aes(x = .data[["year"]],y = .data[[input$cols]]),color = input$colors)+
    geom_line(mapping = aes(x = .data[["year"]],y = .data[[input$cols]]))+
    geom_smooth((mapping = aes(x = .data[["year"]],y = .data[[input$cols]])),color = input$colors,method = "lm")+
    theme_light()+
    labs(x = "year",y = input$cols)
})
output$hover2 = renderPrint({
    req(input$data)
    req(input$cols)
    req(input$colors)
    cat(input$cols,":",input$hover_click$y,"year",as.integer(input$hover_click$x))
})
output$summarie = renderText({req(input$data)
                              req(input$cols)
                              req(input$colors)
paste("Summary of",input$cols)})
output$infos = renderPrint({
    req(input$data)
    req(input$cols)
    req(input$colors)
    month_summary()
}) 
output$coord = renderText({
    req(input$data)
    req(input$cols)
    req(input$colors)
paste("Values per year",input$cols)})
}

shinyApp(ui,server)
