# Code written by ANDRIANAMBININTSOA Dina Larry Ismael
# Weather Data Viz app 
library("dplyr")
library("readxl")
library("ggplot2")
library("lubridate")
library("shiny")
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

print(monthly(read_excel("/media/dina/f4c07323-3819-4c76-ad53-95f7d45b7ae2/weather_data_vizualisation/data.xlsx")))
#App preparation
ui = fluidPage(title = "WeatherViz",
navbarPage(title = "WeatherViz",
tabPanel("Visualization",
sidebarLayout(
mainPanel(width = 9,
    fluidRow(
     column(6,plotOutput("plot1"),uiOutput("slider1"),textOutput("summarie"),verbatimTextOutput("infos")),
     column(6,plotOutput("plot2",hover = "hover_click"),uiOutput("slider2"),"Coordinates",verbatimTextOutput("hover2"))),
    )
,
sidebarPanel(width = 3,
    fileInput("data","Upload data",accept = c(".csv",".xlsx")),
    selectInput("cols","Select parameters",choices = NULL),
    actionButton("btn","Refresh columns",class = "btn-success"),
    selectInput("colors","Choose colors",choices = c("#2997ab","#2cdf85","black","#db5e73ff","grey")),
    )))))
    

server = function(input,output,session){

    df2 = reactive({read_excel(input$data$datapath)})
    df_year = reactive({pere_year(df2())})
    df_month = reactive({monthly(df2())})
       
    output$slider1 = renderUI({
    req(input$data)
    req(input$cols)
    req(input$colors)
    sliderInput("s1","Filter value",max = as.integer(max(df_month()[[input$cols]])),min = as.integer(min(df_month()[[input$cols]])),value = as.integer(max(df_month()[[input$cols]])))
    })
    output$slider2 = renderUI({
    req(input$data)
    req(input$cols)
    req(input$colors)
    sliderInput("s2","Filter value",max = as.integer(max(df_year()[[input$cols]])),min = as.integer(min(df_year()[[input$cols]])),value = as.integer(max(df_year()[[input$cols]])))
    })
    df_year2 = reactive({
    req(input$s2)    
    df_year()%>%
    filter(.data[[input$cols]] <= input$s2)})
    df_month2 = reactive({
    req(input$s1)
    df_month()%>%
    filter(.data[[input$cols]] <= input$s1)
    })
    month_summary = reactive({summary(df_month2()[[input$cols]])})
observeEvent(input$btn,{updateSelectInput(inputId = "cols",choices = colnames(df2())[2:12])})
output$plot1 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)

    ggplot(data = df_month2())+
    geom_point(mapping = aes(x = df_month2()[["year"]],y = df_month2()[[input$cols]]),color = input$colors)+
    facet_wrap(~month(df_month2()[["month"]],label = TRUE,abbr = FALSE))+
    labs(x = "date",y = input$cols)+
    theme_light()
})
output$plot2 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)

    ggplot(data = df_year2())+
    geom_point(mapping = aes(x = year,y = df_year2()[[input$cols]]),color = input$colors)+
    geom_line(mapping = aes(x = year,y = df_year2()[[input$cols]]))+
    geom_smooth((mapping = aes(x = year,y = df_year2()[[input$cols]])),color = input$colors)+
    theme_light()+
    labs(x = "year",y = input$cols)
})
output$hover2 = renderPrint({
    cat(input$cols,":",input$hover_click$y,"year",as.integer(input$hover_click$x))
})
output$summarie = renderText({paste("Summary of",input$cols)})
output$infos = renderPrint({
    req(input$data)
    req(input$cols)
    req(input$colors)
    month_summary()
}) 

}
shinyApp(ui,server)
