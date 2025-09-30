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


#App preparation
ui = fluidPage(title = "WeatherViz",
navbarPage(title = "WeatherViz",
tabPanel("Visualization",
sidebarLayout(
mainPanel(width = 9,
    fluidRow(
     column(6,plotOutput("plot1"),textOutput("summarie"),verbatimTextOutput("infos")),
     column(6,plotOutput("plot2",hover = "hover_click"),"Coordinates",verbatimTextOutput("hover2"))),
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
    month_summary = reactive({summary(df_month()[[input$cols]])})
observeEvent(input$btn,{updateSelectInput(inputId = "cols",choices = colnames(df2())[2:12])})
output$plot1 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)
    ggplot(data = df_month())+
    geom_point(mapping = aes(x = df_month()[["year"]],y = df_month()[[input$cols]]),color = input$colors)+
    facet_wrap(~month(df_month()[["month"]],label = TRUE,abbr = FALSE))+
    labs(x = "date",y = input$cols)+
    theme_light()
})
output$plot2 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)
    ggplot(data = df_year())+
    geom_point(mapping = aes(x = df_year()[["year"]],y = df_year()[[input$cols]]),color = input$colors)+
    geom_line(mapping = aes(x = df_year()[["year"]],y = df_year()[[input$cols]]))+
    geom_smooth((mapping = aes(x = df_year()[["year"]],y = df_year()[[input$cols]])),color = input$colors,method = "lm")+
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

}
shinyApp(ui,server)
