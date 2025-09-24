# Code written by ANDRIANAMBININTSOA Dina Larry Ismael
# Weather Data Viz app 
library("dplyr")
library("readxl")
library("ggplot2")
library("lubridate")
library("shiny")
#data prepraration
monthly = function(data){data%>%
group_by(day(date),month(date))%>%
summarize(RR = sum(.data[[colnames(data)[2]]]),MSLP = mean(.data[[colnames(data)[3]]]),FF10 = mean(as.integer(.data[[colnames(data)[4]]])),
FF200 = mean(as.integer(.data[[colnames(data)[5]]])),FF700 = mean(as.integer(.data[[colnames(data)[6]]])),FF850 = mean(as.integer(.data[[colnames(data)[7]]])),RH500 = mean(as.integer(.data[[colnames(data)[8]]])),RH700 = mean(as.integer(.data[[colnames(data)[9]]])),Tmin = mean(.data[[colnames(data)[10]]]),Tmax = mean(.data[[colnames(data)[11]]]),Tmoy = mean(.data[[colnames(data)[12]]]))
}

pere_year = function(data){data%>%
group_by(year(date))%>%
summarize(RR = sum(.data[[colnames(data)[2]]]),MSLP = mean(.data[[colnames(data)[3]]]),FF10 = mean(as.integer(.data[[colnames(data)[4]]])),
FF200 = mean(as.integer(.data[[colnames(data)[5]]])),FF700 = mean(as.integer(.data[[colnames(data)[6]]])),FF850 = mean(as.integer(.data[[colnames(data)[7]]])),RH500 = mean(as.integer(.data[[colnames(data)[8]]])),RH700 = mean(as.integer(.data[[colnames(data)[9]]])),Tmin = mean(.data[[colnames(data)[10]]]),Tmax = mean(.data[[colnames(data)[11]]]),Tmoy = mean(.data[[colnames(data)[12]]]))
}
#App preparation
ui = fluidPage(title = "WeatherViz",
navbarPage(title = "WeatherViz",
tabPanel("Visualization",
sidebarLayout(
mainPanel(width = 9,
    fluidRow(
     column(6,plotOutput("plot1")),
     column(6,plotOutput("plot2",hover = "hover_click"),verbatimTextOutput("hover2"))),
    )
,
sidebarPanel(width = 3,
    fileInput("data","Upload data",accept = c(".csv",".xlsx")),
    selectInput("cols","Select parameters",choices = NULL),
    actionButton("btn","Refresh columns",class = "btn-success"),
    selectInput("colors","Choose colors",choices = c("#2997ab","#2cdf85","black","#db5e73ff","grey")),
    downloadButton("down"))))))
    

server = function(input,output,session){
    df2 = reactive({read_excel(input$data$datapath)})
    df_year = reactive({pere_year(df2())})
    df_month = reactive({monthly(df2())})
    df_year_cleaned = reactive({
    d = df_year()
    colnames(d) = colnames(df2())
    d
    })
observeEvent(input$btn,{updateSelectInput(inputId = "cols",choices = colnames(df2()))})
output$plot1 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)
    ggplot(data = df2())+
    geom_point(mapping = aes(x = df2()[[colnames(df2())[1]]],y = df2()[[input$cols]]),color = input$colors)+
    facet_wrap(~month(df2()[[colnames(df2())[1]]],label = TRUE,abbr = FALSE))+
    labs(x = "date",y = input$cols)+
    theme_light()
})
output$plot2 = renderPlot({
    req(input$data)
    req(input$cols)
    req(input$colors)
    ggplot(data = df_year_cleaned())+
    geom_point(mapping = aes(x = df_year_cleaned()[[colnames(df_year_cleaned())[1]]],y = df_year_cleaned()[[input$cols]]),color = input$colors)+
    geom_line(mapping = aes(x = df_year_cleaned()[[colnames(df_year_cleaned())[1]]],y = df_year_cleaned()[[input$cols]]))+
    geom_smooth((mapping = aes(x = df_year_cleaned()[[colnames(df_year_cleaned())[1]]],y = df_year_cleaned()[[input$cols]])),color = input$colors)+
    theme_light()+
    labs(x = "year",y = input$cols)
})
output$hover2 = renderPrint({
    cat(input$cols,":",input$hover_click$y)
})
}  
shinyApp(ui,server)
