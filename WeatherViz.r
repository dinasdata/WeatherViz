# Code written by ANDRIANAMBININTSOA Dina Larry Ismael
# Weather Data Viz app 
library("dplyr")
library("readxl")
library("ggplot2")
library("lubridate")
library("shiny")
#data prepraration
monthly = function(data){data%>%
group_by(month(date))%>%
summarize(RR = sum(.data[["RR(mm)"]]),MSLP = mean(.data[["MSLP(Hpa)"]]),FF10 = mean(as.integer(.data[["FF10(Kt)"]])),
FF200 = mean(as.integer(.data[["FF200(Kt)"]])),FF700 = mean(as.integer(.data[["FF700(Kt)"]])),FF850 = mean(as.integer(.data[["FF850(Kt)"]])),RH500 = mean(as.integer(.data[["RH500"]])),RH700 = mean(as.integer(.data[["RH700"]])),Tmin = mean(.data[["Tmin(°C)"]]),Tmax = mean(.data[["Tmax(°C)"]]),Tmoy = mean(.data[["Tmoy(°C)"]]))
return (data)}

monthly_plot = function(parameter){
ggplot(data)+
geom_point(mapping = aes(x = date,y = .data[[parameter]],color = year(.data[["date"]])))+
facet_wrap(~month(date,label = TRUE,abbr = FALSE),nrow = 3)+
theme_light()}

pere_year = function(data){data%>%
group_by(year(date))%>%
summarize(RR = sum(.data[["RR(mm)"]]),MSLP = mean(.data[["MSLP(Hpa)"]]),FF10 = mean(as.integer(.data[["FF10(Kt)"]])),
FF200 = mean(as.integer(.data[["FF200(Kt)"]])),FF700 = mean(as.integer(.data[["FF700(Kt)"]])),FF850 = mean(as.integer(.data[["FF850(Kt)"]])),RH500 = mean(as.integer(.data[["RH500"]])),RH700 = mean(as.integer(.data[["RH700"]])),Tmin = mean(.data[["Tmin(°C)"]]),Tmax = mean(.data[["Tmax(°C)"]]),Tmoy = mean(.data[["Tmoy(°C)"]]))
return (data)}

year_plot = function(parameter){
ggplot(pere_year)+
geom_point(mapping = aes(x = pere_year[["year(date)"]],y = pere_year[[parameter]]),color = "blue")+
geom_line(mapping = aes(x = pere_year[["year(date)"]],y = pere_year[[parameter]]))+
geom_smooth(mapping = aes(x = pere_year[["year(date)"]],y = pere_year[[parameter]]))+
labs(x = "date",y = parameter)+
theme_light()}
#App preparation
ui = fluidPage(title = "WeatherViz",
navbarPage(title = "WeatherViz",
tabPanel("Visualization",
sidebarLayout(
mainPanel(
    fluidRow(
     column(6,plotOutput("plot1",hover = "hover_click"),verbatimTextOutput("hover1")),
     column(3,tableOutput("table1"))),
    fluidRow(
        column(6,plotOutput("plot2",hover = "hover_click"),verbatimTextOutput("hover2")),
        column(3,tableOutput("table2")),
    ),
    fluidRow(column(6,sliderInput("years","Select year",max = 2022,min = 1993,value = 2022)))
),
sidebarPanel(
    fileInput("data","Upload data",accept = c(".csv",".xlsx")),
    selectInput("cols","Select parameters",choices = NULL),
    actionButton("btn","Refresh columns",class = "btn-success"),
    downloadButton("down")
))
)))
server = function(input,output,session){
    df = reactive({read_excel(input$data$datapath)})
observeEvent(input$btn,{updateSelectInput(inputId = "cols",choices = colnames(df()))})
}

shinyApp(ui,server)
