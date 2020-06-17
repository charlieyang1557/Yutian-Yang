#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinyWidgets)
library(httr)
library(tidyverse)
library(jsonlite)
library(scales)

country <- GET("https://api.covid19api.com/summary") %>% content(., as = "text") %>% fromJSON() %>% .$Countries %>% arrange(Country)



ui <- fluidPage(
    setBackgroundColor(
    color = c("#F0FFFF", "#00CED1"),
    gradient = "linear",
    direction = "top"),
    titlePanel("COVID-19 Infomation"),
                sidebarLayout(
                    sidebarPanel(
                        selectInput("country","Country",choices = c("-",country$Country %>% set_names(country$name))),
                        selectInput("country_2","Second Country",choices = c("-",country$Country %>% set_names(country$name))),
                        dateRangeInput("date", "Year Month Date",
                                       start = NULL, end = NULL, min = "2020/01/01", max = Sys.Date() -1 ,format = "yyyy/mm/dd",
                                       separator = " - "),
                        selectInput("status","Status",choices = c("confirmed","deaths","recovered"))
                    ),
                    mainPanel(h4("Global Summary"), tableOutput("global_sum"), tableOutput("global_sum_2"),
                        h4("Status vs. Date"),  plotOutput("scatter"),
                              h4("Status Summary During the Time Period in Country 1"),tableOutput("sum_stats"),
                              h4("vs. Status Summary During the Time Period in Country 2"),tableOutput("sum_stats_2"),
                              h4("Country 1 Summary"),tableOutput("country_sum"),
                        tableOutput("country_sum_2"),
                              h4("Country 2 Summary"),tableOutput("country_sum_second"),
                        tableOutput("country_sum_second_2")
                    )
                ))

server <- function(input, output,session){
   
    summary <- reactive({
        GET("https://api.covid19api.com/summary") %>%
        content(., as = "text") %>%
        fromJSON()
    })
    
    
    country_code <- reactive({
        code <- "-"
        if (input$country != "-") {
            code <- country$Slug[which(country$Country == input$country)]
        }
        code
    })
    
    
    country_code_2 <- reactive({
        code <- "-"
        if (input$country_2 != "-") {
            code <- country$Slug[which(country$Country == input$country_2)]
        }
        code
    })
    
    
    covid <- reactive({
        req(input$country != "-")
        GET(str_glue("https://api.covid19api.com/dayone/country/{input_country}/status/{input_status}",
                      input_country = country_code(),
                      input_status = input$status)) %>% 
            content(., as = "text") %>%
            fromJSON() %>% 
            mutate(Month_Date = substr(Date,6,10))})
    
    covid_2 <- reactive({
        req(input$country_2 != "-")
        GET(str_glue("https://api.covid19api.com/dayone/country/{input_country}/status/{input_status}",
                     input_country = country_code_2(),
                     input_status = input$status)) %>% 
            content(., as = "text") %>%
            fromJSON() %>% 
            mutate(Month_Date = substr(Date,6,10))})
    
    covid_time <- reactive({
        req(input$country != "-")
        GET(str_glue("https://api.covid19api.com/total/country/{input_country}/status/{input_status}?from={input_time_1}T00:00:00Z&to={input_time_2}T00:00:00Z",
                     input_country = country_code(),
                     input_status = input$status,
                     input_time_1 = input$date[1],
                     input_time_2 = input$date[2])) %>%
            content(., as = "text") %>%
            fromJSON()%>% 
            mutate(Month_Date = substr(Date,6,10))
    }) 
    
    
    covid_time_2 <- reactive({
        req(input$country_2 != "-")
        GET(str_glue("https://api.covid19api.com/total/country/{input_country}/status/{input_status}?from={input_time_1}T00:00:00Z&to={input_time_2}T00:00:00Z",
                     input_country = country_code_2(),
                     input_status = input$status,
                     input_time_1 = input$date[1],
                     input_time_2 = input$date[2])) %>%
            content(., as = "text") %>%
            fromJSON()%>% 
            mutate(Month_Date = substr(Date,6,10))
    }) 
    
    
    sum_status <- reactive({stats <- tibble(Country = covid_time()$Country[1],
                                            Total_status = covid_time()$Cases[nrow(covid_time())],
                           Average_status = (covid_time()$Cases[nrow(covid_time())] - covid_time()$Cases[1])/nrow(covid_time()),
                           status_growth = covid_time()$Cases[nrow(covid_time())] - covid_time()$Cases[1],
                           status_Growth_Rate = percent(status_growth/Total_status))
    names(stats) = c(paste("Country"),paste("Total",input$status),paste("Average",input$status, "From",input$date[1],"to",input$date[2]),paste("Total",input$status, "From",input$date[1],"to",input$date[2]),paste(input$status,"Growth Rate"))
    stats
    })
    
    sum_status_2 <- reactive({stats <- tibble(Country = covid_time_2()$Country[1],
                                              Total_status = covid_time_2()$Cases[nrow(covid_time_2())],
                                            Average_status = (covid_time_2()$Cases[nrow(covid_time_2())] - covid_time_2()$Cases[1])/nrow(covid_time_2()),
                                            status_growth = covid_time_2()$Cases[nrow(covid_time_2())] - covid_time_2()$Cases[1],
                                            status_Growth_Rate = percent(status_growth/Total_status))
    names(stats) = c(paste("Country"),paste("Total",input$status),paste("Average",input$status, "From",input$date[1],"to",input$date[2]),paste("Total",input$status, "From",input$date[1],"to",input$date[2]),paste(input$status,"Growth Rate"))
    stats
    })

    country_summary <- reactive({
        summary()$Countries %>%
            filter(Country == input$country) %>%
            select(Country, NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered) %>%
            mutate(ConfirmedGrowthRate = percent(NewConfirmed/TotalConfirmed),DeathGrowthRate = percent(NewDeaths/TotalDeaths),RecoveredGrowthRate = percent(NewRecovered/TotalRecovered),DeathRate = percent(TotalDeaths/TotalConfirmed))
    })
    
    
    country_summary_2 <- reactive({
        summary()$Countries %>%
            filter(Country == input$country_2) %>%
            select(Country, NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered) %>%
            mutate(ConfirmedGrowthRate = percent(NewConfirmed/TotalConfirmed),DeathGrowthRate = percent(NewDeaths/TotalDeaths),RecoveredGrowthRate = percent(NewRecovered/TotalRecovered),DeathRate = percent(TotalDeaths/TotalConfirmed))
    })
    
    global_summary <- reactive({
        tibble(NewConfirmed = summary()$Global$NewConfirmed,
                   TotalConfirmed = summary()$Global$TotalConfirmed,
                   NewDeaths = summary()$Global$NewDeaths,
                   TotalDeaths = summary()$Global$TotalDeaths,
                   NewRecovered = summary()$Global$NewRecovered,
                   TotalRecovered = summary()$Global$TotalRecovered) %>% 
            mutate(ConfirmedGrowthRate = percent(NewConfirmed/TotalConfirmed),DeathGrowthRate = percent(NewDeaths/TotalDeaths),RecoveredGrowthRate = percent(NewRecovered/TotalRecovered),DeathRate = percent(TotalDeaths/TotalConfirmed))
    })
    
    
    output$scatter <- renderPlot({
        if (is.na(input$date[1]) == TRUE) {
            if(input$country_2 == "-" && input$country != "-"){
                ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue")  +  geom_line(data = covid(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue") +
                    geom_label(data= covid()[nrow(covid())-1,], aes(label=Country)) +
                    theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                    ggtitle(paste("Total", input$status,"From",input$country,"in 2020"))
            }
            else if(input$country == "-" && input$country_2 != "-"){
                ggplot(data = covid_2(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") + geom_line(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") +
                    geom_label(data= covid_2()[nrow(covid_2())-1,], aes(label=Country)) +
                    theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                    ggtitle(paste("Total", input$status,"From",input$country_2,"in 2020"))
            }
            else{
            ggplot(data = covid(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue")  +  geom_line(data = covid(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue") +
                geom_label(data= covid()[nrow(covid())-1,], aes(label=Country)) +
                geom_point(data = covid_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") + geom_line(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") +
                geom_label(data= covid_2()[nrow(covid_2())-1,], aes(label=Country)) +
                theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                ggtitle(paste("Total", input$status,"From",input$country,"and",input$country_2,"in 2020"))}} 
        else{
            if(input$country_2 == "-" && input$country != "-"){
                req(is.na(input$date[1]) != TRUE )
                ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue")  +  geom_line(data = covid(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue") +
                    geom_label(data= covid_time()[nrow(covid_time())-1,], aes(label=Country)) +
                    theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                    ggtitle(paste("Total", input$status,"From",input$country,"in" , input$date[1], "to", input$date[2]))
            }
            else if(input$country == "-" && input$country_2 != "-"){
                req(is.na(input$date[1]) != TRUE )
                ggplot(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") + geom_line(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") +
                    geom_label(data= covid_time_2()[nrow(covid_time_2())-1,], aes(label=Country)) +
                    theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                    ggtitle(paste("Total", input$status,"From",input$country_2,"in", input$date[1], "to", input$date[2]))
            }
            else{
            req(is.na(input$date[1]) != TRUE )
            ggplot(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases)) + geom_point(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue")  +  geom_line(data = covid_time(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country)),colour="darkblue") +
                geom_label(data= covid_time()[nrow(covid_time())-1,], aes(label=Country)) +
                geom_point(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") + geom_line(data = covid_time_2(), aes(x = as.factor(Month_Date), y = Cases, group = as.factor(input$country_2)),colour="red") +
                geom_label(data= covid_time_2()[nrow(covid_time_2())-1,], aes(label=Country)) +
                theme(axis.text.x=element_text(angle=50, size=7, vjust=0.5)) + xlab("Month-Date") +
                ggtitle(paste("Total", input$status, "From",input$country,"and",input$country_2,"in", input$date[1], "to", input$date[2]))}
    }})
    
    output$sum_stats <- renderTable({
        if(input$country == "-"){}
        else{sum_status()} 
            })
    
    output$sum_stats_2 <- renderTable({
        if(input$country == "-"){}
        else{sum_status_2()} 
    })
    
    
    output$global_sum <- renderTable({
        global_summary() %>% select(NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered)
    })
    
    
    output$global_sum_2 <- renderTable({
        global_summary() %>% select(ConfirmedGrowthRate,DeathGrowthRate,RecoveredGrowthRate,DeathRate)
    })
    
    
    output$country_sum <- renderTable({
        req(input$country != "-")
        country_summary() %>% select(Country, NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered)
    })
    
    output$country_sum_2 <- renderTable({
        req(input$country != "-")
        country_summary() %>% select(ConfirmedGrowthRate,DeathGrowthRate,RecoveredGrowthRate,DeathRate)
        
    })
    
    
    output$country_sum_second <- renderTable({
        req(input$country_2 != "-")
        country_summary_2() %>% select(Country, NewConfirmed ,TotalConfirmed, NewDeaths, TotalDeaths, NewRecovered, TotalRecovered)
    })
    
    output$country_sum_second_2 <- renderTable({
        req(input$country_2 != "-")
        country_summary_2() %>% select(ConfirmedGrowthRate,DeathGrowthRate,RecoveredGrowthRate,DeathRate)
    })
    
}





shinyApp(ui = ui, server = server)
