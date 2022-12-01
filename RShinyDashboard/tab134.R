#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(tidyverse)
library(shiny)
library(shinydashboard)
# install.packages("plotly")
# install.packages("shinnyapp")
library(plotly)
# install.packages("wordcloud")
library(wordcloud)
# install.packages("DT")
library(DT)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("ggrepel")
library(ggrepel)

library(shiny)

publications = read.csv("USC_SDG1to16_Pub_list.csv")
fullinfo= read.csv("USCpubauthfullinfoSDG1to16.csv")
pubwithzero = read.csv("USC_SDG0to16.csv")
sdg_colors <- c('1' = '#E5243B', '2' = '#DDA63A', '3' = '#4C9F38', '4' = '#C5192D', '5' = '#FF3A21', '6' = '#26BDE2',
                '7' = '#FCC30B', '8' = '#A21942', '9' = '#FD6925', '10' = '#DD1367', '11' = '#FD9D24', '12' = '#BF8B2E',
                '13' = '#3F7E44', '14' = '#0A97D9', '15' = '#56C02B', '16' = '#00689D', '17' = '#19486A')

# Define UI for application that draws a histogram
ui <- dashboardPage( skin="black",
                     
                     # Application title
                     dashboardHeader(title = "USC SDG Mapping"),
                     
                     
                     dashboardSidebar(
                       sidebarMenu( #will eventually add Schools to sdgs and sdgs to schools
                         menuItem("Home (About)", tabName = "6"), #Ric
                         menuItem("Learn About The SDGs", tabName = "5"),#Bhavya
                         menuItem("USC Research: SDGs by Year", tabName = "4"),#Ric
                         menuItem("USC Research: SDGs by Department", tabName = "3"), #Xinyi
                         menuItem("View USC Scholars and Departments by SDGs", tabName = "2"), #Aurora
                         menuItem("Find SDGs and Publications by USC Author", tabName = "1") #Alison
                       )
                     ),
                     dashboardBody( tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")), #link up c
                                    tabItems(
                                      tabItem(tabName = "6",
                                              fluidPage(
                                                h1("Home (Project Overview)"),
                                                # fluidRow(
                                                h3(strong("Are you interested in sustainability and the ",
                                                          a("UN Sustainability Development Goals (SDGs)?", href="https://sdgs.un.org")), 
                                                   "If so, you have come to the right place! 
Right now, the results are from Scopus SDG Search Query. We are working on updating the dashboard with more accurate SDG classification using Machine Learning.
",br(), br(),strong("This 
                           dashboard is a tool that enables you to see which research publications at USC 
                           relate to the 16 UN SDGs (SDG 17 is not included for now). You can use this dashboard as a tool to find your authors and publications that match your academic interest!"),
                                                   br(),br(),"Sustainability incorporates protection for the environment, 
                           balancing a growing economy, and social responsibility to lead to an 
                           improved quality of life for current and future generations. Here, 
                           we have created a program in collaboration with Carnegie Mellon University 
                           to elevate awareness of sustainability in higher education." ),
                                                fluidRow(img(src="Education.png", height="550", style="display: block; margin-left: auto; margin-right: auto;"))
                                              )
                                      ), # end tab item 6
                                      tabItem(tabName = "4",
                                              fluidPage(
                                                h1("USC Research: SDGs By Year"),
                                                #h3("this is a description"),
                                                div(style="font-size:24px;",selectInput(inputId = "Year", 
                                                                                        label = "Choose Year",
                                                                                        choices = sort(unique(publications$Year)))), 
                                                h3("Yearly Total Count of Publications By SDG"),
                                                fluidRow(column(6, plotOutput("year_sdg_barplot"))),
                                                fluidRow(h3("SDG Related Research vs. Non-related Research"),
                                                         plotOutput("pie1")
                                                ))),# end fluid page
                                      tabItem(tabName = "3",
                                              fluidPage(
                                                h1("USC Research: SDGs by Department"),
                                                h3("Select a USC School below to view the number of SDG-related publications by departments.To check out the USC course catalogue, click ", a("here.", href="https://catalogue.usc.edu/")),
                                                div(style="font-size:24px;", selectInput(inputId = "usc_division",
                                                                                         label = "Choose USC School",
                                                                                         selected = "Dornsife College of Letters, Arts and Sciences",
                                                                                         choices = unique(fullinfo$Division))),
                                                h3("(Top 30) Departments of SDG Publications"),
                                                fluidRow(column(12, plotOutput(outputId = "pubs_to_bar"))),
                                                h3("SDG-Related Research"),
                                                fluidRow(column(12, plotOutput("pubs_to_pie")))
                                                
                                              ))
                                    )#end tabitem           
                     ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$year_sdg_barplot <- renderPlot(
    width = 600,
    height = 400,{
      year_sdg_barplot <- publications %>%
        filter(Year == input$Year) %>%
        count(Year,Primary.SDG ) %>%
        mutate(Freq = n) %>%
        ggplot(aes(x = Primary.SDG,y=Freq, fill = factor(as.numeric(Primary.SDG)))) +
        geom_col() +
        geom_text(aes(label = Freq), vjust = -0.2) +
        # geom_hline(yintercept = c(10, 15), color = c("#ffc33c", "#00bc9e")) +
        labs(title = paste0(" (", input$Year, ") ", "Count of Publications Per SDG"),
             fill = "SDG",
             x = "SDG",
             y = "Count of Publications") +
        guides(alpha = FALSE) +
        theme(text = element_text(size = 18)) #+
      #scale_fill_manual(values = sdg_class_keyword_colors)
      
      return(year_sdg_barplot)
    })
  
  output$pie1 <- renderPlot({
    pie_data <- pubwithzero %>% filter(Year == input$Year) 
    sum_notrelated = 0
    #sum_inclusive = 0
    sum_focused = 0
    for (i in 1:nrow(pie_data)){
      if (is.na(pie_data$Primary.SDG[i])){
        sum_notrelated = sum(is.na(pie_data$Primary.SDG))
      }
      
      else {
        sum_focused = sum(!is.na(pie_data$Primary.SDG))
      }
    }
    vals=c(sum_notrelated, sum_focused)
    labels=c("Not Related", "Related")
    pie = data.frame(labels, vals)
    pie = data.frame(labels, vals)
    pie1 <- pie %>% 
      mutate(csum = rev(cumsum(rev(vals))), 
             pos = vals/2 + lead(csum, 1),
             pos = if_else(is.na(pos), vals/2, pos))
    # ggplot(pie, aes(x = "", y = vals, fill = labels)) +
    #   geom_col() +
    #   coord_polar(theta = "y")
    
    # pie(pie$vals, labels=paste(round(prop.table(vals)*100), "%", sep=""), col= c("#767676", "#990000", "#FFC72C"), radius=1)
    
    ggplot(pie, aes(x = "", y = vals, fill = labels)) +
      geom_col(color = "black") +
      # ggtitle("Title") +
      geom_label_repel(data = pie1,
                       aes(y = pos, label = paste0(vals)),
                       size = 4.5, nudge_x = 1, show.legend = FALSE) +
      geom_text(aes(label = vals),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#990000",
                                   "#FFC72C", "#767676")) +
      theme_void()
  })
  output$pubs_to_bar <- renderPlot(
    #width = 800,
    #height = 600,
    {
      pubs_to_bar <- fullinfo %>%
        filter(Division == input$usc_division) %>%
        count(Department,Primary.SDG) %>%
        #group_by(Department) %>%
        mutate(Freq = n) %>%
        #arrange(Department,desc(n)) %>%
        #arrange(desc(sum(Freq))) %>%
        #ungroup() %>%
        #distinct(Department, .keep_all = TRUE) %>%
        #head(30) %>% #  num_top_classes <- 10
        ggplot(aes(x = Department, y = Freq, fill = factor(as.numeric(Primary.SDG)))) +
        #geom_col(position = "stack") +
        geom_col() +
        coord_flip()+
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill"))+
      #geom_text(aes(label = Freq), vjust = -0.2) #+
      labs(#title = paste0("Count of Publications Per SDG"),
           fill = "SDG",
           x = "Departments",
           y = "Count of Publications") +
        #guides(alpha = FALSE) +
        theme(text = element_text(size = 8)) 
      return (pubs_to_bar)
    }
  )
  
  output$pubs_to_pie <- renderPlot(
    #width = 600,
    #height = 400,
    {
      pie_data <- fullinfo %>% filter(Division %in% input$usc_division) 
      sum_sdg1 = 0
      sum_sdg3 = 0
      sum_sdg4 = 0
      sum_sdg2 = 0
      sum_sdg5 = 0
      sum_sdg6 = 0
      sum_sdg7 = 0
      sum_sdg8 = 0
      sum_sdg9 = 0
      sum_sdg10 = 0
      sum_sdg11 = 0
      sum_sdg12 = 0
      sum_sdg13 = 0
      sum_sdg14 = 0
      sum_sdg15 = 0
      sum_sdg16 = 0
      for (i in 1:nrow(pie_data)){
        if (pie_data$Primary.SDG[i]==1){
          sum_sdg1 = sum_sdg1 + 1
        }
        else if (pie_data$Primary.SDG[i]==2){
          sum_sdg2 = sum_sdg2 + 1
        }
        else if (pie_data$Primary.SDG[i]==3){
          sum_sdg3 = sum_sdg3 + 1
        }
        else if (pie_data$Primary.SDG[i]==4){
          sum_sdg4 = sum_sdg4 + 1
        }
        else if (pie_data$Primary.SDG[i]==5){
          sum_sdg5 = sum_sdg5 + 1
        }
        else if (pie_data$Primary.SDG[i]==6){
          sum_sdg6 = sum_sdg6 + 1
        }
        else if (pie_data$Primary.SDG[i]==7){
          sum_sdg7 = sum_sdg7 + 1
        }
        else if (pie_data$Primary.SDG[i]==8){
          sum_sdg8 = sum_sdg8 + 1
        }
        else if (pie_data$Primary.SDG[i]==9){
          sum_sdg9 = sum_sdg9 + 1
        }
        else if (pie_data$Primary.SDG[i]==10){
          sum_sdg10 = sum_sdg10 + 1
        }
        else if (pie_data$Primary.SDG[i]==11){
          sum_sdg11 = sum_sdg11 + 1
        }
        else if (pie_data$Primary.SDG[i]==12){
          sum_sdg12 = sum_sdg12 + 1
        }
        else if (pie_data$Primary.SDG[i]==13){
          sum_sdg13 = sum_sdg13 + 1
        }
        else if (pie_data$Primary.SDG[i]==14){
          sum_sdg14 = sum_sdg14 + 1
        }
        else if (pie_data$Primary.SDG[i]==15){
          sum_sdg15 = sum_sdg15 + 1
        }
        else if (pie_data$Primary.SDG[i]==116){
          sum_sdg16 = sum_sdg16 + 1
        }
      }
      vals=c(sum_sdg1, sum_sdg2, sum_sdg3, sum_sdg4, sum_sdg5, sum_sdg6, sum_sdg7, sum_sdg8, sum_sdg9, sum_sdg10, sum_sdg11, sum_sdg12, sum_sdg13, sum_sdg14, sum_sdg15, sum_sdg16)
      SDG_labels= c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16')
      pie = data.frame(SDG_labels, vals)
      pubs_to_pie <- pie %>% 
        mutate(csum = rev(cumsum(rev(vals))), 
               pos = vals/2 + lead(csum, 1),
               pos = if_else(is.na(pos), vals/2, pos))
      
      
      ggplot(pie, aes(x = "", y = vals, fill = factor(as.numeric(SDG_labels)))) +
        geom_col(color = "black") +
        #ggtitle("SDG-Related Publications") +
        #geom_label_repel(data = pubs_to_pie,
        #aes(y = pos, label = paste0(vals)),
        #size = 4.5, nudge_x = 1, show.legend = FALSE) +
        #geom_text(aes(label = vals),
        #position = position_stack(vjust = 0.5),) +
        labs(fill = "SDG",
             x = "",
             y = "") +
        coord_polar(theta = "y") +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        theme_void()
      
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)