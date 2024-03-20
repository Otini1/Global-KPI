# Load the required packages
library(haven)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggExtra)
library(sitools)
library(gridExtra)
library(readxl)
library(tidyr)
library(writexl)
library(stringr)
library(tidyverse)
library(haven)
library(dplyr)
library(readxl)
library(treemap)
library(flexdashboard)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(waffle)
library(plotly)
library(shinyjs)
library(shinyWidgets)
library(shinythemes) ##newly added

Graph_Indicators <- read_excel("Graph_Indicators.xlsx", 
                               sheet = "Sheet2")

Target_Actual <- read_excel("Graph_Indicators.xlsx", 
                            sheet = "Sheet1")
Targs <- gather(Target_Actual, Variables, Values, Kenya:India)


Trend_analysis <- read_excel("Graph_Indicators.xlsx", 
                             sheet = "Sheet4")

js <- HTML(paste0(c(
  '$(document).on("shiny:value", function(evt) {',
  '  if(evt.name === "wideTable") {',
  '    setTimeout(function() {',
  '      $("#wideTable").freezeTable({',
  '        fastMode: true,',
  '       columnNum: 2',
  '      });',
  '    }, 0);',
  '  }',
  '});'
), collapse = "\n"))

shinyUI(navbarPage(
  
  id="navbar", title = "GLOBAL KPI DASHBOARDS ",inverse = T, position = "fixed-top",
  theme = shinytheme("united"), 
  tabPanel("Home  Page",
           title = "Instruction", 
           value = "Instruction",
           hr(),
           br(), br(),
           
           HTML("<h1><center> <b>GLOBAL KPI DASHBOARDS </b> </center></h1>"),
           
           br(), br(), 
           HTML("<h2>Please read this before viewing and interpreting the Dashboard:<h2>"),
           br(),
           HTML("<h3><b>Purpose:</b></h4>"),
           HTML("<h4>This dashboard is a compilation of the most relevant and comparable KPIs of the DtWI programs as identified by 
                the DtWI M&E Working Group between 2017 and 2019. The goal of this dashboard is to create a central, high-level 
                platform that internal and external stakeholders can use to review and compare DtWI program results and goals.</h4>"),
           br(),
            HTML("<h3><b>Instructions and Organization:</b></h4>"),
           HTML("<h4><b>1.</b> This dashboard provides a high-level view of key performance indicators from all current DtWI geographies in 2020. Previous geographies were not included, but could be added later.</h4>"),
          HTML("<h4><b>2.</b> The tab <b>Data Dictionary</b> provides key context and details of each KPI, including numerator and denominator, collection, and explanation.</h4>"),
           HTML("<h4><b>3</b>. The tab <b>Global KPI</b> provides an overview of all country programs' KPIs side-by-side, including multiple rounds in certain geographies. This is that dashboard that should have focus for initial review and feedback as it has been   most develop. It was decided to use <b>one year</b> view in order to have a good volume of information, without being too busy (though it is still busy).</h4>"),
          HTML("<h4><b>4.</b>. The tab <b>Graphs </b> provides a graphical representation of the Global KPI. In a ddition it provides a comparison of actual results and the targets set for each year</h4>"),
          HTML("<h4><b>5.</b>. The tab <b>Trend </b> provides a graphical representation of the Global KPIs over several years. </h4>"),
         HTML("<h4><b>6.</b> Key notes, caveats, and considerations are included below the dashboard in footnotes, please review these closely.</h4>"),
          br(),
          HTML("<h3><b>Sampling Considerations:</b></h4>"),
          HTML("<h4>Coverage Validation and Process Montioring indicators (Training & Awareness and Program Management categories) are based on random samples of trainings, school observations, and interviews with children.</h4>"),
          HTML("<h4>The sample selection process for Process Montioring indicators is slightly different across geographies, and therefore the sample sizes for each indicator do not represent the same proportion of the overall population across geographies. However, all PM samples are selected to be representative with at least  90% confidence level.</h4>"),
          br(),
          HTML("<h3><b>Data Sources:</b></h4>"),
          HTML("<h4><b>1.</b> Evidence Action PMCV Data</h4>"),
          HTML("<h4><b>2.</b>  Treatment Coverage Data collected on Deworming Day collected by implementing governments.</h4>"),
          br(),
          HTML("<h4> Please review the companion memo for additional context on the methodologies and geographies:</h4>"), 
          HTML("<h4>DtWI Global M&E KPI Dashboard Companion Memo</h4>"), 
          br(),
          HTML("<h3><b>Feedback:</b></h4>"),
          HTML("<h4> Please direct communication of feedback, questions, suggestions, and gaps to <b>Andrew Kitchel</b>.</h4>")
           ),
  tabPanel(
    title = "Data Dictionary",
    value = "data_dict",
    hr(),
    br(), br(),
    
    HTML("<h1><center> <b>DATA DICTIONARY </b> </center></h1>"),
    
    br(),  
    DT::dataTableOutput("mytable")
  ),
  tabPanel(
    title = "Global KPIs",
    value = "kpis",
    hr(),
    br(), br(),
    tabsetPanel(
      tabPanel("KPI Tracker_2020",
    hr(),
    br(), br(),
    HTML("<h1><center> <b>2020 DtWI Global KPI Tracker </b> </center></h1>"),
    
      
    fluidRow(
      tags$head(
        tags$script(
          src = "https://cdn.jsdelivr.net/gh/yidas/jquery-freeze-table/dist/js/freeze-table.min.js"
        ),
        tags$script(js)
      ),
      br(),
    DT::dataTableOutput("mytable2")),
    br(),
    actionButton("viewBtn","View"),
    br(),
    actionButton("saveBtn","Save"),
    br(),
    DT::dataTableOutput("updated.df"),
    br(),
    hr(),
    fluidRow(
    
      HTML("<h4> * Differences in CV methodology between India and other geographies are outlined in the companion memo. </h4>"),
      HTML("<h4> ** CV was conducted in randomly sampled sub-regions of overall implementation in Kenya, Nigeria, and Pakistan, see memo for details </h4>"),
      HTML("<h4> *** In Pakistan, a small subset of all at-risk children were targeted for deworming. These rates are based on a denominator of the targeted population. </h4>"),
      HTML("<h4> **** In KP, schools enrollment data did not allow a target of non-enrolled children. </h4>"),
      HTML("<h4>****** India treatment coverage reporting was disrupted by the COVID-19 pandmic in 2020, and challenges with access to the coverage reporting tool has not allowed for reporting or disaggregation. </h4>"),
      HTML("<h4> - In India, certain indicators were omitted from monitoring surveys due to phone survey deployment during the COVID-19 pandemic </h4>"),
      HTML("<h4> <b>NT</b> = not treated. Some regions and age groups are not treated for Schistosomiasis. </h4>"),
      HTML("<h4> <b>NF</b> = not finalized. Some data has not yet been finalized. </h4>"),
      HTML("<h4> <b>N/A</b> = This activity did not take place in this round of implementation, was not monitored in this geography, or this round of implementation did not take place in this geography.</h4>"),
      HTML("<h4> </h4>")
      
      
    )
  ),
  tabPanel("KPI Tracker_2019",
    hr(),
           br(), br(),
           HTML("<h1><center> <b>2019 DtWI Global KPI Tracker </b> </center></h1>"),
 
    
       fluidRow(
             tags$head(
               tags$script(
                 src = "https://cdn.jsdelivr.net/gh/yidas/jquery-freeze-table/dist/js/freeze-table.min.js"
               ),
               tags$script(js)
             ),
             br(),
             DT::dataTableOutput("mytable2019")),
    br(),
    actionButton("viewBtn","View"),
    br(),
    actionButton("saveBtn","Save"),
    br(),
    DT::dataTableOutput("updated.df"),
    
           br(),
           hr(),
           fluidRow(
             
             HTML("<h4> * Differences in CV methodology between India and other geographies are outlined in the companion memo. </h4>"),
             HTML("<h4> ** CV was conducted in randomly sampled sub-regions of overall implementation in Kenya, Nigeria, and Pakistan, see memo for details </h4>"),
             HTML("<h4> *** In Pakistan, a small subset of all at-risk children were targeted for deworming. These rates are based on a denominator of the targeted population. </h4>"),
             HTML("<h4> **** In KP, schools enrollment data did not allow a target of non-enrolled children. </h4>"),
             HTML("<h4>****** India treatment coverage reporting was disrupted by the COVID-19 pandmic in 2020, and challenges with access to the coverage reporting tool has not allowed for reporting or disaggregation. </h4>"),
             HTML("<h4> - In India, certain indicators were omitted from monitoring surveys due to phone survey deployment during the COVID-19 pandemic </h4>"),
             HTML("<h4> <b>NT</b> = not treated. Some regions and age groups are not treated for Schistosomiasis. </h4>"),
             HTML("<h4> <b>NF</b> = not finalized. Some data has not yet been finalized. </h4>"),
             HTML("<h4> <b>N/A</b> = This activity did not take place in this round of implementation, was not monitored in this geography, or this round of implementation did not take place in this geography.</h4>"),
             HTML("<h4> </h4>")
             
             
           ))
  
  )),
  tabPanel(
    title = "Graphs",
    value = "graphs",
    hr(),
    br(), br(),
    HTML("<h1><center> <b>DtWI Global KPI Graphs </b> </center></h1>"),
    
    br(),  
    fluidRow(
      column(2),
      column(4,selectInput('Indicators','Select Indicator',choice=Graph_Indicators$Indicators,selected ="Training Attendance")),
      column(4,selectInput('Years','Select Year',choice=c(2019,2020,2021),selected=2020)),
      column(2)),
    fluidRow(
    column(3),
    column(6,plotOutput("graph1")),
    column(3)),
    
    br(), hr(), 
    fluidRow(
      column(2),
      column(4,selectInput('Country','Select Country',choice=c("Kenya", "Nigeria","Pakistan", "India"))),
      column(4,selectInput('Years2','Select Year',choice=c(2019,2020,2021),selected=2020)),
      column(2)),
    fluidRow(
      column(3),
      column(6,plotOutput("graph2")),
      column(3)),
    br(), hr(),
    fluidRow(
      column(4,selectInput('Indicators2','Select Indicator',choice=Target_Actual$Indicators,selected = NULL)),
      column(4,selectInput('Country2','Select Country/State/Province',choice=c("Kenya",	"Ogun",	"Oyo",	"Rivers",	"Nigeria",	"ICT",	"Sindh",	"Pakistan",	"Bihar",	"Chhattisgarh R1",	"Chhattisgarh R2","Haryana R1","	Haryana R2",	"Jharkhand R1",	"Jharkhand R2",	"Karnataka R1",	"Karnataka R2",	"Madhya Pradesh",	"Rajasthan",	"Telangana R1",	"Telangana R2",	"Tripura R1",	"Tripura R2",	"Uttar Pradseh",	"Uttarakhand R1",	"Uttarakhand R2",	"India"))),
      column(4,selectInput('Years3','Select Year',choice=c(2019,2020,2021),selected=2020))),
    fluidRow(
      column(3),
      column(6,plotOutput("graph3")),
      column(3))
           
  ),
  tabPanel(
    title = "Trends",
    value = "trends",
    hr(),
    br(), br(),
    HTML("<h1><center> <b>DtWI Global KPI Trend Analysis </b> </center></h1>"),
    
    br(),  
    fluidRow(
      column(3),
      column(6,selectInput('Indicators30','Select Indicator',choice=Graph_Indicators$Indicators,selected ="Training Attendance")),
      #column(4,selectInput('Region','Select Region/Country',choice=Trend_analysis$States),
             column(3)),
      fluidRow(
        column(3),
        column(6,plotlyOutput("trend1")),
        column(3)),
      
      br(), hr(),
    fluidRow(
      column(2),
      column(4,selectInput('Indicators29','Select Indicator',choice=Trend_analysis$Indicators,selected ="Training Attendance")),
      column(4,selectInput('Region','Select Region/Country',choice=Trend_analysis$States),
      column(2)),
    fluidRow(
      column(3),
      column(6,plotlyOutput("trend2")),
      column(3)),
    
    br(), hr() ))
))
           
        