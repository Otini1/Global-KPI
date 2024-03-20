library(readxl)
library(shiny)
library(tidyverse)
library(knitr)
library(kableExtra)
library(DT)
library(plotly)
library(purrr)
library(data.table)


Parent_2020 <- read_excel("Global_KPI.xlsx", 
                          sheet = "2020 Parent ")
Child_2020 <- read_excel("Global_KPI.xlsx", 
                         sheet = "2020 Child")
parents <- data.table(Parent_2020)
Child_2020 <- data.table(Child_2020)



#####2020 Data
# List of inner data tables - list should have length of parents rows
children <- split(Child_2020, f = Child_2020$ID) %>% 
  purrr::map(function(x) {x[, ]})

# ---
# Helping functions
# from https://stackoverflow.com/questions/60662749/parent-child-rows-in-r
# ---
NestedData <- function(Child_2020, children){
  stopifnot(length(children) == nrow(Child_2020))
  g <- function(d){
    if(is.data.frame(d)){
      purrr::transpose(d)
    }else{
      purrr::transpose(NestedData(d[[1]], children = d$children))
    }
  }
  subdats <- lapply(children, g)
  oplus <- sapply(subdats, function(x) if(length(x)) "&oplus;" else "")
  cbind(" " = oplus, Child_2020, "_details" = I(subdats), stringsAsFactors = FALSE)
}

rowNames <- FALSE
colIdx <- as.integer(rowNames)


Dat <- NestedData(
  Child_2020 = parents, 
  children = children
)

parentRows <- which(Dat[, 1] != "")

###2019 Data
Parent_2019 <- read_excel("Global_KPI.xlsx", 
                          sheet = "2019 Parent")
Child_2019 <- read_excel("Global_KPI.xlsx", 
                         sheet = "2019 Child ")
parents2 <- data.table(Parent_2019)
Child_2019 <- data.table(Child_2019)
#####2019 Data 
# List of inner data tables - list should have length of parents rows
children2 <- split(Child_2019, f = Child_2019$ID) %>% 
  purrr::map(function(x) {x[, ]})

# ---
# Helping functions
# from https://stackoverflow.com/questions/60662749/parent-child-rows-in-r
# ---
NestedData2 <- function(Child_2019, children2){
  stopifnot(length(children2) == nrow(Child_2019))
  g <- function(d){
    if(is.data.frame(d)){
      purrr::transpose(d)
    }else{
      purrr::transpose(NestedData2(d[[1]], children2 = d$children2))
    }
  }
  subdats <- lapply(children2, g)
  oplus <- sapply(subdats, function(x) if(length(x)) "&oplus;" else "")
  cbind(" " = oplus, Child_2019, "_details" = I(subdats), stringsAsFactors = FALSE)
}

rowNames2 <- FALSE
colIdx2 <- as.integer(rowNames)


Dat2 <- NestedData2(
  Child_2019 = parents2, 
  children2 = children2
)

parentRows2 <- which(Dat2[, 1] != "")

# make the callback - is dependent on input data (should vanish in future)
callback = JS(
  sprintf("var parentRows = [%s];", toString(parentRows-1)),
  sprintf("var j0 = %d;", colIdx),
  "var nrows = table.rows().count();",
  "for(var i=0; i < nrows; ++i){",
  "  if(parentRows.indexOf(i) > -1){",
  "    table.cell(i,j0).nodes().to$().css({cursor: 'pointer'});",
  "  }else{",
  "    table.cell(i,j0).nodes().to$().removeClass('details-control');",
  "  }",
  "}",
  "",
  "// make the table header of the nested table",
  "var format = function(d, childId){",
  "  if(d != null){",
  "    var html = ",
  "      '<table class=\"display compact hover\" ' + ",
  "      'style=\"padding-left: 30px;\" id=\"' + childId + '\"><thead><tr>';",
  "    for(var key in d[d.length-1][0]){",
  "      html += '<th>' + key + '</th>';",
  "    }",
  "    html += '</tr></thead></table>'",
  "    return html;",
  "  } else {",
  "    return '';",
  "  }",
  "};",
  "",
  "// row callback to style the rows of the child tables",
  "var rowCallback = function(row, dat, displayNum, index){",
  "  if($(row).hasClass('odd')){",
  "  } else {",
  "  }",
  "};",
  "",
  "// header callback to style the header of the child tables",
  "var headerCallback = function(thead, data, start, end, display){",
  "  $('th', thead).css({",
  "    'border-top': '3px solid indigo',",
  "    'color': 'indigo',",
  "  });",
  "};",
  "",
  "// make the datatable",
  "var format_datatable = function(d, childId){",
  "  var dataset = [];",
  "  var n = d.length - 1;",
  "  for(var i = 0; i < d[n].length; i++){",
  "    var datarow = $.map(d[n][i], function (value, index) {",
  "      return [value];",
  "    });",
  "    dataset.push(datarow);",
  "  }",
  "  var id = 'table#' + childId;",
  "  if (Object.keys(d[n][0]).indexOf('_details') === -1) {",
  "    var subtable = $(id).DataTable({",
  "                 'data': dataset,",
  "                 'autoWidth': true,",
  "                 'deferRender': true,",
  "                 'info': false,",
  "                 'lengthChange': false,",
  "                 'ordering': d[n].length > 1,",
  "                 'order': [],",
  "                 'paging': false,",
  "                 'scrollX': false,",
  "                 'scrollY': false,",
  "                 'searching': false,",
  "                 'sortClasses': false,",
  "                 'rowCallback': rowCallback,",
  "                 'headerCallback': headerCallback,",
  "                 'columnDefs': [{targets: '_all', className: 'dt-center'}]",
  "               });",
  "  } else {",
  "    var subtable = $(id).DataTable({",
  "            'data': dataset,",
  "            'autoWidth': true,",
  "            'deferRender': true,",
  "            'info': false,",
  "            'lengthChange': false,",
  "            'ordering': d[n].length > 1,",
  "            'order': [],",
  "            'paging': false,",
  "            'scrollX': false,",
  "            'scrollY': false,",
  "            'searching': false,",
  "            'sortClasses': false,",
  "            'rowCallback': rowCallback,",
  "            'headerCallback': headerCallback,",
  "            'columnDefs': [",
  "              {targets: -1, visible: false},",
  "              {targets: 0, orderable: false, className: 'details-control'},",
  "              {targets: '_all', className: 'dt-center'}",
  "             ]",
  "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
  "  }",
  " subtable.MakeCellsEditable({",
  "    onUpdate: onUpdate,",
  "    confirmationButton: true",
  "  });",
  "};",
  "",
  "// display the child table on click",
  "table.on('click', 'td.details-control', function(){",
  "  var tbl = $(this).closest('table'),",
  "      tblId = tbl.attr('id'),",
  "      td = $(this),",
  "      row = $(tbl).DataTable().row(td.closest('tr')),",
  "      rowIdx = row.index();",
  "  if(row.child.isShown()){",
  "    row.child.hide();",
  "    td.html('&oplus;');",
  "  } else {",
  "    var childId = tblId + '-child-' + rowIdx;",
  "    row.child(format(row.data(), childId)).show();",
  "    td.html('&CircleMinus;');",
  "    format_datatable(row.data(), childId);",
  "  }",
  "});")

path <- "C:/Users/M.Otini/Documents/R/Global KPI/dataTables.cellEdit.js" # folder containing the file dataTables.cellEdit.js
dep <- htmltools::htmlDependency(
  "CellEdit", "1.0.19", 
  path, script = "dataTables.cellEdit.js")



#####AXIS FORMAT
ax <- list(
  title = "Monitoring Period",
  zeroline = TRUE,
  showline = FALSE,
  showticklabels = TRUE,
  showgrid = FALSE
)



Global_KPI_Dashboard <- read_excel("Draft Global KPI Dashboard_2020.xlsx", 
           sheet = "Data Dictionary")

Global_KPI_Dashboard <- Global_KPI_Dashboard%>%
  filter(!is.na(Category ))


Graph_Indicators <- read_excel("Graph_Indicators.xlsx", 
                               sheet = "Sheet2")

Target_Actual <- read_excel("Graph_Indicators.xlsx", 
                            sheet = "Sheet1")
Targs <- gather(Target_Actual, Variables, Values, Kenya:India)

cols <-  c("Target" = "dark grey","Actual" = '#FE6F87')

callback_js <- JS(
  "table.on('click', 'tr.dtrg-group', function () {",
  "  var rowsCollapse = $(this).nextUntil('.dtrg-group');",
  "  $(rowsCollapse).toggleClass('hidden');",
  "});"
)

Trend_analysis <- read_excel("Graph_Indicators.xlsx", 
                             sheet = "Sheet4")

Trend_analysis$Period <- as.factor(Trend_analysis$Period)
# a custom table container
sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, ''),
      th(rowspan = 2, 'Category'),
      th(rowspan = 2, 'ID'),
      th(rowspan = 2, 'Objective'),
      th(rowspan = 2, 'KPI'),
      th(rowspan = 2, 'Numerator'),
      th(rowspan = 2, 'Denominator'),
      th(rowspan = 2, 'Definition'),
      th(rowspan = 2, 'Tool Used'),
      th(rowspan = 2, 'Collected by'),
      th(colspan = 4, 'KPI Collected?')
      
    ),
    tr(
      lapply(rep(c('Kenya', 'Nigeria', 'Pakistan', 'India')), th)
    )
  )
))
print(sketch)

sketch2 = htmltools::withTags(table(
  class = 'display',
  
  thead(
    tr(
      th(rowspan = "2", ''),
      th(rowspan="2" ,'Category'),
      th(rowspan="2" ,'ID'),
      
      th(rowspan="2" ,'Objective'),
      th(rowspan="2" ,'KPI'),
      th(rowspan="2",'Global Targets'),
      
      th(rowspan="1",'Kenya'),
      th(colspan="4" ,rowspan="1" ,'Nigeria'),
      th(colspan="3", rowspan="1" ,'Pakistan'),
      th(colspan="19", rowspan="1" ,'India')),
    tr(
      th(colspan="1" ,rowspan="1" ,'2020 Total'),
      th(colspan="1" ,rowspan="1" ,'Ogun'),
      th(colspan="1", rowspan="1" ,'Oyo'),
      th(colspan="1", rowspan="1" ,'Rivers'),
      th(colspan="1", rowspan="1",'2020 Total'),
      th(colspan="1", rowspan="1" ,'ICT'),
      th(colspan="1", rowspan="1" ,'Sindh'),
      th(colspan="1", rowspan="1", '2020 Total'),
      th(colspan="1",rowspan="1" ,'Bihar August'),
      th(colspan="1", rowspan="1" ,'Chhattisgarh February'),
      th(colspan="1", rowspan="1" ,'Chhattisgarh August'),
      th(colspan="1", rowspan="1" ,'Haryana February'),
      th(colspan="1", rowspan="1" ,'Haryana August'),
      th(colspan="1" ,rowspan="1" ,'Jharkhand February'),
      th(colspan="1" ,rowspan="1" ,'Jharkhand August'),
      th(colspan="1", rowspan="1" ,'Karnataka February'),
      th(colspan="1", rowspan="1" ,'Karnataka August'),
      th(colspan="1", rowspan="1" ,'Madhya Pradesh August'),
      th(colspan="1", rowspan="1" ,'Rajasthan August'),
      th(colspan="1", rowspan="1" ,'Telangana February'),
      th(colspan="1", rowspan="1" ,'Telangana August'),
      th(colspan="1", rowspan="1" ,'Tripura February'),
      th(colspan="1", rowspan="1" ,'Tripura August'),
      th(colspan="1",rowspan="1" ,'Uttar Pradseh August'),
      th(colspan="1", rowspan="1" ,'Uttarakhand February'),
      th(colspan="1", rowspan="1" ,'Uttarakhand August'),
      th(colspan="1", rowspan="1", '2020 Total'))
    
  )))
print(sketch2)

sketch2019 = htmltools::withTags(table(
  class = 'display',
  
  thead(
    tr(
      th(rowspan="2" ,''),
      th(rowspan="2" ,'Category'),
      th(rowspan="2" ,'ID'),
      th(rowspan="2" ,'Objective'),
      th(rowspan="2" ,'KPI'),
      th(rowspan="2",'Global Targets'),
      th(rowspan="1",'Kenya'),
      th(colspan="8" ,rowspan="1" ,'Nigeria'),
      th(colspan="4", rowspan="1" ,'Pakistan'),
      th(colspan="21", rowspan="1" ,'India')),
    tr(
      th(colspan="1", rowspan="1",'2019 Total'),
      th(colspan="1" ,rowspan="1" ,'Ogun Round 1'),
      th(colspan="1" ,rowspan="1" ,'Ogun Round 2'),
      th(colspan="1" ,rowspan="1" ,'Oyo Round 1'),
      th(colspan="1" ,rowspan="1" ,'Oyo Round 2'),
      th(colspan="1" ,rowspan="1" ,'Rivers Round 1'),
      th(colspan="1" ,rowspan="1" ,'Rivers Round 2'),
      th(colspan="1", rowspan="1" ,'Cross Rivers'),
      th(colspan="1", rowspan="1",'2019 Total'),
      th(colspan="1", rowspan="1" ,'ICT February'),
      th(colspan="1", rowspan="1" ,'ICT April'),
      th(colspan="1", rowspan="1" ,'KP'),
      th(colspan="1", rowspan="1", '2019 Total'),
      th(colspan="1", rowspan="1" ,'Bihar February'),
      th(colspan="1", rowspan="1" ,'Bihar August'),
      th(colspan="1", rowspan="1" ,'Chhattisgarh February'),
      th(colspan="1", rowspan="1" ,'Chhattisgarh August'),
      th(colspan="1", rowspan="1" ,'Haryana February'),
      th(colspan="1", rowspan="1" ,'Haryana August'),
      th(colspan="1" ,rowspan="1" ,'Jharkhand February'),
      th(colspan="1" ,rowspan="1" ,'Jharkhand August'),
      th(colspan="1", rowspan="1" ,'Karnataka February'),
      th(colspan="1", rowspan="1" ,'Karnataka August'),
      th(colspan="1", rowspan="1" ,'Madhya Pradesh August'),
      th(colspan="1", rowspan="1" ,'Rajasthan August'),
      th(colspan="1", rowspan="1" ,'Telangana February'),
      th(colspan="1", rowspan="1" ,'Telangana August'),
      th(colspan="1", rowspan="1" ,'Tripura February'),
      th(colspan="1", rowspan="1" ,'Tripura August'),
      th(colspan="1",rowspan="1" ,'Uttar Pradseh February'),
      th(colspan="1",rowspan="1" ,'Uttar Pradseh August'),
      th(colspan="1", rowspan="1" ,'Uttarakhand February'),
      th(colspan="1", rowspan="1" ,'Uttarakhand August'),
      th(colspan="1", rowspan="1", '2019 Total'))
  )))
print(sketch2019)

shinyServer(function(input, output, session) {
  
  output$mytable = renderDataTable({
    
    dataTableProxy(outputId = 'mytable') %>%
      hideCols(hide = c(0,2))
    
    
    # datatable(Global_KPI_Dashboard, options = list(pageLength = 30, dom = 'tip'), rownames = FALSE, class = "compact") # or use "hover"
    datatable(Global_KPI_Dashboard[1:30, c(1, 1:13)], editable = TRUE, container = sketch, rownames = F,
              selection = 'none',  filter = 'none',
              extensions = c('Buttons', 'Scroller', "Responsive"),
              options = list(
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': 'pink','color': 'black','font-size': '100%'});",
                  "}"),
                buttons = list('excel',
                               list(extend = 'colvis', targets = 3, visible = FALSE)),
                dom = 'lBfrtip',
                columnDefs = list(list(targets = c(0, 2), visible = TRUE)),
                paging = F, searching = TRUE, info = FALSE,
                sort = TRUE, scrollX = TRUE, scrollY = "1200px", fixedRow =  2, fixedColumns = list(leftColumns = 2)))
    
  })
  
  ##KPIs
  
  
  output$mytable2 = renderDT({
    
    dataTableProxy(outputId = 'mytable2') %>%
      hideCols(hide = c(3,8,9,10,12,13,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32))
    
   d1 <- datatable(
      Dat, container = sketch2, callback = callback, selection = 'none', editable = TRUE, rownames = rowNames,
      escape = - colIdx - 1,
      extensions = c('Buttons'),
      
      options = list(
        
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': 'pink','color': 'black','font-size': '100%'});",
          "}"),
        scrollY = 650,
        deferRender = TRUE,
        scroller = TRUE,
        # paging = TRUE,
        # pageLength = 25,
        buttons = list('excel',
                       list(extend = 'colvis', targets = c(0,2), visible = FALSE)),
        dom = "t",
        buttons = c('csv', 'excel'),
        columnDefs = list(
          list(visible = FALSE, targets = ncol(Dat)-1+colIdx),
          list(orderable = FALSE, className = 'details-control',
               targets = colIdx),
          list(className = "dt-center", targets = "_all")
        )
      )
    )
   })
  

  observeEvent(input$banking.df_data_cell_edit, {
    Dat[input$banking.df_data_cell_edit$row,input$banking.df_data_cell_edit$col] <<- input$banking.df_data_cell_edit$value
  })
  
  view_fun <- eventReactive(input$viewBtn,{
    if(is.null(input$saveBtn)||input$saveBtn==0)
    {
      returnValue()
    }
    else
    {
      DT::datatable(Dat,selection = 'none')
    }
    
  })
  
  
  observeEvent(input$saveBtn,{
    cat("nn* Saving table in directory: ", getwd())
    file_name <- 'test.csv'
    curr_dt_time <- as.character(Sys.time())
    curr_dt_time <- gsub(":", "_", curr_dt_time  )
    if(file.exists(file_name)){
      df <- read.csv(file = file_name)
      
      write.csv(df, paste0(file_name,"_last_save_",curr_dt_time,".csv")
                #, row.names = FALSE
      )
    }
    write.csv(Dat,'test.csv')
  })
  
  output$updated.df<-renderDataTable({
    view_fun()
  }
  )
  
  cat("nLaunching   'shinyApp' ....")  


 
  output$mytable2019 = renderDT({
    
    dataTableProxy(outputId = 'mytable2019') %>%
      hideCols(hide = c(3,7,8,9,10,11,12,13,15,16,17,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40))
    
    
    # datatable(Global_KPI_Dashboard, options = list(pageLength = 30, dom = 'tip'), rownames = FALSE, class = "compact") # or use "hover"
    datatable(Dat2,callback = callback, selection = 'none', editable = TRUE, rownames = rowNames,
              escape = - colIdx - 1,
              extensions = c('Buttons'),
              
              options = list(
                
                initComplete = JS(
                  "function(settings, json) {",
                  "$(this.api().table().header()).css({'background-color': 'pink','color': 'black','font-size': '100%'});",
                  "}"),
                scrollY = 650,
                deferRender = TRUE,
                scroller = TRUE,
                # paging = TRUE,
                # pageLength = 25,
                buttons = list('excel',
                               list(extend = 'colvis', targets = c(0,2), visible = FALSE)),
                dom = "t",
                buttons = c('csv', 'excel'),
                columnDefs = list(
                  list(visible = FALSE, targets = ncol(Dat)-1+colIdx),
                  list(orderable = FALSE, className = 'details-control',
                       targets = colIdx),
                  list(className = "dt-center", targets = "_all"))))
    
    
    
    
  })
  
  observeEvent(input$banking.df_data_cell_edit2, {
    Dat2[input$banking.df_data_cell_edit2$row,input$banking.df_data_cell_edit2$col] <<- input$banking.df_data_cell_edit2$value
  })
  
  view_fun2 <- eventReactive(input$viewBtn2,{
    if(is.null(input$saveBtn2)||input$saveBtn2==0)
    {
      returnValue()
    }
    else
    {
      DT::datatable(Dat2,selection = 'none')
    }
    
  })
  
  
  observeEvent(input$saveBtn2,{
    cat("nn* Saving table in directory: ", getwd())
    file_name <- 'test.csv'
    curr_dt_time <- as.character(Sys.time())
    curr_dt_time <- gsub(":", "_", curr_dt_time  )
    if(file.exists(file_name)){
      df <- read.csv(file = file_name)
      
      write.csv(df, paste0(file_name,"_last_save_",curr_dt_time,".csv")
                #, row.names = FALSE
      )
    }
    write.csv(Dat2,'test.csv')
  })
  
  output$updated.df2<-renderDataTable({
    view_fun2()
  }
  )
  
  cat("nLaunching   'shinyApp' ....")  
  
  #####Country comparisons of indicidual indicators 
  country_indicators <- reactive({
    Graph_Indicators%>%
      filter(Indicators == input$Indicators & Year == input$Years)
  }) 
  
  output$graph1 <- renderPlot({
    validate(
      need(country_indicators()$Year, 'Please select an Indicator'))
    ggplot(data = country_indicators(), aes(x = reorder(Country,Values), y = Values, fill = Country))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      #geom_text(aes(label=values), vjust=-0.3, size=3.5) +
      geom_text(aes(label = paste(round(Values*100),"%",sep=""),  hjust = "right", size = 4),
                colour = "black",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()})
  
  ######Graphs showing all the indicators per countries 
  all_indicators <- reactive({
    Graph_Indicators%>%
      filter(Year == input$Years2 & Country == input$Country)
  }) 
  
  output$graph2 <- renderPlot({
    validate(
      need(all_indicators()$Year, ('Please select an Indicator')))
    ggplot(data = all_indicators(), aes(x = reorder(Indicators,Values), y = Values, fill = Country))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = c('#FE6F87'), aesthetics = "fill") +
      geom_text(aes(label = paste(round(Values*100),"%",sep=""),  hjust = "right", size = 4),
                colour = "black",
                position = position_dodge(width = .0),
                size = 6,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()})
  
  
  ##############Comparison between Target and Actual for Each Indicator Per Country?State/Province  
  
  target_acts <- reactive({
    Targs%>%
      filter(Year == input$Years3 & Indicators == input$Indicators2 & Variables == input$Country2)
  }) 
  
  output$graph3 <- renderPlot({
    validate(
      need(target_acts()$Year, 'Please select an Indicator'))
    ggplot(data = target_acts(), aes(x = (Taget_Actual), y =  Values, fill = Taget_Actual))+
      geom_bar(stat = 'identity', position = "stack",  width = 0.4) +
      scale_fill_manual(values = cols, aesthetics = "fill") +
      geom_text(aes(label = paste(round( Values*100),"%",sep=""),  hjust = "right", size = 4),
                colour = "black",
                position = position_dodge(width = .0),
                size = 5,
                show.legend = FALSE,
                fontface="bold") +
      theme(plot.title = element_text(color="Black",  size=22, hjust=0)) +
      theme(axis.text.y = element_text(color="BlACK",size=14)) +
      theme(axis.title.x = element_blank(), 
            axis.ticks.x =  element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            #axis.text.y = element_blank(), 
            axis.ticks.y = element_blank(),
            plot.margin = unit(c(1,1,1,0), "mm"),
            legend.position = "none",
            panel.border = element_blank()) +
      coord_flip()+
      removeGrid()})
  
  #####Country comparisons of indicidual indicators across Different years
  trend_indicators <- reactive({
    Trend_analysis%>%
      filter(Indicators == input$Indicators29 & States == input$Region & Taget_Actual == "Actual")
  }) 
  
  output$trend2 <- renderPlotly({
    validate(
      need(trend_indicators()$Year, 'Please select an Indicator'))
    plot_ly(trend_indicators(), x=~Period, y=~Values, group=~Variables,
            type="scatter",color=~Variables, mode="lines+markers")%>%
      layout(title = "", yaxis = list(title = "Percentage",  showticklabels = TRUE,
                                      showgrid = FALSE, ticksuffix = "%",  range = c(0, 100)), xaxis = ax)  })
  
  #####Country comparisons of indicidual indicators 
  country_indicators_tr <- reactive({
    Graph_Indicators%>%
      filter(Indicators == input$Indicators30)%>%
      mutate(vals = Values*100,
             Year = as.character(Year))
  }) 
  
  output$trend1 <- renderPlotly({
    validate(
      need(country_indicators_tr()$Year, 'Please select an Indicator'))
    plot_ly(country_indicators_tr(), x=~Year, y=~vals, group=~Country,
            type="scatter",color=~Country, mode="lines+markers")%>%
      layout(title = "", yaxis = list(title = "Percentage",  showticklabels = TRUE,
                                      showgrid = FALSE, ticksuffix = "%",  range = c(0, 100)), xaxis = ax)
    
    
  })
})
