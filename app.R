
library(shiny)
library(dplyr)
library(shinybusy)
library(knitr)
library(kableExtra)
library(shinythemes)
library(shinyWidgets)
library(openxlsx)
library(zip)
library(DT)
library(duckdb)
library(tidyverse)
library(DBI)
library(waiter)
library(shinyBS)
library(shinyalert)


# Bring in nest data    --------------------------------------------------------------------------------------
# 
# source('functions/nesting_functions.R')
load('data/nest_data.Rdata')

# bring in data for inputs ----------------------------------------------------------------------------------------

source('functions/load_data.R')


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("DRAFT [INTERNAL USE ONLY] IR 2024 Integrated Report"),
  waiter::use_waiter(), 
  useShinyalert(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 2,
      actionButton("go", "Filter",  icon("filter")),
      bsTooltip(id = "go", 
                title = "Not inputting any filters will return all 2024 IR decisions"),
     
      
      selectizeInput("AUs",
                     "Select Assessment Unit",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(maxOptions = 7000)),
      
      selectizeInput("Select_AUName",
                     "Select AU Name",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(maxOptions = 7000)),
      #   selectizeInput("admin_basin_selector",
      #                  "Select Admin Basin",
      #                  choices = AU_s,
      #                  multiple = TRUE),
      selectizeInput("pollutant_selector",
                     "Select Pollutant",
                     choices = pollutants,
                     multiple = TRUE),
      selectizeInput("category_selector",
                     "Select IR category",
                     choices = Parameter_category,
                     multiple = TRUE),
      selectizeInput("status_selector",
                     "Select Parameter Attainment Status",
                     choices =status,
                     multiple = TRUE),
      selectizeInput("status_change_selector",
                     "Select 2024 change status",
                     choices =status_change,
                     multiple = TRUE),
      selectizeInput("huc4_selector",
                     "Select HUC 4",
                     choices =huc4_name,
                     multiple = TRUE),
      selectizeInput("huc6_selector",
                     "Select HUC 6",
                     choices =huc6_name,
                     multiple = TRUE),
      selectizeInput("huc8_selector",
                     "Select HUC 8",
                     choices =huc8_name,
                     multiple = TRUE),
      selectizeInput("huc10_selector",
                     "Select HUC 10",
                     choices =huc10_name,
                     multiple = TRUE),
      checkboxInput("permitcheck", label = "Permitee Data", value = FALSE),
      bsTooltip(id = "permitcheck", 
                title = "This will go away when available to the public"),

      
      
      
      
      #   
      #   selectizeInput("benuse_selector",
      #                  "Select Beneficial Use",
      #                  choices =ben_uses,
      #                  multiple = TRUE)
      # )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      downloadButton("dl", "Download"),
      bsTooltip(id = "dl", 
                title = "Download an excel file of filtered assessment decisions"),
      div(DTOutput("table"), style = "font-size:85%") 
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Build loading screen
  w <- waiter::Waiter$new(
    html = shiny::tagList(
      "Querying Data...", 
      waiter::spin_ball()
    )
  )
  
  shinyalert("Internal Use Only", "The 2024 database is not yet at draft stage. Please do not share outside the agency.", type = "info")
  
  updateSelectizeInput(session, 'AUs', choices = AU_s, server = TRUE)
  updateSelectizeInput(session, 'Select_AUName', choices = AU_Names, server = TRUE)
  
  
  
  
  AU_data <- eventReactive(input$go,{
    
    con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb", read_only = TRUE)
    
    t <-tbl(con, "AU_decisions") 
    
    if (!is.null(input$AUs)){
      
      AU_select <- input$AUs
      
      t <- t %>%
        filter(AU_ID %in% AU_select)
    }
    
    if (!is.null(input$Select_AUName)){
      
      AU_name <- input$Select_AUName
      
      t <- t %>%
        filter(AU_Name %in% AU_name)
    }
    
    if (!is.null(input$pollutant_selector)){
      
      pollutant <- input$pollutant_selector
      
      t <- t %>%
        filter(Char_Name %in% pollutant)
    }
    
    if (!is.null(input$category_selector)){
      
      category <- input$category_selector
      
      t <- t %>%
        filter(final_AU_cat %in% category)
    }
    
    if (!is.null(input$status_selector)){
      
      status <- input$status_selector
      
      t <- t %>%
        mutate(param_status = case_when(grepl('5', final_AU_cat) | grepl('4', final_AU_cat) ~ "Impaired",
                                        grepl('3', final_AU_cat) ~ "Insufficient",
                                        grepl('2',final_AU_cat) ~ 'Attains',
                                        TRUE ~ 'Unassessed')) %>%
        filter(param_status %in% status) %>%
        select(-param_status)
    }
    
    if (!is.null(input$status_change_selector)){
      
      stat_change <- input$status_change_selector
      
      t <- t %>%
        filter(status_change %in% stat_change)
    }
    
    if (!is.null(input$huc4_selector)){
      
      huc4 <- input$huc4_selector
      
      t <- t %>%
        filter(HUC4_NAME %in% huc4)
    }
    
    if (!is.null(input$huc6_selector)){
      
      huc6 <- input$huc6_selector
      
      t <- t %>%
        filter(HUC6_NAME %in% huc6)
    }
    
    if (!is.null(input$huc8_selector)){
      
      huc8 <- input$huc8_selector
      
      t <- t %>%
        filter(HUC8_NAME %in% huc8)
    }
    
    if (!is.null(input$huc10_selector)){
      
      huc8 <- input$huc10_selector
      
      t <- t %>%
        filter(HUC10_NAME %in% huc10)
    }
    
    
    if (input$permitcheck){
      

      
      t <- t %>%
        filter(permitee)
    }
    
    
    
    
    
    
    
    t <- t %>%
      select(-HUC12_NAME, -HUC10,-HUC10_NAME,
             -HUC8, -HUC8_NAME, -HUC6, -HUC6_NAME, -HUC4,
             -HUC4_NAME, -recordID)%>%
      collect()
    
    dbDisconnect(con, shutdown=TRUE)
    
    t
    
  })
  
  
  
  GNIS_data <- eventReactive(input$go,{

    con <- dbConnect(duckdb(), dbdir = "data/decisions.duckdb", read_only = TRUE)

    gnis_AUs <- AU_data()$AU_ID

    g <- tbl(con, "GNIS_decisions") %>%
      filter(AU_ID %in% gnis_AUs) %>%
      collect()


    dbDisconnect(con, shutdown=TRUE)

    g

  })


  combined_data <- eventReactive(input$go,{
    w$show()
    
    data <- AU_data()
    
    
    Dat <-data %>% 
      left_join(nest_data) %>% 
      mutate(" " = case_when(lengths(`_details`) ==0 ~ "",
                             TRUE ~"&#11208;")) %>%
      relocate(" ")
    
    
    w$hide()
    return(Dat)
  })
  Dat <- reactive({combined_data()
  })
  rowNames <- FALSE
  colIdx <- as.integer(rowNames)
  
  
  ## make the callback
  parentRows <- 1
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
    "    $(row).css('background-color', '#C0DDE1FF');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#C0DDE1FF');",
    "    }, function() {",
    "      $(this).css('background-color', '#C0DDE1FF');",
    "    });",
    "  } else {",
    "    $(row).css('background-color', '#C0DDE1FF');",
    "    $(row).hover(function(){",
    "      $(this).css('background-color', '#C0DDE1FF');",
    "    }, function() {",
    "      $(this).css('background-color', '#C0DDE1FF');",
    "    });",
    "  }",
    "};",
    "",
    "// header callback to style the header of the child tables",
    "var headerCallback = function(thead, data, start, end, display){",
    "  $('th', thead).css({",
    "    'border-top': '3px solid indigo',", 
    "    'color': 'indigo',",
    "    'background-color': '#0E84B4FF'",
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
    "                 'columnDefs': [{targets: '_all', className: 'dt-left'}]",
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
    "              {targets: '_all', className: 'dt-left'}",
    "             ]",
    "          }).column(0).nodes().to$().css({cursor: 'pointer'});",
    "  }",
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
    "    td.html('&#11208;');",
    "  } else {",
    "    var childId = tblId + '-child-' + rowIdx;",
    "    row.child(format(row.data(), childId)).show();",
    "    td.html('&#11206;');",
    "    format_datatable(row.data(), childId);",
    "  }",
    "});")
  
  output$table <- DT::renderDataTable({
    
    
    
    
    ## the datatable
    datatable(
      combined_data(), 
      #extensions = 'Buttons',
      
      callback = callback, rownames = rowNames, escape = -colIdx-1,
      #filter = "top",
      selection = 'none',
      #next line adds grid lines
      class = 'cell-border stripe',
      options = list(
        autoWidth = TRUE,
        pageLength = 100,
        columnDefs = list(
          list(visible = FALSE,
               targets = '_details'),
          list(orderable = FALSE, className = 'details-control', targets = colIdx),
          list(className = "dt-left", targets = "_all")
        ),
        dom = 'Bfrtip'#,
       # buttons = c('copy', 'csv', 'excel')
      )
    )
  })
  
  output$dl <- downloadHandler(
    filename = function() { "Oregon 2024 IR- filtered.xlsx"},
    content = function(filenm) {write.xlsx(list('AU Decisions' = AU_data(), 'GNIS Decisions' = GNIS_data()), file = filenm)}
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
