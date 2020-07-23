library(shiny)
library(shinyWidgets)
library(shinyFiles)

ui <- fluidPage(
  titlePanel("Data Analysis - Definition of ROI"),
  sidebarPanel(
    
    #--- Input Datafile, Load excel sheets data-------------------------
    strong("Select Excel File"),
    br(),
    
        fluidRow(
      column(2,
             shinyFilesButton(id="roi_xlsx_file",
                              label = "", 
                              title = "Please select a xlsx file", 
                              icon = icon("file"),
                              multiple = FALSE, 
                              viewtype = "detail")
             ),
      column(5,
             div(style="margin-top: 8px;",
             textOutput("roi_text_selected_xlsx_file"))
      ))
    ,
    
    br(),                                                      
    conditionalPanel(
      condition = "input.roi_xlsx_file",                          
      actionButton("roi_button_load_sheet_options", label = "Load Excel Sheet Selection")
    ),
    uiOutput("roi_excel_sheet_options"),
    uiOutput("roi_button_load_excel_sheet"),
    
    #--- Additional information as text-------------------------------
    conditionalPanel(
      condition = "input.roi_button_load_selected_excel_sheet%2==1",
      textOutput("roi_text_excel_file_name"),
      textOutput("roi_text_selected_excel_sheet"),
      textOutput("roi_text_THGEM_type"),
      textOutput("roi_text_file_locations")
    ),
    
    tags$hr(style="border-color: darkgrey;"),
    #--- Meas. Number, Show data table, plot CpT-----------------------------------
    fluidRow(
      column(12,
             tags$table(width = "100%",
                        tags$tr(width = "100%",
                                #Meas No
                                tags$td(width = "60%", div(style = "", "Meas. No.",circleButton(inputId = "roi_info_meas_no", icon = icon("info"), size = "xs"))),
                                tags$td(width = "40%", numericInput(inputId = "roi_meas_no", label = NULL, 
                                                                    value= 1, min=1, step=1))#,
                        )))),
    
    actionButton("roi_button_show_dataTable_meas_info", label = "Show Measurement Information"),
    actionButton("roi_button_show_Plot_CpT", label = "Show Plot of Measurement Data"),
    br(),
    
    #--- Additional Comments------------------------------
    fluidRow(
      column(12,
             tags$table(width = "100%",
                        tags$tr(width = "100%",
                                #Additional Comments
                                tags$td(width = "20%", div(style = "", "Add. Comments")),
                                tags$td(width = "60%", textInput("roi_meas_description", "", placeholder = "No comment")),
                                tags$td(width = "5%", actionButton("roi_button_update_additional_comments", label = NULL,icon = icon("refresh"))),
                                tags$style(type='text/css', "#roi_button_update_additional_comments { width:100%; margin-top: 4px;}")
                        ),
             ))),    
    #--- Curve Shape, ROI Calculation, ROI results-------------------------------
    radioButtons(inputId ="roi_curve_shape", 
                 label = "Select Curve Shape",
                 choices = c("0 - To be determined", 
                             "1 - Exp. Decreasing", 
                             "2 - Linear", 
                             "3 - Exp. Increasing",
                             "4 - Other")),
    textOutput("roi_warning_curve_shape"),
    tags$style("#roi_warning_curve_shape{color: red;}"),
    uiOutput("roi_button_warning_curve_shape"),
    
    br(),
    textOutput("roi_text_alph_start"),
    textOutput("roi_text_alph_end"),
    actionButton("roi_button_next_meas", label = "Continue with next Measurement"),
    
    tags$hr(style="border-color: darkgrey;"),
    
    #--- Save back to Excel------------------------------
    actionButton("roi_button_save_back_to_excel", label="Save Calculations to Excel File"),
    textOutput("warning_save_excel_file"),
    tags$head(tags$style("#warning_save_excel_file{color: red;}")),
    
    tags$hr(style="border-color: darkgrey;"),
    #--- More details button-----------------------------------------------------------
    actionButton("roi_button_more_Details", label= NULL, icon=icon("cogs")),
    br(),
    conditionalPanel(
      condition = "input.roi_button_more_Details%2==1",
      fluidRow(
        column(12,
               tags$table(width = "100%",
                          tags$tr(width = "100%",
                                  #Margin
                                  tags$td(width = "50%", tags$div(style = "", "Margin")),
                                  tags$td(width = "50%", numericInput(inputId = "roi_margin", label = NULL,
                                                                      value = 0.01, min=0.005,step= 0.005))
                          ),
                          tags$tr(width = "100%",
                                  #No. Excel Rows
                                  tags$td(width = "50%", tags$div(style = "", "No. Excel Rows")),
                                  tags$td(width = "50%", numericInput(inputId = "roi_excel_nrows", label = NULL,
                                                                      value = 100,step= 1))
                          ),
                          tags$tr(width = "100%",
                                  #No. Excel Columns
                                  tags$td(width = "30%", tags$div(style = "", "No. Excel Columns")),
                                  tags$td(width = "70%", numericInput(inputId = "roi_excel_ncols", label = NULL,
                                                                      value = 180,step= 1))
                          ),
                          tags$tr(width = "100%",
                                  #Alpha packet size
                                  tags$td(width = "50%", tags$div(style = "", "Alpha Packet Size")),
                                  tags$td(width = "50%", numericInput(inputId = "roi_alpha_packet_size", label = NULL,
                                                                      value = 100,step= 1))
                          ),
               )
        )      
      )   
      
    )
    
  ),
  #--- Main Panel: Plot CpT, dataTables------------------------------
  mainPanel(
    
    plotOutput("roi_plot_CpT", width = "100%", height = "600px"),
    tableOutput("roi_dataTable_meas_info1"),
    tableOutput("roi_dataTable_meas_info2")
  )
  
)