library(shiny)
library(shinyWidgets)
library(shinyFiles)

library(readxl)    # Import library to use recode() function
library(openxlsx)
library(dplyr)
library("viridis")  #Color

library(drc)
library(nlme)

#Load all functions from R folder.
#sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)

server <-shinyServer(function(input, output, session) {
  #---------------------------------------------------------------------------------
  # Initialize reactive values
  roi_values <- reactiveValues()
  volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
  #volumes <- c(wd='.')
  shinyFileChoose(input, "roi_xlsx_file", roots = volumes, session = session)
  observeEvent(input$roi_xlsx_file,{

    #Extract info from selected file
    roi_values$parse_file_paths <- parseFilePaths(volumes, input$roi_xlsx_file)

    #Paste together total datapath to selected Excel File
    #roi_values$roi_xlsx_file_datapath <- paste(getwd(), gsub('^.', '',roi_values$parse_file_paths$datapath ), sep="")
    roi_values$roi_xlsx_file_datapath <- paste("C",gsub('^.', '',roi_values$parse_file_paths$datapath ), sep="")

    roi_values$roi_xlsx_file_name <- roi_values$parse_file_paths$name

    cat("\nDatapath: ",roi_values$roi_xlsx_file_datapath)
    cat("\nFile Name: ",roi_values$roi_xlsx_file_name )

    output$roi_text_selected_xlsx_file <- renderText({
      roi_values$roi_xlsx_file_name
    })
  })



  #---Text Outputs with Measurement Info------------------------------------------------------------------------------
  #Outputs Excel File Name as text

  output$roi_text_excel_file_name <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("Excel File: ",roi_values$roi_xlsx_file_name)
  })


  #Outputs Excel File Sheet Name as text
  output$roi_text_selected_excel_sheet <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("Selected Sheet: ",input$roi_excel_sheet)
  })
  #Outputs THGEM type
  output$roi_text_THGEM_type <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("THGEM: ",roi_values$gem_type)
  })
  #Outputs File Location of Measurement files
  output$roi_text_file_locations <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("File Locations: ",roi_values$file_locations)
  })

  #Outputs alpha start
  output$roi_text_alph_start <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("Alpha Start: ",roi_values$alph_start)
  })

  #outputs alpha end
  output$roi_text_alph_end <- renderText({
    req(input$roi_button_load_sheet_options)
    paste("Alpha End: ",roi_values$alph_end)
  })


  #---Load Excel File------------------------------------------------------------
  #Check whether new file has been uploaded, if so start loading said file
  #Read Info Sheet and extract File Locations an THGEM Type
  #Open Excel Workbook and extract all Sheet Names, create drop down menu for user selection

  observeEvent(input$roi_button_load_sheet_options, {
    req(input$roi_button_load_sheet_options)
    cat("\n--- Wohow, a file has been selected! Let's get to work! Yip Yip! -----------------------")

    #Open Excel Sheet Info and copy out File Locations an THGEM Type


    cat("\n--- Excel Info Sheet Extraction started ------------------------------------------------")
    print(roi_values$roi_xlsx_file_name)
    print(roi_values$roi_xlsx_file_datapath)
    roi_values$data_info <- extract_info_from_xlsx(excel_file = roi_values$roi_xlsx_file_datapath)

    roi_values$gem_type <- roi_values$data_info$gem_type
    roi_values$file_locations <- roi_values$data_info$file_locations
    cat("\n--- Excel Info Sheet Extraction completed ----------------------------------------------")

    cat("\n--- Loading List of Excel Sheets started -----------------------------------------------")

    #get Sheet Names from Excel File
    sheetNames = openxlsx::getSheetNames(roi_values$roi_xlsx_file_datapath)
    cat("\n",sheetNames)

    output$roi_excel_sheet_options = renderUI({
      selectInput(inputId = "roi_excel_sheet",
                  label = "Select Worksheet",
                  choices = c(sheetNames),
                  selected = 'M1')
    })
    output$roi_button_load_excel_sheet <- renderUI({
      actionButton("roi_button_load_selected_excel_sheet", label = "Load Selected Excel Sheet Data")
    })
    cat("\n****************************************************************************************")

  })



  #---Load Measurement data from Data sheet into roi_values$data---------
  observeEvent(input$roi_button_load_selected_excel_sheet, {
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    cat("\n--- Loading Measurement Data from Sheet", input$roi_excel_sheet, "started -------------------------------------\n")
    # Function to get measurement number from excel row
    roi_values$data <- extract_dataFrame_from_xlsx(excel_file = roi_values$roi_xlsx_file_datapath,
                                                   excel_sheet_name =input$roi_excel_sheet,
                                                   excel_nrows = input$roi_excel_nrows,
                                                   excel_ncols = input$roi_excel_ncols)

    cat("\n****************************************************************************************")

  })






  #---Plot CpT------------------------------------------------------------------------------
  observeEvent(input$roi_button_show_Plot_CpT, {
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    req(roi_values$data)
    req(is.na(input$roi_meas_no)==FALSE)
    cat("\n --- Starting plotting of measurement data ------------------")
    #Extract measurement data and save it into roi_values$meas
    roi_values$meas <- NULL
    roi_values$alph_start <- NULL
    roi_values$alph_end <- NULL
    roi_values$meas <- extract_meas_data_from_xlsx(excel_file = roi_values$roi_xlsx_file_datapath,
                                                   excel_data_frame = roi_values$data,
                                                   meas_no = input$roi_meas_no,
                                                   alpha_packet_size = input$roi_alpha_packet_size)

    meancpa <- roi_values$meas$meancpa
    alpha_packet_size <- input$roi_alpha_packet_size

    #Mean Counts per Alpha Plot
    # output$plot_CpT_initial <- renderPlot({
    output$roi_plot_CpT <- make_plot_CpT(meancpa = roi_values$meas$meancpa,
                                         meas_no = input$roi_meas_no,
                                         alpha_packet_size = input$roi_alpha_packet_size)
    i <- input$roi_meas_no
    cat("\ni = ", input$roi_meas_no)
    cat("\nFilename:    ", roi_values$data[i,2])
    cat("\nAlpha Start: ", roi_values$data[i,19])
    cat("\nAlpha End:   ", roi_values$data[i,20])
    cat("\nMean C/T: ", roi_values$data[i,21])
    cat("\n****************************************************************************************")

  })









  #---Show Measurement Data in Table------------------------------------------------------------------------------

  #Checks if Show measurement data in Table button has been clicked and then displays it.
  observeEvent(input$roi_button_show_dataTable_meas_info,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_button_load_selected_excel_sheet)
    cat("\n--- Creating Measurement Table ---------------------------------------------------------")


    inFile <- input$roi_datafile
    i <- input$roi_meas_no
    df_matrix1 <- matrix(c(roi_values$data[i,14], #Meas No
                           roi_values$data[i,2] #Filename
    ), byrow = TRUE, nrow=1)
    df1 <- data.frame(df_matrix1)

    colnames(df1) <- c("Meas. No", "Filename")
    #*******************************************
    df_matrix2 <- matrix(c(roi_values$data[i,5],
                           roi_values$data[i,6],
                           roi_values$data[i,7],
                           roi_values$data[i,8],
                           roi_values$data[i,9],
                           roi_values$data[i,10]), byrow = TRUE, nrow=1)
    df2 <- data.frame(df_matrix2)

    colnames(df2) <- c("p [torr]", "HV [-V]", "Drift [V]", "t [ms]", "Amplification", "Threshold [mV]")


    cat("\n****************************************************************************************")
    output$roi_dataTable_meas_info1 <- renderTable({df1})
    output$roi_dataTable_meas_info2 <- renderTable({df2})



  })



  #---Calculate ROI and fits----------------------
  observeEvent(input$roi_curve_shape,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    req(is.na(input$roi_meas_no)==FALSE)

    roi_values$curve_fit_param_A <- NULL
    roi_values$curve_fit_param_B <- NULL
    roi_values$curve_fit_param_C <- NULL

    roi_values$alph_start <- NULL
    roi_values$alph_end <- NULL
    roi_values$additional_comment <- NULL

    roi_values$curve_shape <- 0

    i <- input$roi_meas_no

    output$roi_button_warning_curve_shape <- NULL
    output$roi_warning_curve_shape <- NULL

    if((roi_values$data$`Shape.of.C/T.curve`[i] !=0)|| (is.na(roi_values$data$`Shape.of.C/T.curve`[i]))){
      cat("\nWarning: Curve Shape has already previously been selected.")
      output$roi_warning_curve_shape <- renderText({
        paste("Warning: There has already been a curve shape selected previously for this measurement.")
      })

      output$roi_button_warning_curve_shape <- renderUI({
        actionButton("roi_button_warning_curve_shape_overwrite_excel", label = "Calculate new ROI")
      })

    }else{

      cat("\n--- Calculation of fit and ROI has started ---------------------------------------------")

      if(input$roi_curve_shape =="0 - To be determined"){
        cat("\n No curve shape selected.")

        return(NULL)}



      #---Exp. Decreasing-----------------------------------------------------------------------
      if(input$roi_curve_shape == "1 - Exp. Decreasing"){
        cat("\n Curve shape selected: 1")
        alpha_packet_size <- input$roi_alpha_packet_size


        result_fit1 <- create_fit_exp_decreasing(meancpa = roi_values$meas$meancpa,
                                                 mfpga_bin = roi_values$meas$mfpga_bin,
                                                 margin = input$roi_margin,
                                                 meas_no =  input$roi_meas_no,
                                                 alpha_packet_size = input$roi_alpha_packet_size)
        roi_values$alph_start <- result_fit1$alph_start
        roi_values$alph_end <- result_fit1$alph_end

        roi_values$curve_fit_param_A <- result_fit1$curve_fit_param_A
        roi_values$curve_fit_param_B <- result_fit1$curve_fit_param_B
        roi_values$curve_fit_param_C <- result_fit1$curve_fit_param_C
        roi_values$curve_shape <- 1

        output$roi_plot_CpT <- result_fit1$CpT_plot



        #---Linear-----------------------------------------------------------------------
      }else if(input$roi_curve_shape == "2 - Linear"){
        cat("\n Curve shape selected: 2")

        alpha_packet_size <- input$roi_alpha_packet_size

        result_fit2 <- create_fit_linear(meancpa = roi_values$meas$meancpa,
                                         mfpga_bin = roi_values$meas$mfpga_bin,
                                         margin = input$roi_margin,
                                         meas_no =  input$roi_meas_no,
                                         alpha_packet_size = input$roi_alpha_packet_size)
        roi_values$alph_start <- result_fit2$alph_start
        roi_values$alph_end <- result_fit2$alph_end

        roi_values$curve_fit_param_A <- result_fit2$curve_fit_param_A
        roi_values$curve_fit_param_B <- result_fit2$curve_fit_param_B
        roi_values$curve_fit_param_C <- result_fit2$curve_fit_param_C
        roi_values$curve_shape <- 2

        output$roi_plot_CpT <- result_fit2$CpT_plot



        #---Exp. Increasing-----------------------------------------------------------------------
      }else if(input$roi_curve_shape == "3 - Exp. Increasing"){
        cat("\n Curve shape selected: 3")

        alpha_packet_size <- input$roi_alpha_packet_size

        result_fit3 <- create_fit_exp_increasing(meancpa = roi_values$meas$meancpa,
                                                 mfpga_bin = roi_values$meas$mfpga_bin,
                                                 margin = input$roi_margin,
                                                 meas_no =  input$roi_meas_no,
                                                 alpha_packet_size = input$roi_alpha_packet_size)
        roi_values$alph_start <- result_fit3$alph_start
        roi_values$alph_end <- result_fit3$alph_end

        roi_values$curve_fit_param_A <- result_fit3$curve_fit_param_A
        roi_values$curve_fit_param_B <- result_fit3$curve_fit_param_B
        roi_values$curve_fit_param_C <- result_fit3$curve_fit_param_C
        roi_values$curve_shape <- 3

        output$roi_plot_CpT <- result_fit3$CpT_plot



        #---Other-----------------------------------------------------------------------
      }else if(input$roi_curve_shape == "4 - Other"){
        cat("\n Curve shape selected: 4")

        roi_values$alph_start <- 1
        roi_values$alph_end <- nrow(roi_values$meas$mfpga_bin) #set alpha_end to last alpha

        roi_values$curve_fit_param_A <- 0
        roi_values$curve_fit_param_B <- 0
        roi_values$curve_fit_param_C <- 0
        roi_values$curve_shape <- 4
      }

      #After plotting the selected curve fit, alpha start, alpha end
      #and any additional comments get saved.
      roi_values$additional_comment <- input$roi_meas_description

      i <- input$roi_meas_no
      roi_values$data$Alpha.Start[i] <- as.numeric(roi_values$alph_start)
      roi_values$data$Alpha.End[i] <- as.numeric(roi_values$alph_end)

      roi_values$data$Margin.of.fit[i] <- as.numeric(input$roi_margin)
      roi_values$data$Comments.to.usefulness.of.data[i] <- roi_values$additional_comment


      roi_values$data$Curve.fitting.parameter.A[i] <- roi_values$curve_fit_param_A
      roi_values$data$Curve.fitting.parameter.B[i] <- roi_values$curve_fit_param_B
      roi_values$data$Curve.fitting.parameter.C[i] <- roi_values$curve_fit_param_C
      roi_values$data$`Shape.of.C/T.curve`[i] <- roi_values$curve_shape

      cat("\n--- The ROI calculation has been successful.--------------",
          "\n Meas No.:        ", input$roi_meas_no,
          "\n Alpha Start:     ", roi_values$data$Alpha.Start[i],
          "\n Alpha End:       ", roi_values$data$Alpha.End[i],
          "\n Margin:          ", roi_values$data$Margin.of.fit[i],
          "\n Comment:         ", roi_values$data$Comments.to.usefulness.of.data[i])
    }
  })


  #---Overwrite ROI for previously selected curve shapes-----------
  observeEvent(input$roi_button_warning_curve_shape_overwrite_excel,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_meas_no)
    i <- input$roi_meas_no
    cat("\n--- Resetting ROI to default values ----------------------------------------------------")
    roi_values$data$Alpha.Start[i] <- "TBD"
    roi_values$data$Alpha.End[i] <- "TBD"

    roi_values$data$Margin.of.fit[i] <- NA
    roi_values$data$Comments.to.usefulness.of.data[i] <- NA


    roi_values$data$Curve.fitting.parameter.A[i] <- NA
    roi_values$data$Curve.fitting.parameter.B[i] <- NA
    roi_values$data$Curve.fitting.parameter.C[i] <- NA
    roi_values$data$`Shape.of.C/T.curve`[i] <- 0

    updateTextInput(session, "roi_meas_description", value= "No comment" )
    updateRadioButtons(session, "roi_curve_shape", selected = "0 - To be determined")

    roi_values$meas <- NULL
    roi_values$alph_start <- NULL
    roi_values$alph_end <- NULL

    roi_values$meas <- extract_meas_data_from_xlsx(excel_file = roi_values$roi_xlsx_file_datapath,
                                                   excel_data_frame = roi_values$data,
                                                   meas_no = input$roi_meas_no,
                                                   alpha_packet_size = input$roi_alpha_packet_size)

    meancpa <- roi_values$meas$meancpa
    alpha_packet_size <- input$roi_alpha_packet_size

    #Mean Counts per Alpha Plot
    output$roi_plot_CpT <- make_plot_CpT(meancpa = roi_values$meas$meancpa,
                                         meas_no = input$roi_meas_no,
                                         alpha_packet_size = input$roi_alpha_packet_size)



    output$roi_dataTable_meas_info1 <- renderTable({NULL})
    output$roi_dataTable_meas_info2 <- renderTable({NULL})
    output$roi_button_warning_curve_shape <- NULL
    output$roi_warning_curve_shape <- NULL

    cat("\n--- The ROI has been resetted to default values.--------------",
        "\n Meas No.:        ", input$roi_meas_no,
        "\n Alpha Start:     ", roi_values$data$Alpha.Start[i],
        "\n Alpha End:       ", roi_values$data$Alpha.End[i],
        "\n Margin:          ", roi_values$data$Margin.of.fit[i],
        "\n Comment:         ", roi_values$data$Comments.to.usefulness.of.data[i])

  })

  #---update add. comments field------------------
  observeEvent(input$roi_button_update_additional_comments,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    req(is.na(input$roi_meas_no)==FALSE)
    cat("\n--- Additional comments updated and added to dataframe----------------------------------")

    i = input$roi_meas_no

    roi_values$additional_comment <- input$roi_meas_description
    roi_values$data$Comments.to.usefulness.of.data[i] <- roi_values$additional_comment

    cat("\n The ROI calculation has been updated",
        "\n Meas No.:        ", input$roi_meas_no,
        "\n Alpha Start:     ", roi_values$data$Alpha.Start[i],
        "\n Alpha End:       ", roi_values$data$Alpha.End[i],
        "\n Margin:          ", roi_values$data$Margin.of.fit[i],
        "\n Comment:         ", roi_values$data$Comments.to.usefulness.of.data[i])
  })


  #---Continue with next measurement------------------------------------------------------------
  observeEvent(input$roi_button_next_meas,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    req(is.na(input$roi_meas_no)==FALSE)
    cat("\n---- Next measurement-------------------------------------------------------------------")


    #Increase Meas No by 1
    x <- reactive({
      incr_meas_no <- input$roi_meas_no +1
      return(incr_meas_no)})

    cat("\nMeasurement No has been updated from ", input$roi_meas_no, "to", x())
    updateNumericInput(session, "roi_meas_no", value = x())

  })
  observeEvent(input$roi_meas_no,{
    req(input$roi_button_load_sheet_options)
    req(is.na(input$roi_meas_no)==FALSE)
    cat("\n--- Meas. No has been changed. Reloading CpT plot --------------------------------------")
    cat("\n Measurement No: ", input$roi_meas_no)
    updateTextInput(session, "roi_meas_description", value= "No comment" )
    updateRadioButtons(session, "roi_curve_shape", selected = "0 - To be determined")

    req(input$roi_button_load_sheet_options)
    req(input$roi_button_load_selected_excel_sheet)
    cat("\n --- Starting plotting of measurement data ---------------------------------------------")
    roi_values$meas <- NULL
    roi_values$alph_start <- NULL
    roi_values$alph_end <- NULL
    roi_values$meas <- extract_meas_data_from_xlsx(excel_file = roi_values$roi_xlsx_file_datapath,
                                                   excel_data_frame = roi_values$data,
                                                   meas_no = input$roi_meas_no,
                                                   alpha_packet_size = input$roi_alpha_packet_size)

    meancpa <- roi_values$meas$meancpa
    alpha_packet_size <- input$roi_alpha_packet_size


    #Mean Counts per Alpha Plot
    output$roi_plot_CpT <- make_plot_CpT(meancpa = roi_values$meas$meancpa,
                                         meas_no = input$roi_meas_no,
                                         alpha_packet_size = input$roi_alpha_packet_size)


    output$roi_dataTable_meas_info1 <- renderTable({NULL})
    output$roi_dataTable_meas_info2 <- renderTable({NULL})
    cat("\n****************************************************************************************")

  })

  #---Save back to Excel -------------------------------------------------------------
  observeEvent(input$roi_button_save_back_to_excel,{
    req(input$roi_button_load_sheet_options)
    req(input$roi_excel_sheet)
    output$warning_save_excel_file <- renderText({
      NULL
    })

    cat("\n--- Saving Measurement Data from Sheet", input$roi_excel_sheet, "back to Excel -------------------------------------\n")
    tryCatch(
      expr = {
        save_dataFrame_back_to_xlsx(excel_file = roi_values$roi_xlsx_file_datapath,
                                    excel_sheet_name = input$roi_excel_sheet,
                                    data = roi_values$data)
      },
      error = function(e){
        message("Error: Unable to save dataframe back to Excel.")
        print(e)
      },
      warning = function(w){
        message("Warning: Unable to save dataframe back to Excel. Check if Excel File is open in Background!")
        output$warning_save_excel_file <- renderText({
          paste("Warning: Excel file was not saved. Check whether you closed the file.")
        })
        print(w)
      },
      finally = {
        #message('All done, quitting.')
      }
    )

  })








  #---Info Button Meas Info-------------------
  observeEvent(input$roi_info_meas_no, {
    showModal(modalDialog(
      title = "Measurement number",
      "This parameter corresponds to the measurement number the user
      whishes to analyze. It is given by the excel row of the measurement.
      The header ist not counted, hence the second row corresponds to measurement 1.

      The button 'Show Measurement Information' allows to double check the correct input
      by showing the file name, as well as its measurement number and other parameters."
    ))
  })








})
