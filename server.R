shinyServer(function(input, output, session) {
  GetRawdata <- reactive({    # Read imported data
    if (is.null(input$fileUploaded)) {
      return(data.frame())     # User has not uploaded a file yet
    }
    
    raw.dataframe <- read.csv(input$fileUploaded$datapath,
                              na.strings = c("", " "),
                              header = TRUE)
    return(raw.dataframe)
  })
  GetDataset <- reactive({   # Create subset of raw data & update column type[numerical, factor, etc.]
    if (is.null(input$selectVariable)) {
      return(data.frame())
    }
    dataset <- GetRawdata()[input$selectVariable]
    dataset %<>% na.omit()
    dataset[, input$selectColfactor] %<>% lapply(as.factor)
    dataset[, input$selectColnumeric] %<>% lapply(as.numeric)
    return(dataset)
    
  })
  GetDatatarget <- reactive({  # Insert column named target showing 1 for targeted population
    if (is.null(input$selectVariabletarget)) {
      return(data.frame())
    }
    dataset <- GetDataset()
    targetcol <- NULL
    for (var in input$selectVariabletarget) {
      if (is.numeric(dataset[[var]])) {
        if (!is.null(input[[paste0("filter.option", var)]])) {
          if (input[[paste0("filter.option", var)]] == "Percentage") {
            deciles <- dataset[[var]] %>%
              quantile(prob = seq(0, 1, length = 11), 
                       type = 5,
                       na.rm=TRUE)
            dec_start <- (input[[paste0("filter.value", var)]][1] / 10) + 1
            dec_stop <- (input[[paste0("filter.value", var)]][2] / 10) + 1
            target <- dataset[[var]] >= deciles[[dec_start]] & 
              dataset[[var]] <= deciles[[dec_stop]]
          } else {
            target <- dataset[[var]] >= input[[paste0("filter.value", var)]][1] & 
              dataset[[var]] <= input[[paste0("filter.value", var)]][2]
          }
        }
      } else {
        target <- dataset[[var]] %in% input[[paste0("filter.value", var)]]
      }
      if (is.null(targetcol)) {     # Merge target into one common vector
        targetcol <- target
      } else {
        targetcol <- targetcol & target
      }
    }
    targetcol %<>% as.numeric()     # Switch TRUE/FALSE to 1/0
    dataset$target <- targetcol
    return(dataset)
  })
  GetFit <- reactive({
    dataset <- GetDatatarget()
    dataset[input$selectVariabletarget] <- NULL   # Delete target variables
    fit.dataset <- rpart(target~.,
                         data = dataset,
                         method = "class")
    return(fit.dataset)
  })
  
  observe({
    output$raw.data <- renderDataTable({               # Render raw data
      GetRawdata() %>%
        datatable(options = list(scrollX = TRUE))
    })
    if (input$variableManualSelect == TRUE) {
      updateSelectInput(                                 # Update selection of variables
        session,
        "selectVariable",
        choices = names(GetRawdata()),
        selected = c("LINE_OF_BUSINESS", "SOURCE_SYSTEM","Last_Service_Type","First_Service_Type","CLAIM_STATUS","Inc_Age_Grp",
                     "Incurral_Year","DIAG_HIGH_LVL_GRP","INCURRAL_AGE","POLICY_FORM","Gender","LOS_mnths","Claim_paid_since_inception",
                     "Avg_Pd_mo","length_of_clm_mnt","RISK_MGMT_CD","POLICY_ISSUE_STATE","Attained_age_grouping","INCURRAL_AGE_grouping")
      )
    } else {
      updateSelectInput(                                 # Select all variables
        session,
        "selectVariable",
        choices = names(GetRawdata()),
        selected = names(GetRawdata())
      )
    }
  })
  
  observe({
    updateSelectInput(                 # Update selection of categorical variables
      session,
      inputId = "selectColfactor",
      choices = input$selectVariable,
      selected = c("LINE_OF_BUSINESS",
                   "SOURCE_SYSTEM",
                   "Last_Service_Type",
                   "First_Service_Type",
                   "CLAIM_NUM",
                   "CLAIM_NUM2",
                   "CLAIM_STATUS",
                   "DIAG_HIGH_LVL_GRP",
                   "POLICY_FORM",
                   "Gender",
                   "RISK_MGMT_CD",
                   "POLICY_ISSUE_STATE",
                   "Attained_age_grouping",
                   "INCURRAL_AGE_grouping",
                   "Incurral_Year")
    )
    updateSelectInput(                 # Update selection of numerical variables
      session,
      inputId = "selectColnumeric",
      choices = input$selectVariable,
      selected = c("Inc_Age_Grp",
                   "INCURRAL_AGE",
                   "LOS_mnths",
                   "Claim_paid_since_inception",
                   "Avg_Pd_mo",
                   "length_of_clm_mnt")
    )
  })
  
  observe({
    output$raw.data.subset <- renderDataTable({      # Render subset of raw data
      GetDataset() %>% 
        datatable(options = list(scrollX = TRUE))
    })
    updateSelectInput(                              # Update selection of target variable
      session,
      "selectVariabletarget",
      choices = names(GetDataset()),
      selected = c("Avg_Pd_mo")
    )
  })
  
  observe({    # Render input objects filter.option* & filter.value*
    count.variable <- input$selectVariabletarget %>% length()
    # Cannot combine following two inputs due to limitation of renderUI
    output$filter.option <- renderUI({        # Create a selection of percentile or value if column is numerical
      lapply(1:count.variable, function(i) {
        if (is.numeric(GetDataset()[[input$selectVariabletarget[i]]])) {
          selectInput(
            inputId = paste0("filter.option", input$selectVariabletarget[i]),
            label = paste("Filter option for ", input$selectVariabletarget[i]),
            choices = c("Percentage" ="Percentage", 
                        "Numeric" = "Numeric"),
            multiple = FALSE,
            selected = "Percentage"
          )
        } else {}
      })
    })
    output$clustercondition <- renderUI({   # Create value selection input objects for selected variables
      lapply(1:count.variable, function(i) {  
        if (is.numeric(GetDataset()[[input$selectVariabletarget[i]]])) {   # If variable is numeric
          sliderInput(
            inputId = paste0("filter.value", input$selectVariabletarget[i]),
            label = paste0("Identify percentile for ", input$selectVariabletarget[i]),
            min = 0,
            max = 100,
            value = c(90, 100),
            step = 10
          )
        }
        else if (is.factor(GetDataset()[[input$selectVariabletarget[i]]])) {  # If variable is categorical
          selectInput(
            inputId = paste0("filter.value", input$selectVariabletarget[i]),
            label = paste0("Select target values for ", input$selectVariabletarget[i]),
            choices = GetDataset()[[input$selectVariabletarget[i]]] %>% unique(na.rm = TRUE),
            selected = GetDataset()[[input$selectVariabletarget[i]]] %>% unique(na.rm = TRUE),
            multiple = TRUE
          )
        }
        else {}
      })
    })
  })
  
  observe({    # Update sliderinput value based on selection between percentile & absolute value
    variable.numeric <- input$selectVariabletarget[input$selectVariabletarget %in% input$selectColnumeric]
    if (length(variable.numeric > 0)) {
      for (var in variable.numeric) {  # If filtered by percentage
        if(!is.null(input[[paste0("filter.option", var)]])) {
          if (input[[paste0("filter.option", var)]] == "Percentage") {
            updateSliderInput(
              session,
              inputId = paste0("filter.value", var),
              label = paste0("Identify percentile for ", var),
              min = 0,
              max = 100,
              value = c(90, 100),
              step = 10
            )
          } else {
            updateSliderInput(        # If filtered by absolute value
              session,
              inputId = paste0("filter.value", var),
              label = paste0("Identify data range for ", var),
              min = GetDataset()[[var]] %>% min(na.rm = TRUE),
              max = GetDataset()[[var]] %>% max(na.rm = TRUE),
              value = c(GetDataset()[[var]] %>% min(na.rm = TRUE), 
                        GetDataset()[[var]] %>% max(na.rm = TRUE)
              ),
              step = 1
            )
          }
        }
      }
    }
  })
  
  observe({
    output$value.target.freq <- renderValueBox({        # Count the frequency of the target
      valueBox(
        value = sum(GetDatatarget()$target == 1, na.rm = TRUE),
        subtitle = "Records Selected",
        icon = icon("filter"),
        color = "yellow"
      )
    })
    output$value.total.freq <- renderValueBox({        # Count total number of observations
      valueBox(
        value = GetDatatarget() %>% nrow(),
        subtitle = "Records in Total",
        icon = icon("saved"),
        color = "green"
      )
    })
  })
  
  observe({
    vector.numeric.variable <- input$selectVariabletarget[input$selectVariabletarget %in% input$selectColnumeric]
    if (length(vector.numeric.variable) > 0) {
      count.numeric.variable <- length(vector.numeric.variable)
      output$plot.target <- renderUI({
        target.scatter.list <- lapply(1:count.numeric.variable, function(i) {
          plotname <- paste0("target.scatter", i)
          plotlyOutput(plotname)
        })
        do.call(tagList, target.scatter.list)
      })
      for (i in 1:count.numeric.variable) {
        local({
          my.i <- i
          plotname <- paste0("target.scatter", my.i)
          output[[plotname]] <- renderPlotly({
            plot_ly(x = GetDatatarget()[[vector.numeric.variable[my.i]]], 
                    marker = list(color = GetDatatarget()$target), 
                    type = "scatter", 
                    mode = "markers")
          })
        })
      }
    }
  })
  
  observe({
    output$tree_plot <- renderPlot({     # Plot Desicion Tree
      # Plot colorful tree
      GetFit() %>% prp(cex=.7, extra=104, box.col=3, varlen=0)
      #Show nicer tree
      # prp(fit_av4_v2,tweak=1)
      # fancyRpartPlot(fit_av4_v2)
      
      #show text as rules for each cluster
      #rule<-asRules(fit_av4_v2)
      #t<-data.frame(fit_av4_v2$variable.importance)
      # colnames(t)[1]<-"Importance of Variable"
      # t
      #ggplot(t, aes(x = fit_av4_v2$variable.importance)) + geom_dotplot()
    })
  })
  
  
  observe({
    output$tree_var_imp <-renderPlotly({      # Show variable importance as horizontal barplot
      variable.importance <- GetFit()$variable.importance %>% data.frame()
      colnames(variable.importance)[1] <- "importance"
      variable.importance$variable <- variable.importance %>% rownames()
      rownames(variable.importance) <- NULL
      plot_ly(variable.importance,
              x = ~importance,
              y = ~reorder(variable,importance),
              type = "bar",
              orientation = "h",
              marker = list(color = '#6CC18E',
                            line = list(color = '#32AB60', width = 1))) %>%
        layout(margin = list(l = 200),
               xaxis = list(title = "Variable Importance"),
               yaxis = list(title = ""))
    })
  })

})

