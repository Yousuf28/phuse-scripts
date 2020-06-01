library(shiny)
library(ggplot2)
library(stringr)
library(htmltools)
library(shinydashboard)

library(tidyverse)
library(ggstance)
library(ggrepel)
library(RColorBrewer)
library(patchwork)
library(ggh4x)
library(DT)
library(plotly)
library(officer)
library(flextable)
library(magrittr)
# Bugs ####

# Project Improvement Ideas:
# - Add legend to figure that lists dose compared and PK/HED option
# - Allow user to create display names of findings with legend at bottom
# - Add option to display margin on top of figure
# - Make an optional figure legend (with checkbox)
# - Color "errorbar" to indicate severity (white for no toxicity at dose)
#   Color by the lowest dose on the ladder and switch color half-way between dose edges if space allows
#     on the UI bar side, change checkboxes to selectInputs to indicate dose severity
# - For table export, generate the three tables from the smart template in Word format
# - Add footnotes tied to findings (numbered) as well as a general footnote
# - Start with Smart Template as default table layout
# - Allow table to be flexibly modified
# - Brackets for findings
# - Text wrap finding names so that they don't overlap and use bullets to denote findings
# - Stagger doses (down -> up) so they don't overlap when close
# - use error bar to combine findings across doses

## added by Yousuf

### need to add or change in 3rd table of template
# correct the HED calculation
# add starting Dose and MHRD 
# add 


'%ni%' <- Negate('%in%')

# # Save configuration of blankData.rds below for later: ####

# Data <- list(
#   INDnumber = NULL,
#   'Clinical Information'= list(
#     HumanWeight = 60,
#     MgKg = F,
#     'Start Dose' = list(
#       StartDose = NULL,
#       StartDoseMgKg = NULL,
#       StartDoseCmax = NULL,
#       StartDoseAUC = NULL
#     ),
#     'MRHD' = list(
#       MRHDDose = NULL,
#       MRHDDoseMgKg = NULL,
#       MRHDCmax = NULL,
#       MRHDAUC = NULL
#     ),
#     'Custom Dose' = list(
#       CustomDose = NULL,
#       CustomDoseMgKg = NULL,
#       CustomDoseCmax = NULL,
#       CustomDoseAUC = NULL
#     )
#   ),
#   'Nonclinical Information' = list(
#     'New Study' = list(
#       Species = NULL,
#       Duration = NULL,
#       Doses = list(
#         Dose = NULL,
#         NOAEL = F,
#         Cmax = NULL,
#         AUC = NULL
#       ),
#       Findings = list(
#         Finding = NULL,
#         Reversibility = F,
#         FindingDoses = NULL
#       )
#     ),
#     'Rat Study' = list(
  #       Species = NULL,
  #       Duration = NULL,
  #       Doses = list(
  #         Dose = NULL,
  #         NOAEL = F,
  #         Cmax = NULL,
  #         AUC = NULL
  #       ),
  #       Findings = list(
  #         Finding = NULL,
  #         Reversibility = F,
  #         FindingDoses = NULL
  #       )
  #     )
#     'Dog Study' = list(
#       Species = NULL,
#       Duration = NULL,
#       Doses = list(
#         Dose = NULL,
#         NOAEL = F,
#         Cmax = NULL,
#         AUC = NULL
#       ),
#       Findings = list(
#         Finding = NULL,
#         Reversibility = F,
#         FindingDoses = NULL
#       )
#     )
#   )
# )
# 
# saveRDS(Data,'blankData.rds')

addUIDep <- function(x) {
  jqueryUIDep <- htmlDependency("jqueryui", "1.10.4", c(href="shared/jqueryui/1.10.4"),
                                script = "jquery-ui.min.js",
                                stylesheet = "jquery-ui.min.css")
  
  attachDependencies(x, c(htmlDependencies(x), list(jqueryUIDep)))
}


values <- reactiveValues()
values$Application <- NULL
values$SM <- NULL
values$selectData <- NULL

# Species Conversion ----

speciesConversion <- c(6.2,1.8,3.1,3.1)
names(speciesConversion) <- c('Rat','Dog','Monkey','Rabbit')

clinDosingOptions <- c('Start Dose','MRHD','Custom Dose')

# Server function started here (selectData) ----

server <- function(input,output,session) {

  output$selectData <- renderUI({
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Program'
    if (is.null(values$selectData)) {
      selectInput('selectData','Select Program:',datasets,selected='blankData.rds')
    } else {
      selectInput('selectData','Select Program:',datasets,selected=values$selectData)
    }
  })
  
  output$studyName <- renderUI({
    req(input$selectData)
    if (input$selectData!='blankData.rds') {
      HTML(paste(
        p(HTML(paste0('<h4>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u>Selected Study</u></h4><h4>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;',
                      (basename(unlist(strsplit(input$selectData,'.rds')))),'</h4>')
        ))
      ))
    }
  })
  
# getData ------
  
  getData <- reactive({
    input$refreshPlot
    req(input$selectData)
    input$selectStudy
    Data <- readRDS(input$selectData)
  })
  
  observe({
    req(input$selectData)
    if (input$selectData == 'blankData.rds') {
      values$Application <- paste0('Applications/',input$newApplication,'.rds')
    } else {
      values$Application <- input$selectData
    }
  })
  
  observeEvent(input$saveData,{
    Data <- getData()
    saveRDS(Data,values$Application)
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Program'
    selectInput('selectData','Select Program:',datasets)
    updateSelectInput(session,'selectData',choices=datasets,selected=values$Application)
  })
  
  observeEvent(input$deleteData,{
    file.remove(values$Application)
    datasets <- c('blankData.rds',grep('.rds',list.files('Applications/',full.names = T),value=T))
    names(datasets) <- basename(unlist(strsplit(datasets,'.rds')))
    names(datasets)[which(datasets=='blankData.rds')] <- 'New Program'
    selectInput('selectData','Select Program:',datasets)
    updateSelectInput(session,'selectData',choices=datasets,selected='blankData.rds')
  })
  
  output$selectStudy <- renderUI({
    req(input$selectData)
    input$selectData
    isolate(Data <- getData())
    studyList <- names(Data[['Nonclinical Information']])
    selectInput('selectStudy','Select Study:',choices=studyList)
  })
  
  # Clinical information -----
  
  observeEvent(input$selectData,ignoreNULL = T,{
    Data <- getData()
    clinData <- Data[['Clinical Information']]
    if (clinData$MgKg==F) {
      updateNumericInput(session,'HumanWeight',value = clinData$HumanWeight)
    }
    clinDosing <- NULL
    for (dose in clinDosingOptions) {
      if (!is.null(clinData[[dose]][[gsub(' ','',dose)]])) {
        clinDosing <- c(clinDosing,dose)
      }
    }
    updateCheckboxGroupInput(session,'clinDosing',selected=clinDosing)
    for (dose in clinDosing) {
      doseName <- gsub(' ','',dose)
      if (clinData$MgKg==F) {
        updateNumericInput(session,doseName,value = clinData[[dose]][[doseName]])
      } else {
        updateNumericInput(session,paste0(doseName,'MgKg'),value = clinData[[dose]][[paste0(doseName,'MgKg')]])
      }
      updateNumericInput(session,paste0(doseName,'Cmax'),value = clinData[[dose]][[paste0(doseName,'Cmax')]])
      updateNumericInput(session,paste0(doseName,'AUC'),value = clinData[[dose]][[paste0(doseName,'AUC')]])
    }
  })
  
# Nonclinical data update ------
  
  observeEvent(input$selectStudy,ignoreNULL = T,{
    Data <- getData()
    studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
    updateSelectInput(session,'Species',selected=studyData$Species)
    updateTextInput(session,'Duration',value=studyData$Duration)
    updateNumericInput(session,'nDoses',value=studyData$nDoses)
    updateNumericInput(session,'nFindings',value=studyData$nFindings)
    
  })
  
  observeEvent(eventExpr = input$saveStudy, {
    doseList <- as.list(seq(input$nDoses))
    names(doseList) <- paste0('Dose',seq(input$nDoses))
    for (i in seq(input$nDoses)) {
      doseList[[i]] <- list(Dose=input[[paste0('dose',i)]],
                            NOAEL = input[[paste0('NOAEL',i)]],
                            Cmax = input[[paste0('Cmax',i)]],
                            AUC = input[[paste0('AUC',i)]]
      )
    }
    
    findingList <- as.list(seq(input$nFindings))
    names(findingList) <- paste0('Finding',seq(input$nFindings))
    if (input$nFindings > 0) {
      for (i in seq(input$nFindings)) {
        severity <- list()
        for (j in seq(input$nDoses)) {
        severity[[paste0("Dose", j)]] <- input[[paste0("Severity", i, "_", j)]]
        }
        findingList[[i]] <- list(Finding=input[[paste0('Finding',i)]],
                                 Reversibility = input[[paste0('Reversibility',i)]],
                                 # FindingDoses = input[[paste0('FindingDoses',i)]],
                                 Severity = severity
        )
      }
    } else {
      findingList[[1]] <- NULL
    }
    
    # Severity data update -----
    
    
    
    # studyName and data -----
    
    Data <- getData()
    studyName <- paste(input$Species,input$Duration,sep=': ')
    Data[['Nonclinical Information']][[studyName]] <- list(
      Species = input$Species,
      Duration = input$Duration,
      nDoses = input$nDoses,
      Doses = doseList,
      nFindings = input$nFindings,
      Findings = findingList
    )
    
    saveRDS(Data,values$Application)
    
    studyList <- names(Data[['Nonclinical Information']])
    updateSelectInput(session,'selectStudy',choices=studyList,selected=studyName)
    input$refreshPlot
  })
  

  observeEvent(input$saveClinicalInfo, {
    Data <- getData()
    clinData <- Data[['Clinical Information']]
    if (input$MgKg==F) {
      clinData[['HumanWeight']] <- input$HumanWeight
    } else {
      clinData[['HumanWeight']] <- NULL
    }
    clinData[['MgKg']] <- input$MgKg
    if (length(input$clinDosing)>0) {
      for (clinDose in input$clinDosing) {
        clinDoseName <- gsub(' ','',clinDose)
        if (input$MgKg==F) {
          clinData[[clinDose]][[clinDoseName]] <- input[[clinDoseName]]
        } else {
          clinData[[clinDose]][[paste0(clinDoseName,'MgKg')]] <- input[[paste0(clinDoseName,'MgKg')]]
        }
        clinData[[clinDose]][[paste0(clinDoseName,'Cmax')]] <- input[[paste0(clinDoseName,'Cmax')]]
        clinData[[clinDose]][[paste0(clinDoseName,'AUC')]] <- input[[paste0(clinDoseName,'AUC')]]
      }
    }
    Data[['Clinical Information']] <- clinData
    saveRDS(Data,values$Application)
  })
  
  
  observeEvent(input$deleteStudy,{
    Data <- getData()
    studyIndex <- which(names(Data[['Nonclinical Information']])==input$selectStudy)
    restIndex <- seq(length(names(Data[['Nonclinical Information']])))[-studyIndex]
    restNames <- names(Data[['Nonclinical Information']])[restIndex]
    Data[['Nonclinical Information']] <- Data[['Nonclinical Information']][restNames]
    saveRDS(Data,values$Application)
    studyList <- names(Data[['Nonclinical Information']])
    updateSelectInput(session,'selectStudy',choices=studyList,selected='New Study')
  })
  
  output$studyTitle <- renderText({
    paste(input$Species,input$Duration,sep=': ')
  })
  
  output$displayStudies <- renderUI({
    req(input$clinDosing)
    input$selectData
    input$selectStudy
    isolate(Data <- getData())
    studyList <- names(Data[['Nonclinical Information']])
    studyList <- studyList[-which(studyList=='New Study')]
    addUIDep(selectizeInput('displayStudies',label='Select Studies to Display:',choices=studyList,
                            selected=studyList,
                            multiple=TRUE,width='100%',options=list(plugins=list('drag_drop','remove_button'))))
  })
  
  ## output$Doses -----
  
  output$Doses <- renderUI({
    req(input$selectStudy)
    if (input$selectStudy=='New Study') {
      lapply(1:(4*input$nDoses), function(i) {
        I <- ceiling(i/4)
        if (i %% 4 == 1) {
          numericInput(paste0('dose',I),paste0('Dose ',I,' (mg/kg/day):'),NULL)
        } else if (i %% 4 == 2) {
          checkboxInput(paste0('NOAEL',I),'NOAEL?',value=F)
        }
        else if (i %% 4 == 3) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('Cmax',I),paste0('Dose ',I,' Cmax (ng/mL):'),NULL))
        } else {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('AUC',I),paste0('Dose ',I,' AUC (ng*h/mL):'),NULL))
        }
      })
    } else {
      Data <- getData()
      studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
      lapply(1:(4*input$nDoses), function(i) {
        I <- ceiling(i/4)
        doseName <- names(studyData$Doses)[I]
        if (i %% 4 == 1) {
          textInput(paste0('dose',I),paste0('Dose ',I,' (mg/kg/day):'),studyData$Doses[[doseName]][['Dose']])
        } else if (i %% 4 == 2) {
          checkboxInput(paste0('NOAEL',I),'NOAEL?',value=studyData$Doses[[doseName]][['NOAEL']])
        }
        else if (i %% 4 == 3) {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('Cmax',I),paste0('Dose ',I,' Cmax (ng/mL):'),studyData$Doses[[doseName]][['Cmax']]))
        } else {
          div(style="display: inline-block;vertical-align:top; width: 115px;",
              numericInput(paste0('AUC',I),paste0('Dose ',I,' AUC (ng*h/mL):'),studyData$Doses[[doseName]][['AUC']]))
        }
      })
    }
  })
  
  # findings with severity -----
  
  output$Findings <- renderUI({
    req(input$selectStudy)
    if (input$selectStudy=='New Study') {
      if (input$nFindings>0) {
        numerator <- 2 + input$nDoses
        lapply(1:(numerator*input$nFindings), function(i) {
          I <- ceiling(i/numerator)
          if (i %% numerator == 1) {
            textInput(paste0('Finding',I),paste0('Finding ',I,':'))
          } else if (i %% numerator == 2) {
            radioButtons(paste0('Reversibility',I),'Reversibility:',
                         choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
                                       'Partially Reversible [PR]','Not Assessed'),
                         choiceValues=c('[Rev]','[NR]','[PR]',''))
          } else {
            lapply(1:input$nDoses, function(j) {
              if ((i %% numerator == 2+j)|((i %% numerator == 0)&(j==input$nDoses))) {
                selectInput(inputId = paste0('Severity',I,'_',j),label = paste0('Select Severity at Dose ',j,' (',input[[paste0('dose',j)]],' mg/kg/day)'),
                            choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
              }
            })
            
            
          }
            # } else if (i %% numerator == 4) {
          #   selectInput(inputId = paste0('Severity',I,'_2'),label = paste0('Select Severity at Dose ',I),
          #               choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
          # } else {
          #   selectInput(inputId = paste0('Severity',I,'_3'),label = paste0('Select Severity at Dose ',I),
          #               choices = c('Absent','Present','Minimal','Mild','Moderate','Marked','Severe'))
          # }
          
          
          
          # else {
          #   doseLevels <- NULL
          #   for (i in seq(input$nDoses)) {
          #     if (i %% numerator == 2+i)
          #     doseLevel <- input[[paste0('dose',i)]]
          #     if (is.null(doseLevel)) {
          #       doseLevels[i] <- ''
          #     } else {
          #       doseLevels[i] <- doseLevel
          #     }
          #   }
            # checkboxGroupInput(paste0('FindingDoses',I),'Dose Levels:',
            #                    choiceNames = paste(doseLevels,'mg/kg/day'),
            #                    choiceValues = doseLevels,
            #                    selected = NULL)
          # }
        })
      }
    } else {
      Data <- getData()
      studyData <- Data[['Nonclinical Information']][[input$selectStudy]]
      if (input$nFindings>0) {
        numerator <- 2 + input$nDoses
        lapply(1:(3*input$nFindings), function(i) {
          I <- ceiling(i/numerator)
          if (i %% numerator == 1) {
            textInput(paste0('Finding',I),paste0('Finding ',I,':'),
                      studyData$Findings[[paste0('Finding',I)]]$Finding)
          } else if (i %% numerator == 2) {
            radioButtons(paste0('Reversibility',I),'Reversibility:',
                         choiceNames=c('Reversible [Rev]','Not Reversible [NR]',
                                       'Partially Reversible [PR]','Not Assessed'),
                         choiceValues=c('[Rev]','[NR]','[PR]',''),
                         selected=studyData$Findings[[paste0('Finding',I)]]$Reversibility)
          } else {
            for (j in seq(input$nDoses)) {
              print(i%%numerator)
              print(j+2)
              if ((i %% numerator == 2+j)|(i%%numerator==0)) {
                print('worked!')
                selectInput(paste0('Severity',I,'_',j),paste0('Select Severity at Dose ',I),
                            choices = c('Absent','Present','Minimal',
                                        'Mild','Moderate','Marked','Severe'))
                break
              }
            }
          }
         
               # } else {
          #   doseLevels <- NULL
          #   for (i in seq(input$nDoses)) {
          #     doseLevel <- input[[paste0('dose',i)]]
          #     if (is.null(doseLevel)) {
          #       doseLevels[i] <- ''
          #     } else {
          #       doseLevels[i] <- doseLevel
          #     }
          #   }
          #   checkboxGroupInput(paste0('FindingDoses',I),'Dose Levels:',
          #                      choiceNames = paste(doseLevels,'mg/kg/day'),
          #                      choiceValues = doseLevels,
          #                      selected = studyData$Findings[[paste0('Finding',I)]]$FindingDoses)
          # }
        })
      }
    }
  })
  
  # Create PlotData (changed) -----
  
  print("510")
  
 getPlotData <- reactive({
  Data <- getData()
  plotData <- data.frame(matrix(ncol = 14 ))
  column_names <- c("Study", "Species", "Months", "Dose", 
                    "NOAEL", "Cmax", "AUC", "Findings",
                    "Reversibility", "Severity", "Value", "Value_order", "SM", "HED_value")
  colnames(plotData) <- column_names
  
  count <- 1
  
  for (Study in names(Data[["Nonclinical Information"]])) {
    if (Study != "New Study") {
      studyData <- Data[["Nonclinical Information"]][[Study]]
      
      for (i in seq(studyData$nFindings)){
        for (j in seq(studyData$nDoses)){
          
          plotData[count, "Study"] <- Study
          plotData[count, "Species"] <- studyData[["Species"]]
          plotData[count, "Months"] <- studyData[["Duration"]]
          plotData[count, "Dose"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Dose"]]
          plotData[count, "NOAEL"] <- studyData[["Doses"]][[paste0("Dose",j)]][["NOAEL"]]
          plotData[count, "Cmax"] <- studyData[["Doses"]][[paste0("Dose", j)]][["Cmax"]]
          plotData[count, "AUC"] <- studyData[["Doses"]][[paste0("Dose", j)]][["AUC"]]
          plotData[count, "Findings"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Finding"]]
          plotData[count, "Reversibility"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Reversibility"]]
          plotData[count, "Severity"] <- studyData[["Findings"]][[paste0("Finding", i)]][["Severity"]][[paste0("Dose", j)]]
          plotData[count, "Value"] <- 1
          plotData[count, "Value_order"] <- j
          plotData[count, "SM"] <- NA
          plotData[count, "HED_value"] <- NA
          count <- count+1
          
          
        }
      }
    }
  }
  
  plotData$Rev <- gsub("\\[|\\]", "", plotData$Reversibility)
  plotData$finding_rev <- paste0(plotData$Findings,"_", plotData$Rev)
  plotData$find_rev_b <- paste0(plotData$Findings, plotData$Reversibility)
  plotData <- plotData[which(plotData$Study %in% input$displayStudies),]
  return(plotData)
  
})
  
print("559")
  
  output$humanDosing <- renderUI({
    req(input$clinDosing)
    Data <- getData()
    clinDosingNames <- input$clinDosing
    names(clinDosingNames) <- clinDosingNames
    if (length(clinDosingNames)>0) {
      for (clinDose in input$clinDosing) {
        if (Data[['Clinical Information']][['MgKg']]==F) {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,
                                                                             ': (',Data[['Clinical Information']][[clinDose]][[paste0(unlist(strsplit(clinDose,' ')),
                                                                                                                                      collapse='')]],' mg)')
        } else {
          names(clinDosingNames)[which(clinDosingNames==clinDose)] <- paste0(clinDose,': (',Data[['Clinical Information']][[clinDose]][[paste0(unlist(strsplit(clinDose,' ')),'MgKg',collapse='')]],' mg/kg)')
        }
      }
    }
    selectInput('humanDosing','Select Human Dose:',choices=clinDosingNames)
  })
# ## calculate safety margin (SM) ------
#
  calculateSM <- reactive({
    Data <- getData()
    plotData <- getPlotData()
    # HED_value <- NULL
    # SM <- NULL
    if (nrow(plotData)>0) {
      for (i in seq(nrow(plotData))) {
        if (input$SMbasis=='HED') {
          Dose <- as.numeric(plotData[i,'Dose'])
        } else if (input$SMbasis=='Cmax') {
          Dose <- as.numeric(plotData[i,'Cmax'])
        } else if (input$SMbasis=='AUC') {
          Dose <- as.numeric(plotData[i,'AUC'])
        }
        Species <- unlist(strsplit(plotData[i,'Study'],':'))[1]
        humanDoseName <- gsub(' ','',input$humanDosing)
        # humanDose <- input[[humanDoseName]]
        if (input$SMbasis=='HED') {
          humanDose <- Data[['Clinical Information']][[input$humanDosing]][[humanDoseName]]
          HED <- Dose/speciesConversion[[Species]]
          if (input$MgKg==F) {
            HED <- HED*Data[['Clinical Information']][['HumanWeight']]
          }
        } else {
          humanDose <- Data[['Clinical Information']][[input$humanDosing]][[paste0(humanDoseName,input$SMbasis)]]
          HED <- Dose
        }
        plotData[i, "HED_value"]<- round(HED, digits = 2) ##for table 03
        plotData[i, "SM"] <- round(HED/humanDose, digits = 2)
      }
    }
    #plotData <- cbind(plotData,SM, HED_value)
    return(plotData)
  })

## output table (changed) ----
  
  output$table <- renderDT({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      select(Study, Dose, NOAEL, Cmax, AUC, SM,HED_value, finding_rev, Severity) %>% 
      pivot_wider(names_from = finding_rev, values_from = Severity, values_fill = list(Severity = "Absent"))
    plotData_tab <- datatable(plotData_tab, rownames = FALSE, class = "cell-border stripe",
                              
                              options = list(
                                scrollY = TRUE,
                                pageLength = 25,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}")
                              ))
  })
  
  ### change path ----
  
  # 
  # options = list(autoWidth = TRUE,
  #                scrollX = TRUE,
  
  output$table_01 <- renderDT({
    plotData_01 <- calculateSM()
    plotData_01 <- plotData_01 %>% 
      select( Study,Findings, Rev, Severity, Dose, SM) %>% 
      group_by(Study, Dose)
    plotData_01 <- datatable(plotData_01,rownames = FALSE, 
                             extensions = list("Buttons" = NULL,
                                                "ColReorder" = NULL), 
                             class = "cell-border stripe",
                             #caption = "Nonclinical Findings of Potential Clinical Relevance",
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                               "Table :", htmltools::strong("Nonclinical Findings of Potential Clinical Relevance")
                             ),
                             options = list(
                               
                               #autoWidth = TRUE,
                                            #columnDefs = list(list(width = "150px", targets = "_all")),
                                            dom = "lfrtipB",
                                            buttons = c("csv", "excel", "copy", "print"),
                                            colReorder = TRUE,
                                            pageLength = 25,
                                            scrollY = TRUE,
                                            initComplete = JS(
                                              "function(settings, json) {",
                                              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                              "}"),
                             
                                            rowsGroup = list(0,1,2,3))) %>% 
      formatStyle(columns = colnames(plotData_01), `font-size` = "18px")
    path <- "yousuf" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    plotData_01$dependencies <- c(plotData_01$dependencies, list(dep))
    plotData_01
  })
  
  
  
  output$table_02 <- renderDT({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
    select( Findings,Rev, Study, Dose, SM) %>% 
      group_by(Findings, Dose)
      
    plotData_tab <- datatable(plotData_tab, rownames = FALSE, class = "cell-border stripe",
                              
                              options = list(
                                scrollY = TRUE,
                                pageLength = 25,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                
                                rowsGroup = list(0,1,2))) %>% 
      formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    path <- "yousuf" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  })
  
  
  ## from template 02 ----
  output$table_03 <- renderDT({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      select(Study, Dose, NOAEL, Cmax, AUC, SM, Findings) %>% 
      filter(NOAEL == TRUE)
      
    plotData_tab <- datatable(plotData_tab, rownames = FALSE, class = "cell-border stripe",
                              
                              options = list(
                                scrollY = TRUE,
                                pageLength = 25,
                                initComplete = JS(
                                  "function(settings, json) {",
                                  "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                  "}"),
                                rowsGroup = list(0,1,2,3,4,5)
                              )) %>% 
      formatStyle(columns = colnames(plotData_tab), `font-size` = "18px")
    
    path <- "yousuf" # folder containing dataTables.rowsGroup.js
    dep <- htmltools::htmlDependency(
      "RowsGroup", "2.0.0", 
      path, script = "dataTables.rowsGroup.js")
    plotData_tab$dependencies <- c(plotData_tab$dependencies, list(dep))
    plotData_tab
  })
  
  
  ## table 04 ----
  
  output$table_04 <- renderDT({
    plotData_04 <- calculateSM()
    plotData_04 <- plotData_04 %>% 
      select( Study,NOAEL, Dose, SM , HED_value, Cmax, AUC ) %>% 
      unique() %>% 
      filter(NOAEL == TRUE) %>% 
      select(-NOAEL) %>% 
      dplyr::rename( HED = HED_value, NOAEL = Dose) %>% 
      dplyr::mutate('Starting Dose' = NA, MRHD = NA) # have to change
    
    
      
    plotData_04 <- datatable(plotData_04,rownames = FALSE, 
                             extensions = list("Buttons" = NULL,
                                               "ColReorder" = NULL), 
                             class = "cell-border stripe",
                           
                             caption = htmltools::tags$caption(
                               style = "caption-side: top; text-align: center; font-size: 20px; color: black",
                               "Table :", htmltools::strong("Safety Margins Based on NOAEL from Pivotal Toxicology Studies")
                             ),
                             options = list(
                               
                               #autoWidth = TRUE,
                               #columnDefs = list(list(width = "150px", targets = "_all")),
                               dom = "lfrtipB",
                               buttons = c("csv", "excel", "copy", "print"),
                               colReorder = TRUE,
                               pageLength = 10,
                               scrollY = TRUE,
                               initComplete = JS(
                                 "function(settings, json) {",
                                 "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                 "}"))) %>% 
      formatStyle(columns = colnames(plotData_04), `font-size` = "18px")

    plotData_04
  })
  
  
  
  
  
  ### flextable for (table 01) ----
  
  table_01_fun <- reactive({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      select( Findings,Rev, Study, Dose, SM) %>% 
      #group_by(Findings, Rev, Study) %>% 
      dplyr::arrange(Findings, Rev, Study) %>% 
      flextable() %>% 
      merge_v(j = ~ Findings + Rev + Study) %>% 
      
      #flextable::autofit(add_w = 1) %>% 
      add_header_row(values = c("Nonclinical Findings of Potential Clinical Relevance"), colwidths = c(5)) %>% 
      theme_box()
      #fontsize(size = 18, part = "all") %>% 
    plotData_tab
    
  })
  
  output$table_01_flex <- renderUI({
    table_01_fun() %>% 
      flextable::autofit(add_w = 1) %>% 
      fontsize(size = 18, part = "all") %>% 
      htmltools_value()
    
  })
  
  table_01_down <- reactive({
    table_01_fun() %>% 
      
      #flextable::autofit() %>% 
      fit_to_width( max_width = 7) %>% 
      fontsize(size = 12, part = "all")
  })
  
  observeEvent(table_01_down(), {save_as_docx(table_01_down(), path = "table_01.docx")})
  
  
  
  output$down_01 <- downloadHandler(
    filename = function() {
      paste("table_01", ".docx")
    },
    content = function(file) {
      file.copy("table_01.docx", file)
      
      
    }
  )
  
  
  
  
  
  #### flextable 02
  
  table_02_fun <- reactive({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>% 
      dplyr::select(Study, Dose, NOAEL, Cmax, AUC, Findings) %>% 
      filter(NOAEL == TRUE) %>% 
      dplyr::select(-NOAEL) %>% 
      #group_by(Findings, Rev, Study) %>% 
      dplyr::arrange(Study, Dose) %>% 
      flextable() %>% 
      merge_v(j = ~ Study + Dose + Cmax+ AUC) %>% 
      #fontsize(size = 18, part = "all") %>% 
      #flextable::autofit(add_w = 1) %>% 
      theme_box()
    plotData_tab
    
  })
  
  
  output$table_02_flex <- renderUI({
    table_02_fun() %>% 
      flextable::autofit(add_w = 1) %>% 
      fontsize(size = 18, part = "all") %>% 
      htmltools_value()
    
  })
  
  table_02_down <- reactive({
    table_02_fun() %>% 
      
      #flextable::autofit() %>% 
      fit_to_width( max_width = 7) %>% 
      fontsize(size = 12, part = "all")
  })
  
  observeEvent(table_02_down(), {save_as_docx(table_02_down(), path = "table_02.docx")})
  
  
  
  output$down_02 <- downloadHandler(
    filename = function() {
      paste("table_02", ".docx")
    },
    content = function(file) {
      file.copy("table_02.docx", file)
      
      
    }
  )
  
  
  
  
 #### flextable 03 -----
  
  #table 03 
  table_03_fun <- reactive({
    plotData_tab <- calculateSM()
    plotData_tab <- plotData_tab %>%
      select( Study,NOAEL, Dose, HED_value, Cmax, AUC ) %>%
      unique() %>%
      filter(NOAEL == TRUE) %>%
      select(-NOAEL) %>%
      dplyr::rename( NOAEL = Dose, HED = HED_value) %>%
      dplyr::mutate('Starting Dose' = NA, MRHD = NA) %>%
      dplyr::arrange(Study, NOAEL) %>% 
      flextable() %>%
    #flextable::autofit() %>%
     add_header_row(values = c("Nonclinical", "Clinical Safety Margins"), colwidths = c(5,2)) %>%
     add_header_row(values = c("Safety Margins Based on NOAEL from Pivotal Toxicology Studies"), colwidths = c(7)) %>%
     theme_box()
    #fontsize(size = 12, part = "all")
    plotData_tab
    
    
  })
  
  output$table_03_flex <- renderUI({
    table_03_fun() %>% 
      flextable::autofit(add_w = 0.5) %>% 
      fontsize(size = 16, part = "all") %>% 
      htmltools_value()
    
  })
  
  table_03_down <- reactive({
    table_03_fun() %>% 
      
      #flextable::autofit() %>% 
      fit_to_width( max_width = 7) %>% 
      fontsize(size = 12, part = "all")
  })
  
  observeEvent(table_03_down(), {save_as_docx(table_03_down(), path = "table_03.docx")})
  
  
  
  output$down_03 <- downloadHandler(
    filename = function() {
      paste("table_03", ".docx")
    },
    content = function(file) {
      file.copy("table_03.docx", file)
      
      
    }
  )
  
  
  
  ## download all table 
  
  
  
  download_all <- reactive({
    doc <- read_docx("table_01.docx")
    doc_02 <- body_add_par(doc,"   ") %>% 
      body_add_flextable( table_02_down()) %>%
      body_add_par("   ") %>% 
      body_add_flextable(table_03_down()) %>% 
      body_add_par("   ")
      
    doc_02
  })
  
 observeEvent(download_all(), {print(download_all() , target = "table_all.docx")})
  
  
  
  output$down_all <- downloadHandler(
    filename = function() {
      paste("table_all", ".docx")
    },
    content = function(file) {
      file.copy("table_all.docx", file)
      
      
    }
  )
  
  
  
 #### plotheight ----

  plotHeight <- function() {
    plotData <- calculateSM()
    nStudies <- length(unique(plotData$Study))
    plotHeight <- as.numeric(250*nStudies)
  }
  

## Figure in UI

  output$figure <- renderPlotly({
    plotData <- calculateSM()
    
    ## plotdata for p plot (changed) ----
    plotData_p <- calculateSM() %>% 
      select(Study, Species, Months, Dose, SM, Value, NOAEL, Value_order) %>% 
      group_by(Study, Dose, SM) %>% 
      unique()
    
    if (nrow(plotData)>0) {
      plotData$Study <- factor(plotData$Study,levels=rev(input$displayStudies))
      plotData$DoseLabel <- factor(paste(plotData$Dose,'mg/kg/day'),levels=unique(paste(plotData$Dose,'mg/kg/day'))[order(unique(as.numeric(plotData$Dose),decreasing=F))])
      maxFindings <- 1
      for (doseFinding in plotData$doseFindings) {
        nFindings <- str_count(doseFinding,'\n')
        if (nFindings > maxFindings) {
          maxFindings <- nFindings
        }
      }
      maxFindings <- maxFindings + 1


      
      plotData$Findings <- as.factor(plotData$Findings)
      plotData$Severity <- as.factor(plotData$Severity)
      # make severity ordered factor
      plotData$Severity <- factor(plotData$Severity, 
                                  levels= c('Absent','Present','Minimal', 'Mild',
                                            'Moderate', 'Marked', 'Severe'), ordered = TRUE)
      
      color_manual <- c('transparent','black','#feb24c','#fd8d3c','#fc4e2a','#e31a1c','#b10026')

# order of study need to be fixed
  
      
# # Study vs safety margin plot  (changed) -------
      color_NOAEL <- c("TRUE" = "#239B56", "FALSE" = "black")
      
      
      p <- ggplot(plotData_p)+
        geom_tile(aes (x = SM, y = Value_order, fill = NOAEL), 
                  color = "transparent", width = 0.40, height = 0.65)+
        geom_text(aes(x = SM, y = Value_order, label = paste(Dose, " mg/kg/day")), #DoseLabel changed
                  color = "white", fontface = "bold")+
        scale_x_log10(limits = c(min(plotData_p$SM/2), max(plotData_p$SM*2)),sec.axis = dup_axis())+
        scale_fill_manual(values = color_NOAEL)+
        facet_nested( Species+ Months ~ .)+
        labs( title = "Summary of Toxicology Studies")+
        theme_bw(base_size=12)+
        theme(axis.title.y = element_blank(),
              axis.ticks.y= element_blank(),
              axis.text.y = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.title = element_text(hjust = 0.5),
              legend.position = "none",
              strip.text.y = element_text(size=7, color="black", face="plain"),
              strip.background = element_rect( fill = "white"))
      
      
      q <- ggplot(plotData)+
        geom_col(aes(x= Findings, y = Value, fill = Severity, group = Dose),
                 position = position_stack(reverse = TRUE),
                 color = 'transparent')+
        geom_text(aes(x = Findings, y = Value, label = Dose, group = Dose),
                  size = 4,
                  color = 'white',
                  fontface = 'bold',
                  position = position_stack(vjust = 0.5, reverse = TRUE))+
        scale_y_discrete(position = 'right')+
        scale_fill_manual(values = color_manual)+
        facet_grid(Study ~ ., scales = 'free')+
        theme_bw(base_size=12)+
        theme(axis.title.y = element_blank(),
              strip.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.text.x = element_text(angle = 90),
              plot.title = element_text(hjust = 0.5),
              panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.x = element_line(),
              panel.grid.minor.x = element_blank(),
              legend.justification = "top")+
        labs(title = 'Findings' )+
        guides(fill = guide_legend(override.aes = aes(label = "")))
      
      #p + q + plot_layout(ncol=2,widths=c(3,1))
      
      #ggplotly(p, tooltip = "x")
      
      p <- ggplotly(p, tooltip = "x", height = plotHeight())
      q <- ggplotly(q, tooltip = "x",  height = plotHeight())
      
      subplot(p, q, nrows = 1, widths = c(0.7, 0.3), titleX = TRUE, titleY = TRUE) %>% 
        layout(title= "Summary of Toxicology Studies",
               xaxis = list(title = "Safety Margin"), 
               xaxis2 = list(title = "Findings"))
               
      
      
     #  p <- ggplot(plotData_p)+
     #    geom_label(aes(x = SM, y = Value, label = paste(Dose, " mg/kg/day")),
     #               color = 'white',
     #               size = 6,
     #               fill = ifelse(plotData_p$NOAEL == TRUE, "#239B56", "black"),
     #               label.padding = unit(0.8, "lines"),
     #               fontface = "bold",
     #               position = ggstance::position_dodge2v(height = 1, preserve = "total",padding=2))+
     # 
     #    scale_x_log10(limits = c(min(plotData_p$SM/2),
     #                             max(plotData_p$SM*2)), sec.axis = dup_axis())+
     #    facet_nested( Species+ Months ~ .)+
     # 
     #    labs(x = "Safety Margin", title = "Summary of Toxicology Studies")+
     #    theme_bw(base_size=12)+
     #    theme(axis.title.y = element_blank(),
     #          axis.ticks.y= element_blank(),
     #          axis.text.y = element_blank(),
     #          panel.grid.major = element_blank(),
     #          panel.grid.minor = element_blank(),
     #          plot.title = element_text(hjust = 0.5),
     #          strip.text.y = element_text(size=11, color="black", face="plain"),
     #          strip.background = element_rect( fill = "white"))
     # 
     #  
     # # findings plot
     #  
     #  q <- ggplot(plotData)+
     #    geom_col(aes(x= Findings, y = Value, fill = Severity, group = Dose), 
     #             position = position_stack(reverse = TRUE), 
     #             color = 'transparent')+  
     #    geom_text(aes(x = Findings, y = Value, label = Dose, group = Dose),
     #              size = 5,
     #              color = 'white',
     #              fontface = 'bold',
     #              position = position_stack(vjust = 0.5, reverse = TRUE))+
     #    scale_y_discrete(position = 'right')+ 
     #    scale_fill_manual(values = color_manual)+
     #    
     #    facet_grid(Study ~ ., scales = 'free')+
     #    
     #    theme_bw(base_size=12)+
     #    theme(axis.title.y = element_blank(),
     #          #strip.text.y = element_blank(),
     #          axis.ticks.y = element_blank(),
     #          axis.text.y = element_blank(),
     #          axis.title.x = element_blank(), 
     #          axis.text.x = element_text(angle = 90),
     #          plot.title = element_text(hjust = 0.5),
     #          panel.grid.major.y = element_blank(),
     #          panel.grid.minor.y = element_blank(),
     #          panel.grid.major.x = element_line(),
     #          panel.grid.minor.x = element_blank(),
     #          legend.justification = "top")+
     #    labs(title = 'Findings' )+
     #    guides(fill = guide_legend(override.aes = aes(label = "")))
     #  p + q + plot_layout(ncol=2,widths=c(3,1))
     #  
      
    }
  })

  observe({
    req(input$selectData)
    values$selectData <- input$selectData
  })
  
  # output$menu function -----
  
  output$menu <- renderMenu({
    if (!is.null(input$selectData)) {
      if (input$selectData=='blankData.rds') {
        sidebarMenu(id='menu',
                    menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                             uiOutput('selectData'),
                             conditionalPanel('input.selectData=="blankData.rds"',
                                              textInput('newApplication','Enter Tox Progam Name:')
                             ),
                             actionButton('saveData','Open New Program',icon=icon('plus-circle')),
                             br()
                    ),
                    br(),
                    uiOutput('studyName'),
                    br(),
                    br()
        )
      } else {
        sidebarMenu(id='menu',
                    menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                             uiOutput('selectData'),
                             conditionalPanel('input.selectData=="blankData.rds"',
                                              textInput('newApplication','Enter Tox Program Name:')
                             ),
                             actionButton('deleteData','Delete Program',icon=icon('minus-circle')),
                             br()
                    ),
                    hr(),
                    uiOutput('studyName'),
                    hr(),
                    menuItem('Clinical Data',icon=icon('user'),
                             checkboxGroupInput('clinDosing','Clinical Dosing:',clinDosingOptions),
                             conditionalPanel('condition=input.MgKg==false',
                                              numericInput('HumanWeight','*Human Weight (kg):',value=60)
                             ),
                             checkboxInput('MgKg','Dosing in mg/kg?',value=F),
                             conditionalPanel(
                               condition='input.clinDosing.includes("Start Dose")',
                               h4('Start Dose Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('StartDoseMgKg','*Start Dose (mg/kg/day):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('StartDose','*Start Dose (mg/day):',value = NULL)
                               ),
                               numericInput('StartDoseCmax','Start Dose Cmax (ng/mL):',value=NULL),
                               numericInput('StartDoseAUC','Start Dose AUC (ng*h/mL):',value=NULL)
                             ),
                             conditionalPanel(
                               condition='input.clinDosing.includes("MRHD")',
                               h4('MRHD Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('MRHDMgKG','*MRHD (mg/kg):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('MRHD','*MRHD (mg):',value = NULL)
                               ),
                               numericInput('MRHDCmax','MRHD Cmax (ng/mL):',value=NULL),
                               numericInput('MRHDAUC','MRHD AUC (ng*h/mL):',value=NULL)
                             ),
                             conditionalPanel(
                               condition='input.clinDosing.includes("Custom Dose")',
                               h4('Custom Dose Information:'),
                               conditionalPanel(condition='input.MgKg==true',
                                                numericInput('CustomDoseMgKG','*Custom Dose (mg/kg):',value=NULL)
                               ),
                               conditionalPanel(condition='input.MgKg==false',
                                                numericInput('CustomDose','*Custom Dose (mg):',value = NULL)
                               ),
                               numericInput('CustomDoseCmax','Custom Dose Cmax (ng/mL):',value=NULL),
                               numericInput('CustomDoseAUC','Custom Dose AUC (ng*h/mL):',value=NULL)
                             ),
                             actionButton('saveClinicalInfo','Save Clinical Information',icon=icon('plus-circle')),
                             br()
                    ),                   
                    menuItem('Nonclinical Data',icon=icon('flask'),tabName = 'Nonclinical Info',
                             uiOutput('selectStudy'),
                             actionButton('saveStudy','Save Study',icon=icon('plus-circle')),
                             actionButton('deleteStudy','Remove Study',icon=icon('minus-circle')),
                             
                             h4('Study Name:'),
                             verbatimTextOutput('studyTitle'),
                             
                             selectInput('Species','*Select Species:',choices=names(speciesConversion)),
                             textInput('Duration','*Study Duration/Description:'),
                             numericInput('nDoses','Number of Dose Levels:',value=3,step=1,min=1),
                             uiOutput('Doses'),
                             numericInput('nFindings','Number of Findings:',value=0,step=1,min=0),
                             uiOutput('Findings'),
                             br()
                    ),
                    hr(),
                    h6('* Indicates Required Fields')
                    
        )
      }
    } else {
      sidebarMenu(id='menu',
                  menuItem('Data Selection',icon=icon('database'),startExpanded = T,
                           uiOutput('selectData'),
                           conditionalPanel('input.selectData=="blankData.rds"',
                                            textInput('newApplication','Enter Tox Program Name:')
                           ),
                           actionButton('saveData','Open New Program',icon=icon('plus-circle')),
                           br()
                  ),
                  br(),
                  uiOutput('studyName'),
                  br(),
                  br()
      )
    }
  })
}


# ui function ------
ui <- dashboardPage(
  
  dashboardHeader(title="Nonclinical Summary Tool",titleWidth = 250),
  
  dashboardSidebar(width = 250,
                   sidebarMenuOutput('menu')
  ),
  
  dashboardBody(
    fluidRow(
      column(4,
             uiOutput('humanDosing')
      ),
      column(4,
             conditionalPanel(
               'input.clinDosing != null && input.clinDosing != ""',
               selectInput('SMbasis','Base Safety Margin on:',c('HED','Cmax','AUC'))
             )
      ),
      column(4,
             uiOutput('displayStudies')
      )
    ),
    conditionalPanel(
      condition='input.selectData!="blankData.rds"',
      tabsetPanel(
        
        tabPanel('Figure',
                 actionButton('refreshPlot','Refresh Plot'),
                 br(),
                 plotlyOutput('figure')
        ),
        
        tabPanel('Table',
                 DT::dataTableOutput('table')
                 
        ),
        
        tabPanel("Table_01",
                 DT::dataTableOutput('table_01')
      ),
      tabPanel("Table_02",
               DT::dataTableOutput('table_02')
      ),
      tabPanel("Table_03",
               DT::dataTableOutput('table_03')
      ),
      
      tabPanel("Table_04",
               DT::dataTableOutput('table_04')
      ),
      
      tabPanel("Table_01_flex",
               uiOutput('table_01_flex'),
               downloadButton('down_01', 'Download')
      ),
      
      tabPanel("Table_02_flex",
               uiOutput('table_02_flex'),
               downloadButton('down_02', 'Download')
      ),
      
      tabPanel("Table_03_flex",
               h4("Safety Margins Based on NOAEL from Pivotal Toxicology Studies"),
               uiOutput('table_03_flex'),
               downloadButton('down_03', 'Download')
      ),
      
      tabPanel("Table_All",
               downloadButton('down_all', 'Download_All')
      )
      
      
      
      
     
  ))))



# app running function ----

shinyApp(ui = ui, server = server)
