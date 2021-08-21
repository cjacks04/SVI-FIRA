library(shiny)
library(stringr)

options(scipen=100)      

#The below list helps in populating county names
county_csv <- read.csv("V:\\FIRA WEDSS\\Analysis\\Wisconsin_COUNTY.csv")
cty_options <- sort(county_csv$COUNTY)

#The below list helps in populating census tract names
census_csv <- read.csv("V:\\FIRA WEDSS\\Analysis\\Wisconsin.csv")
cen_options <- sort(census_csv$LOCATION)
cen_options <- strsplit(cen_options," County, Wisconsin")

#The below list helps in populating vaccine encouragements, this file needs to 
#be replaced with the latest one
enco_csv <- read.csv("V:\\FIRA WEDSS\\Analysis\\encouragement-example.csv")

#The R files used to generate vaccine rate plots in census tracts and counties
source("V:\\FIRA WEDSS\\Analysis\\county_analysis.R")
source("V:\\FIRA WEDSS\\Analysis\\ct_analysis.R")

source("V:\\FIRA WEDSS\\Analysis\\county_analysis_scatter.R")
source("V:\\FIRA WEDSS\\Analysis\\ct_analysis_scatter.R")

#The actual immunization data to work with in the analysis 
filename <- "V:\\FIRA WEDSS\\CJ_COVID19_Immunization_20210709_092302.txt"
my_data <- read.table(filename,sep="|",header = TRUE, dec = ".")

#Creating directories to store vaccine rate plots in census tracts and counties
cnty_dir_path <- "V:\\FIRA WEDSS\\Analysis\\County_Result"
ct_dir_path <- "V:\\FIRA WEDSS\\Analysis\\CT_Result"

dir.create(cnty_dir_path)
dir.create(ct_dir_path)

#This is a placeholder text 
para <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut est et lacinia.
In et congue ex. Aenean nec nunc vulputate, auctor mi nec, sollicitudin mauris. Vestibulum eget bibendum ipsum. Praesent convallis sit amet sem sit amet pulvinar. Nam vestibulum nunc nisi, in egestas tellus iaculis quis. Aliquam vulputate leo vel est posuere, in auctor neque dictum. Etiam rutrum neque a velit efficitur pharetra. Duis vitae dui dolor. Proin euismod eros in ligula ultrices, vel facilisis massa porta. Nam at tortor metus."

get_fips_county <- function(loca){
  svi <- "V:\\FIRA WEDSS\\Analysis\\Wisconsin_COUNTY.csv"
  data <- read.csv(svi)
  retval <- subset(data, data$COUNTY == loca)
  ct <- retval$FIPS
  ct <- substr(ct,1,5) 
  return (ct)
}

get_fips_census <- function(loca) {
  ct_name <- paste(loca," County, Wisconsin",sep="")
  
  svi <- "V:\\FIRA WEDSS\\Analysis\\Wisconsin.csv"
  data <- read.csv(svi)
  retval <- subset(data, data$LOCATION == ct_name)
  ct <- retval$FIPS
  ct <- substr(ct,3,11)
  return (ct)
}

# The inputs and outputs in tab1, tab2 and tab3 are named with _1, _2 and _3 respectively

server <- function(input, output, session) {
  
    # To fill counties/census tracts based on geography type selected in Tab1
    observeEvent(input$type_1,
               {
                 if (input$type_1 == "Counties") {
                  updateSelectInput(session,"loc_1",choices=cty_options)
                 }
                 else if (input$type_1 == "Census Tracts") {
                   updateSelectInput(session,"loc_1",choices=cen_options)
                 } 
              })
  
    # To fill counties/census tracts based on geography type selected in Tab2
    observeEvent(input$type_2,
               {
                 if (input$type_2 == "Counties") {
                   updateSelectInput(session,"loc_2",choices=cty_options)
                 }
                 else if (input$type_2 == "Census Tracts") {
                   updateSelectInput(session,"loc_2",choices=cen_options)
                 } 
               })
    
    # To fill counties/census tracts based on geography type selected in Tab3
    observeEvent(input$type_3,
                 {
                   if (input$type_3 =="Counties") {
                     updateSelectInput(session,"loc_3",choices=cty_options)
                   }
                   else if (input$type_3 =="Census Tracts") {
                     updateSelectInput(session,"loc_3",choices=cen_options)
                   } 
                 })
    
    # To fill vaccine encouragements for the selected county/census tract in Tab3
    observeEvent(input$loc_3,
                 {
                   if (input$type_3 =="Counties") {
                     
                     #get fips code
                     ct <- get_fips_county(input$loc_3)
                     
                     enco_data <- subset(enco_csv, substr(enco_csv$FIPS,1,5) == ct)
                     len <- length(enco_data$FIPS)
                     i <- 1
                     inter_list <- c()
                     
                     while (i <= len) {
                       inter <- paste(enco_data$Intervention.name[i],",",enco_data$Address[i],",",enco_data$Date.added[i],sep="")
                       inter_list[i] <- inter
                       i <- i+1
                     }
                     
                     if (len > 1)
                       updateSelectInput(session,"enco_3",choices=inter_list)
                     else 
                       updateSelectInput(session,"enco_3",choices="No encouragements found")
                   }
                   else if (input$type_3 =="Census Tracts") {
                     
                     #get fips code
                     ct <- get_fips_census(input$loc_3)
                     
                     enco_data <- subset(enco_csv, substr(enco_csv$FIPS,3,11) == ct)
                     len <- length(enco_data$FIPS)
                     i <- 1
                     inter_list <- c()
                     
                     while (i <= len) {
                       inter <- paste(enco_data$Intervention.name[i],",",enco_data$Address[i],",",enco_data$Date.added[i],sep="")
                       inter_list[i] <- inter
                       i <- i+1
                     }
                     
                     if(len > 1)
                       updateSelectInput(session,"enco_3",choices=sort(inter_list))
                     else 
                       updateSelectInput(session,"enco_3",choices="No encouragements found")
                   } 
                 })
  
    # To get vaccine rate plots in counties/census tracts
    observeEvent(input$submit_1,
                 {
                    if (input$type_1 == "Counties") {
                      
                      output$plot_1 <- renderImage({
                        outfile <- call_cnty(cnty_dir_path,my_data,input$loc_1,
                                             input$gender_1,input$ethnicity_1,
                                             input$race_1,input$age_1)
                        list(src=outfile, alt="Some text")
                      },deleteFile=TRUE)
                    }
                    else if (input$type_1 == "Census Tracts") {
                     
                     output$plot_1 <- renderImage({
                       outfile <- call_ct(ct_dir_path,my_data,input$loc_1,
                                          input$gender_1,input$ethnicity_1,
                                          input$race_1, input$age_1)
                       list(src=outfile, alt="Some text")
                     },deleteFile=TRUE)
                    }
                   # Some placeholder text to output in tab1, this should be moved in above observeEvent 
                   # and text should be according to the geography type selected
                   output$text_1 <- renderUI(HTML("<br/><br/><br/><br/><p align=justify>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut est et lacinia.
</p>
                            <h4><i>Sample Sub heading</i></h4>
                            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odi.</p>
                            <ul><li>Lorem ipsum dolor sit amet</li><li>consectetur adipiscing elit</li></ul>"
                   ))
                   
                 })
    
    observeEvent(input$submit_2, {
      # Some placeholder table to output in tab2
      if (input$type_2 == "Counties") {
        ct <- get_fips_county(input$loc_2)
        tab <- subset(enco_csv,substr(enco_csv$FIPS,1,5) == ct)
        output$table_2 <- renderDataTable({
        
        columns2show <- c("Date.added","Intervention.name", "Intervention.Location","Address")
        columns2hide <- which(!(colnames(tab) %in% columns2show))
        
        if (!is.null(tab)) {
          datatable(tab,selection='single',rownames=FALSE,
                    ##columns to hide##
                    options = list(columnDefs=list(list(visible=FALSE,targets=columns2hide-1))))  
          
        }
        })
      }
      else if (input$type_2 == "Census Tracts") {
        ct <- get_fips_census(input$loc_2)
        tab <- subset(enco_csv,substr(enco_csv$FIPS,3,11) == ct)
        output$table_2 <- renderDataTable({
          columns2show <- c("Date.added","Intervention.name", "Intervention.Location","Address")
          columns2hide <- which(!(colnames(tab) %in% columns2show))
          
          if (!is.null(tab)) {
            datatable(tab,selection='single',rownames=FALSE,
                      ##columns to hide##
                      options = list(columnDefs=list(list(visible=FALSE,targets=columns2hide-1))))  
            
          }  
        })
      }
      
      # Some placeholder text to output in tab2
      output$text_2 <- renderUI(HTML("<br/><p align=justify>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut est et lacinia.
</p>
                            <h4><i>Sample Sub heading</i></h4>
                            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odi.</p>
                            <ul><li>Lorem ipsum dolor sit amet</li><li>consectetur adipiscing elit</li></ul>"
      ))
    })
  
    observeEvent(input$submit_3, {
      if (input$type_3 == "Counties") {
        
        enco <- gsub(".*,","",input$enco_3)
        enco <- substr(enco,1,nchar(enco)-5)
        enco <- as.Date(enco,"%m/%d")
        
        output$plot_3 <- renderImage({
          outfile <- call_cnty_scatter(cnty_dir_path,my_data,input$loc_3,
                               input$gender_3,input$ethnicity_3,
                               input$race_3,input$age_3, enco, input$period_3)
          list(src=outfile, alt="Some text")
        },deleteFile=TRUE)
      }
      else if (input$type_3 == "Census Tracts") {
        
        enco <- gsub(".*,","",input$enco_3)
        enco <- substr(enco,1,nchar(enco)-5)
        enco <- as.Date(enco,"%m/%d")
        
        output$plot_3 <- renderImage({
          outfile <- call_ct_scatter(ct_dir_path,my_data,input$loc_3,
                             input$gender_3,input$ethnicity_3,
                             input$race_3, input$age_3, enco, input$period_3)
          list(src=outfile, alt="Some text")
        },deleteFile=TRUE)
      }
      
      # Some placeholder text to output in tab3
      output$text_3 <- renderUI(HTML("<br/><br/><br/><br/><h4>Analysis of x encouragement and its impact on vaccinations in census tract</h4>
                            <p align=justify>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odio, eu suscipit dolor fringilla et. In ipsum nisi, ornare vitae purus at, maximus sodales nibh. Pellentesque bibendum lectus et nisl tristique, et efficitur nulla aliquam. Sed sit amet dui ut dui vestibulum sodales. Cras tempor nisi efficitur leo malesuada, eget volutpat lorem vulputate. Suspendisse in pulvinar est. Phasellus feugiat ut est et lacinia.
</p>
                            <h4><i>Sample Sub heading</i></h4>
                            <p>Lorem ipsum dolor sit amet, consectetur adipiscing elit. Suspendisse pretium enim odi.</p>
                            <ul><li>Lorem ipsum dolor sit amet</li><li>consectetur adipiscing elit</li></ul>"
      ))
    })
}