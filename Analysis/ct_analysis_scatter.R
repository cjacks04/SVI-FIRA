#This function collects immunization data about the selected census tract and plots a 
#scatterplot graph to denote the vaccine rate in that census tract around the 
#encoragement period

#Initially, the immunization data is filtered based on the selected options such 
#as census tract, age group, gender, ethnicity and race. From the filtered data, 
#vaccination dose 1 dates are retrieved, and filtered to match the period around 
#encoragement and the frequency of them are calculated. Vaccine dates and their 
#cumulative counts over the days are used to plot the required vaccine count 
#scatterplot graph.

#It is very important to note that in FIPS code, in addition to census tract 
#number,we must include the county code as well as different counties have the 
#same census tract number

#dir_path - directory created to store the vaccine plots
#my_data - parsed immunization data
#enco - date of encouragement
#period - period of time before and after an encouragement to be considered

call_ct_scatter <- function(dir_path, my_data, ct_name, gender, ethnicity, race, age,enco,period) {
  
  copy_ct_name <- ct_name
  ct_name <- paste(ct_name," County, Wisconsin",sep="")
  
  #From the census tract name, find the census tract FIPS
  svi <- "V:\\FIRA WEDSS\\Analysis\\Wisconsin.csv"
  data <- read.csv(svi)
  retval <- subset(data, data$LOCATION == ct_name)
  ct <- retval$FIPS
  ct <- substr(ct,3,11)
  tot_pop <- retval$E_TOTPOP
  age_65 <-  retval$E_AGE65
  
  #Metadata of the census tract to display in graph
  str1 <- "Vaccine Rate (2021) in "
  str2 <- paste(dir_path,"\\CT_",sep="")
  str3 <- ct
  copy_ct_name <- paste(copy_ct_name," County",sep="")
  header <- paste(str1, copy_ct_name, sep="")
  graph_img <- paste(str2,str3,sep="")
  jpeg_img <- paste(graph_img, ".png", sep="")
  
  #Save the vaccine rate plot as .png to show to users   
  png(file = jpeg_img)
  
  # Find rows of a specific census tract
  if (ct_name == "All census tracts") {
    CT_rows <- my_data
  }
  else {
    CT_rows <- subset(my_data, substr(my_data$Census_Blk,3,11) == ct)
  }
  
  # Find rows of specific gender in that census tract
  if (gender == "male") {
    CT_rows <- subset(CT_rows, CT_rows$Gender == "M")
  }
  else if (gender == "female") {
    CT_rows <- subset(CT_rows, CT_rows$Gender == "F")
  }
  
  # Find rows of specific race in that census tract
  if (race != "All") {
    CT_rows <- subset(CT_rows, (gsub("/.*", "", CT_rows$X.Race.Ethnicity.) == race))
  }
  
  # Find rows of specific ethnicity in that census tract
  if (ethnicity == "yes") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Hispanic or Latino"))
  }
  else if (ethnicity == "no") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Not Hispanic or Latino"))
  }
  else if (ethnicity == "unknown") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Unknown"))
  }
  
  # Find rows of specific age group in that census tract
  if (age == "a65") {
    CT_rows <- subset(CT_rows, (as.integer(substr(CT_rows$DOB,1,4))) < 1956)
  }
  else if (age == "b18") {
    CT_rows <- subset(CT_rows, (as.integer(substr(CT_rows$DOB,1,4))) > 2002)
  }
  
  # With regex, extract only Vaccination date 1 field for a census tract
  i <- 1
  dates <- CT_rows$COVID19VaccinationDate_1
  
  #get vaccine dates around the encouragement period
  #if period not specified, get all dates
  if (period == "")
    period <- 200
  else
    period <- as.integer(period)
  
  begin <- as.Date(enco) - period
  last <- as.Date(enco) + period

  #Ignore vaccine dates from year 2020 
  date_list <- c()
  for (x in dates) {
    if (!(grepl("2020",x,fixed=TRUE))) {
      if (x >= begin && x <= last) {
        date_list[i] <- x
        i <- i + 1
      }
    }
  }
  
  list1 <- date_list[!is.na(date_list)]
  
  # Get dates and their frequencies
  date <- sort(unique(list1))
  vaccination_rate <- as.integer(table(list1))
  
  #To get vaccination rate as percentage
  #Uncomment the below section to get vaccination rate in y axis
  # #get cumulative vaccination rate for weeks
  # i <- 1
  # tot <- 0
  # for (x in vaccination_rate) {
  #   tot <- tot + x
  #   vaccination_rate[i] <- tot
  #   i <- i + 1
  # }
  # 
  # # get percentage of vaccination rate in total pop for weeks
  # i <- 1
  # tot <- 0
  # for (x in vaccination_rate) {
  #   tot <- (x/tot_pop)*100
  #   vaccination_rate[i] <- tot
  #   i <- i + 1
  # }
  
  #This is to ignore the year 
  date <- substr(date,6,10)
  
  len <- length(vaccination_rate)
  
  # If there are no vaccine dates available, return
  if (len == 0) {
    dev.off()
    return (jpeg_img)
  }
  
  date <- as.Date(date, "%m-%d")
  
  #This is to create a vertical line during the date of encouragement 
  max_rate <- max(sapply(vaccination_rate,max))
  enco_list <- c(enco,enco)
  enco_list <- as.Date(enco_list, "%Y-%m-%d")
  count <- c(0,max_rate)
  
  # Plot the graph denoting vaccine rate in the census tract
  if (len < 2) {
    plot(date, vaccination_rate, col = "blue", pch = 19, lty =1, ylab = "vaccine count", xlab = "date", main = header,ylim=c(2,10))
  } else {
    plot(date, vaccination_rate, col = "blue", pch = 19, lty =1, ylab = "vaccine count", xlab = "date", main = header)
    lines(enco_list,count,col="red",lty=2)
  }
  
  dev.off()
  return (jpeg_img)
}

