#This function collects immunization data about the selected county and plots a 
#scatterplot graph to denote the vaccine rate in that county around the encoragement 
#period

#Initially, the immunization data is filtered based on the selected options such 
#as county, age group, gender, ethnicity and race. From the filtered data, 
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

call_cnty_scatter <- function(dir_path, my_data, cnty_name, gender, ethnicity, race, age, enco,period) {
  
  #From the county name, find county FIPS
  svi <- "V:\\FIRA WEDSS\\Analysis\\Wisconsin_COUNTY.csv"
  data <- read.csv(svi)
  retval <- subset(data, data$COUNTY == cnty_name)
  ct <- retval$FIPS
  tot_pop <- retval$E_TOTPOP
  age_65 <-  retval$E_AGE65
  
  #Metadata of the county to display in graph
  str1 <- "Vaccine Rate (2021) in County "
  str2 <- paste(dir_path,"\\County_",sep="")
  str3 <- substr(ct,3,5)
  header <- paste(str1, cnty_name, sep="")
  graph_img <- paste(str2,str3,sep="")
  jpeg_img <- paste(graph_img, ".png", sep="")
  
  #Save the vaccine rate plot as .png to show to users  
  png(file = jpeg_img)
  
  #Find rows of a specific county
  if (cnty_name == "All counties") {
    CT_rows <- my_data
  }
  else {
    CT_rows <- subset(my_data, substr(my_data$Census_Blk,1,5) == ct)
  }
  
  #Find rows of specific gender in that county
  if (gender == "male") {
    CT_rows <- subset(CT_rows, CT_rows$Gender == "M")
  }
  else if (gender == "female") {
    CT_rows <- subset(CT_rows, CT_rows$Gender == "F")
  }
  
  #Find rows of specific race in that county
  if (race != "All") {
    CT_rows <- subset(CT_rows, (gsub("/.*", "", CT_rows$X.Race.Ethnicity.) == race))
  }
  
  #Find rows of specific ethnicity in that county
  if (ethnicity == "yes") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Hispanic or Latino"))
  }
  else if (ethnicity == "no") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Not Hispanic or Latino"))
  }
  else if (ethnicity == "unknown") {
    CT_rows <- subset(CT_rows, (gsub(".*/","",CT_rows$X.Race.Ethnicity.) == "Unknown"))
  }
  
  #Find rows of specific age group in that county
  if (age == "a65") {
    CT_rows <- subset(CT_rows, (as.integer(substr(CT_rows$DOB,1,4))) < 1956)
  }
  else if (age == "b18") {
    CT_rows <- subset(CT_rows, (as.integer(substr(CT_rows$DOB,1,4))) > 2002)
  }
  
  #With regex, extract only Vaccination date 1 field for a county
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
  
  #ignore dates other than 2021, retrieve only dates within the encouragement period
  date_list <- c()
  i <- 1
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
  #To get vaccination count
  vaccination_rate <- as.integer(table(list1))
  
  #To get vaccination rate as percentage
  #Uncomment the below section to get vaccination rate in y axis
  ## get cumulative vaccination rate for weeks
  # i <- 1
  # tot <- 0
  # for (x in vaccination_rate) {
  #   tot <- tot + x
  #   vaccination_rate[i] <- tot
  #   i <- i + 1
  # }
  
  ## get percentage of vaccination rate in total pop for weeks
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

  # Plot the graph denoting vaccine rate in the county 
  if (len < 2) {
    plot(date, vaccination_rate, col = "blue", pch = 19, lty =1, ylab = "vaccine count", xlab = "date", main = header,ylim=c(2,10))
  } else {
    plot(date, vaccination_rate, col = "blue", pch = 19, lty =1, ylab = "vaccine count", xlab = "date", main = header)
    lines(enco_list,count,col="red",lty=2)
  }
  
  dev.off()
  return (jpeg_img)
}

