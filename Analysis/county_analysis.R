#This function collects immunization data about the selected county and plots a 
#graph to denote the vaccine rate in that county

#Initially, the immunization data is filtered based on the selected options such 
#as county, age group, gender, ethnicity and race. From the filtered data, 
#vaccination dose 1 dates are retrieved, and the equivalent week number for the 
#dates are found. Frequency of the week numbers are found. Week numbers and their 
#cumulative counts over the weeks are used to plot the required vaccine rate graph.

#It is very important to note that in FIPS code, in addition to census tract 
#number,we must include the county code as well as different counties have the 
#same census tract number

#dir_path - directory created to store the vaccine plots
#my_data - parsed immunization data

call_cnty <- function(dir_path, my_data, cnty_name, gender, ethnicity, race, age) {
  
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
  
  #Ignore vaccine dates from year 2020 
  date_list <- c()
  for (x in dates) {
    if (!(grepl("2020",x,fixed=TRUE))) {
      date_list[i] <- x
      i <- i + 1
    }
  }
  
  date_list <- date_list[!is.na(date_list)]
  
  # Find the equivalent week of the Vaccination date
  list1 <- lubridate::isoweek(date_list)
  
  # Get weeks and their frequencies
  a <- sort(unique(list1))
  b <- as.integer(table(list1))

  # Removing last week in 2020
  j <- 0
  for (x in a) {
    j <- j+1  
  }
  week <- a
  if (j > 0 && a[j] > 40)
    week <- a[-j]
  
  # If there are no vaccine dates available, return 
  if (length(week) == 0) {
    dev.off()
    return (jpeg_img)
  }
  
  # Some weeks may not have any vaccine date, hence filling those weeks' counts with 0
  max <- week[length(week)]
  
  week <- c()
  i <- 1
  while (i <= max) {
    week[i] <- i
    i <- i+1
  }
  
  vaccination_rate <- c()
  i <- 1
  cnt <- 1
  while (cnt <= max) {
    cnt_s <- toString(cnt)
    if (is.na(table(list1)[cnt_s]))
      vaccination_rate[i] = 0
    else
      vaccination_rate[i] = table(list1)[cnt_s]
    cnt <- cnt+1
    i <- i+1
  }
  
  # get cumulative vaccination rate for weeks
  i <- 1
  tot <- 0
  for (x in vaccination_rate) {
    tot <- tot + x
    vaccination_rate[i] <- tot
    i <- i + 1
  }
  
  # get percentage of vaccination rate in total pop for weeks
  i <- 1
  tot <- 0
  for (x in vaccination_rate) {
    tot <- (x/tot_pop)*100
    vaccination_rate[i] <- tot
    i <- i + 1
  }
  
  # Plot the graph denoting vaccine rate in the county 
  if (j < 2) {
    plot(week, vaccination_rate, type = "o", col = "blue", pch = "o", lty =1, ylab = "vaccine_rate (in % of tot_pop)", xlab = "Week", main = header,ylim=c(2,10))
  } else {
    plot(week, vaccination_rate, type = "o", col = "blue", pch = "o", lty =1, ylab = "vaccine_rate (in % of tot_pop)", xlab = "Week", main = header)
  }
  
  dev.off()
  return (jpeg_img)
}

