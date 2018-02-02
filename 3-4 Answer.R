{
  rm(list=ls());  options(show.error.locations = TRUE);
  
  ### Read data from CSV file and save to a variable
  lansJanTempsDF = read.csv(file = "data/lansingJanTemps.csv"); 
  
  # Default variable is data frame -- convert the variable to a matrix
  lansJanTemps = as.matrix(x=lansJanTempsDF);
  
  # Creating a boxplot of each of the six year's worth of January temperatures (the columns)
  boxplot(x=lansJanTemps, use.cols = TRUE);
  
  # Add labels and names to the plots to the boxplot
  boxplot(x=lansJanTemps, use.cols = TRUE,
          names = c(2011,2012,2013,2014,2015,2016),
          main="January 2011 through January 2016",
          xlab="Temperature (Fahrenheit)",
          ylab="Years");
  
  ##### 1st t-test #####
  # compare the means from Jan 2014 and Jan 2011 using a t-test
  tTest1 = t.test(x=lansJanTemps[,4], y=lansJanTemps[,1]);
  # compare the means from Jan 2014 and Jan 2012 using a t-test
  tTest2 = t.test(x=lansJanTemps[,4], y=lansJanTemps[,2]);
  # compare the means from Jan 2014 and Jan 2013 using a t-test
  tTest3 = t.test(x=lansJanTemps[,4], y=lansJanTemps[,3]);
  # compare the means from Jan 2014 and Jan 2015 using a t-test
  tTest4 = t.test(x=lansJanTemps[,4], y=lansJanTemps[,5]);
  # compare the means from Jan 2014 and Jan 2016 using a t-test
  tTest5 = t.test(x=lansJanTemps[,4], y=lansJanTemps[,6]);
  
  # print out a summary of the t-test
  print(tTest1);  # most statistically similar (p=0.3393)
  print(tTest2);
  print(tTest3);
  print(tTest4);
  print(tTest5);  # 2nd most similar (p=0.09822)
  
  #### Do an ANOVA with all six years
  # Data -- a vector with temperature values from 2011 through 2016
  JanTemps = c(lansJanTemps[,1], lansJanTemps[,2],
               lansJanTemps[,3], lansJanTemps[,4],
               lansJanTemps[,5], lansJanTemps[,6]);
  
  # Categories -- a vector with 31 2011s through 2016s
  JanYears = c(rep(2011,31), rep(2012,31),
               rep(2013,31), rep(2014,31),
               rep(2015,31), rep(2016,31));
  
  # Combine the data and category vectors in a data frame
  JanDataFrame = data.frame(JanTemps, JanYears);
  
  # Make a boxplot for each year
  boxplot(JanTemps~JanYears, use.cols = TRUE,
          names = c(2011,2012,2013,2014,2015,2016),
          main = "January 2011 through January 2016",
          xlab = "Years",
          ylab = "Temperature (Fahrenheit)");
  
  # Performs a ANOVA (data: temperatures, category: years)
  JanAnova = aov(JanTemps~JanYears, data=JanDataFrame);
  
  # Summarize the second ANOVA in the Console Window
  cat("\nSecond ANOVA results:\n");
  print(summary(JanAnova));
  
  # test the normality assumption using a histogram
  hist(x=residuals(JanAnova));
}