{
  rm(list=ls()); options(show.error.locations = TRUE);  
  
  # Part A: Get the fish's age, weight, location, and gender from the user
  fishAge = readline("What is the fish's age? ");
  fishWeight = readline("What is the fish's weight? ");
  fishLocation = readline("What is the fish's location (north or south)? ");
  fishGender = readline("What is the fish's gender? ");

  # convert the age and weight from strings to numeric values
  fishAge = as.numeric(fishAge);
  fishWeight = as.numeric(fishWeight);
  

  # Part B: check if fish is between 5 aqnd 8 
  if(fishAge > 5 && < 8)  # error: must be explicit and include the variable
  {
    cat("fish is between 5 and 8\n");
  }
  
  # Part C: check if the fish's weight is between 50 and 150
  if(fishWeight > 50 && fishWeight < 150)
  {
    cat("fish weight is between 50 and 150\n");
  }
  
  # Part D: Give a message for the four possible location/gender combinations
  # and an error if the user entered something other than the 4 possibilities
  if(fishLocation == "north" && fishGender == "male")
  {
    cat("fish is male and from the north\n");
  }
  else if(fishLocation == "south" && fishGender == "male")
  {
    cat("fish is male and from the south\n");
  }
  else if(fishLocation == "north" && fishGender == "female")
  {
    cat("fish is female and from the north\n");
  }
  else if(fishLocation == "south" && fishGender == "female")
  {
    cat("fish is female and from the south\n");
  }
  else
  {
    cat("Invalid loaction or gender entered\n"); 
  }
  
  #Part E: 
  if( fishLocation == "north" || fishLocation == "south" && 
      fishWeight > 20 && fishWeight < 100 )
  {
    cat("fish is from the north or south and is between 20 and 100 grams\n");
  }
}

# line 23 gives the error: "unexpected '<'"
# because conditional operators like < compare two value, whereas
# there is no first value.  In English, this line works: 
# If fishAge is greater than 5 and less than 8
# But in programming it needs to be:
# If fishAge is greater than 5 and *fishAge is* less than 8