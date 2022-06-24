setwd("~/R/r-programming-course/rprog_data_specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  directory <- paste(getwd(), "/", directory, "/", sep = "")
  
  file_list <- list.files(directory)
  data <- data.frame()
  
  for(i in id){
    files_dir <- paste(directory, file_list[i], sep= "")
    files <- read.csv(files_dir)
    
    data <- rbind(data, files)
  }
  
  mean(data[[pollutant]], na.rm = TRUE)
}

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

#######################################################

complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names= TRUE)
  
  data <- data.frame()
  for (i in id) {
    files <- read.csv(files_list[i])
    
    nobs <-sum(complete.cases(files))
    data <-rbind(data, data.frame(i, nobs))
    
  }
  return(data)
  
}

complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)

#######################################################

corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names= TRUE)
  corr_vector <- numeric()
  
  for(i in 1:length(files_list)) {
    files <- read.csv(files_list[i])
    nobs <-sum(complete.cases(files))
    
    if(nobs > threshold) {
      dat <- files[complete.cases(files), ]
      
      corr_vector <- c(corr_vector, cor(dat$sulfate, dat$nitrate))
    }
  }
  
  corr_vector
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)

######################################

# Quiz

pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "sulfate", 34)
pollutantmean("specdata", "nitrate")

cc <- complete("specdata", c(6, 10, 20, 34, 100, 200, 310))
print(cc$nobs)

cc <- complete("specdata", 54)
print(cc$nobs)

RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

cr <- corr("specdata")                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)

cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))