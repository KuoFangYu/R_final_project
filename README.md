＃R_final_project
#薪資變化
#資料來源 https://data.gov.tw/dataset/9634

install.packages("jsonlite")
install.packages("ggplot2")
library(ggplot2)
library(jsonlite)

url <- "https://quality.data.gov.tw/dq_download_json.php?nid=9634&md5_url=7b41ec11a497a37184b82402be86eda5"   # get the URL

adata <- as.data.frame( fromJSON(url) )   # Convert to dataframe
cat("please input Occupation's number")   
t <- colnames(adata)
t[2:21]    # Occupation code is here

ysalary <- function( year1, year2, occ1, occ2) {    # input years and the Occupation code
  if ( occ1 <= 20 && occ2 <= 20 && occ1 >= 0 && occ2 >= 0 &&
       year1 <= 2019 && year2 <= 2019 && year1 >= 1980 && year2 >= 1980 ) {
    if (year1 != year2) {
      f <- (year1 - 1980)*13+1   # get the salary position in datafarme
      s <- (year2 - 1980)*13+1   
      iyear <- c()       
      while( f<=s ){              # get the year data 
        temp <- f
        iyear <- union(iyear,temp)
        f <- f+13
      }  #while
    }  #if
    else 
      iyear <- (year1 - 1980)*13+1
  
    iyear <- as.numeric(iyear)
    
    if ( occ1 == occ2 ) {                 # get salary
      salary <- c(adata[iyear,occ1+1])
      salary <- as.numeric(salary)
    }
    else {
      salary2 <- c(adata[iyear,occ2+1])     # get salary
      salary2 <- as.numeric(salary2)
    
      salary <- c(adata[iyear,occ1+1])
      salary <- as.numeric(salary)
    }
  
    if ( occ1 != occ2 ) {
      nyear <- (length(t<-c(year1:year2)))*2   # get year length
      Year <- c(1:nyear)     # advance set the size 
      type <- c(1:nyear)
      Salary <- c(1:nyear)   
      tyear1 <- year1    
      j <- 1
      i <- 1
     
       while(j <= nyear ){    # set datafarme year,salary,type and write it in a specific way
        Year[j] <- tyear1
        Year[j+1] <- tyear1
        type[j] <- occ1 
        type[j+1] <- occ2
        Salary[j] <- salary[i]
        Salary[j+1] <- salary2[i]
        j <- j +2
        i <- i +1
        tyear1 <- tyear1 +1
      }
      Year <- unname(Year)       # i don't need name
      type <- unname(type)  
      Salary <- unname(Salary)
  
      a <- data.frame(Year,Salary,type)   # dataframe for the image
      
      a$type <- factor(a$type)
      ggplot(data = a, aes(x=Year, y=Salary, color = type )) +    # draw a image
        geom_point(size = 2)
    }
    else if ( occ1 == occ2 ) {  
      a <- data.frame(Year=c(year1:year2),Salary= salary,type = occ1)  #  dataframe for the image
      
      a$type <- factor(a$type)
      ggplot(data = a, aes(x=Year, y=Salary, color = type )) +         # draw a image
        geom_point(size = 2)
    
    }
  }
  else 
    cat("you put a wrong input")
}



ysalary(1980,2000,20,3)
ysalary(1980,2019,2,15)
ysalary(1980,2019,4,13)
