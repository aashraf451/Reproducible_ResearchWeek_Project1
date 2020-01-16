##Reading Data
# Set Global Echo = On

# Load data
if (!file.exists("activity.csv") )
{
  dlurl <- 'http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
  download.file(dlurl,destfile='repdata%2Fdata%2Factivity.zip',mode='wb')  
  unzip('repdata%2Fdata%2Factivity.zip')
}

# Read data
data <- read.csv("activity.csv")  