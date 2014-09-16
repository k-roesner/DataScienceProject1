Sys.setenv(JAVA_HOME="")
options(java.parameters="-Xmx2g")
library(rJava)

# Output Java version
.jinit()
print(.jcall("java/lang/System", "S", "getProperty", "java.version"))

# Load RJDBC library
library(RJDBC)

jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="C:/Program Files/Java/jdk1.7.0_67/ojdbc6.jar")

possibleError <- tryCatch(
  # Establishing the connection
  jdbcConnection <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@128.83.138.158:1521:orcl", "C##cs347_mas5774", "orcl_mas5774"),
  error=function(e) e
)
if(!inherits(possibleError, "error")){
  
  #Get some specific subsets of the data for some of the plots
  tsunami_subset <- dbGetQuery(jdbcConnection, "select * from (select country, count(country) as \"COUNT\" from tsunami_runup_data group by country) where \"COUNT\" > 100")
  dbDisconnect(jdbcConnection)
}

# Correlation plot for seismic data

GGally::ggpairs(seismic_data[,c(4,5,7)], title = "Correlation Plots for Seismic Data")

# Earthquake losses scatter plot, region vs. average annual loss

library(ggplot2)
ggplot(data = earthquake_losses, aes(x = REGION, y = AVERAGE_ANNUAL_LOSS)) + geom_point() + ylab("Average Annual Loss") + ggtitle("Average Annual Loss Due to Earthquakes by Region")

# Earthquake losses boxplot, region vs. average annual loss without Japan
# Remove Japan from the data set, since it is such an extreme outlier
earthquake_losses_no_japan <- earthquake_losses[-96,]
ggplot(data = earthquake_losses_no_japan, aes(x = REGION, y = AVERAGE_ANNUAL_LOSS)) + geom_boxplot() + ylab("Average Annual Loss") + ggtitle("Average Annual Loss Excluding Japan")

# Histogram of earthquake magnitudes

ggplot(data = seismic_data, aes(x = MAGNITUDE)) + geom_histogram() + ggtitle("Distribution of Earthquake Magnitudes")

# Volcano data scatter plot, VEI (Volcano Explosivity Index) vs. number of deaths

ggplot(data = volcano_data, aes(x = VEI, y = NUM_DEATHS)) + geom_point() + ylab("Number of Deaths") + xlab("Volcano Explosivity Index") + ggtitle("VEI vs. Number of Deaths")

# Histogram of the VEI of eruptions

ggplot(data = volcano_data, aes(x = VEI)) + geom_histogram(binwidth = 1) + xlab("Volcano Explosivity Index") + ggtitle("Distribution of VEI")

# Histogram of countries where at least 100 tsunamis have hit

ggplot(data = tsunami_subset, aes(x = COUNTRY, y = COUNT, fill = COUNTRY)) + geom_histogram(stat = "identity") + coord_flip() + xlab("Number of Tsunamis") + ggtitle("Nations With More Than 100 Tsunamis")
