#installera paket
vignette(topic="pxweb")
install.packages('pxweb')

library(pxweb)
library(ggplot2)
#api kod för scb
pxweb_interactive("https://api.scb.se/OV0104/v1/doris/sv/ssd/")

#Bildata
# PXWEB query 
pxweb_query_list <- 
  list("Region"=c("00"),
       "Agarkategori"=c("000"),
       "ContentsCode"=c("TK1001AB"),
       "Tid"=c("2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/TK/TK1001/TK1001A/PersBilarA",
            query = pxweb_query_list)

# Convert to data.frame 
bil_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
#convert to numerical
str(bil_data)
bil_data$år <- as.numeric(bil_data$år)
# make plot
ggplot(bil_data, aes(x = år, y = `Personbilar i trafik`)) +
  geom_line()

#folk data
# PXWEB query 
pxweb_query_list <- 
  list("Alder"=c("tot"),
       "Kon"=c("1","2"),
       "ContentsCode"=c("0000053A"),
       "Tid"=c("2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"))

# Download data 
px_data <- 
  pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningR1860N",
            query = pxweb_query_list)

# Convert to data.frame 
folk_data <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")

# convert to numerical
str(folk_data)
folk_data$år <- as.numeric(folk_data$år)
# makep lot
ggplot(folk_data, aes(x = år, y = Folkmängd)) +
  geom_line()
