#Lets load the libraries
library(ggplot2)
library(waffle)
library(magrittr)
library(tidyverse)
library(lessR)
library(utils)
library(scales)
library(rnaturalearth)
library(sf)
library(ggrepel)
library(rgeos)

#Lets read the files
c <- Read("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")
f <- Read("http://web.pdx.edu/~gerbing/521/data/covid19.xlsx")
W <- Read("C:\Users\vsanj\OneDrive\Documents\GIT\Covid_Viz_usingR\Dataset\worldcities.csv")

# Look at the first ten rows. From the output of Read(), we will only focus on a subset of variables. Read () also provides the column indices to identify the variables.

details(c)

#ggplot2 wants only data in long format. Is the data in wide format or long format? Do we need to transform the data? Just examine the variables in columns 1, 5, 6, and 8 through 10

c1 <- c[,c(1,5,6,8,9,10)] 
head(c1)

# Subset the data table to include only data for USA, Italy, and Spain. The corresponding two-character country codes, the variable geoId, are US, IT and ES. Just retain the variables in columns 1, 5, 6, and 8 through 10.

c2 <- c1[c1$geoId=='US' | c1$geoId=='IT' | c1$geoId=='ES',]

#Convert the character string date to an R variable type Date.

dates <- as.Date(c2$dateRep,format="%d/%m/%Y")
c2$dateRep <- dates

#Invoke the base R range() function on d$dateRep to verify the range of dates. What are the beginning and ending dates? 

range(c2$dateRep)

#Plot the time series of deaths just for the USA for i) lessR and ii) ggplot2. First form a separate data frame, sub-setting again, but leave d alone as we will use that also.

#Using lessR

Us <- c2[c2$geoId=='US',]
Plot(data = Us,dateRep,deaths, fill="steelblue2", color="steelblue3")

#Using ggplot2

ggplot(Us, aes(dateRep, deaths)) + geom_line() +
  scale_x_date(labels=date_format("%b-%Y"))


#Get a Trellis plot for the time series of death for USA, Italy, and Spain for i) lessR and ii) ggplot2. Interpret and compare deaths across the three countries.

#Using lessR

Plot(data = c2,dateRep,deaths,by1 =geoId)

#Using ggplot2

ggplot(c2, aes(dateRep, deaths, fill=geoId)) +
  geom_area() +
  facet_grid(rows=vars(geoId)) +
  ggtitle("Deaths for different counrties")


#Add some customization to the lessR Trellis time series plot. Add a black background and choose border and fill colors of your own, different from the examples. To help choose the colors, use base R colors() for all the named colors, or, examine them with lessR showColors()

style(sub_theme="black", trans=.55,
      window_fill="gray10", grid_color="gray25")
Plot(dateRep, deaths, by1=geoId, data=c2, n.col=1, 
     fill="darkred", color="red", 
     main="Deaths of Countries")
style()

#Map generation

BarChart(data=f,State,Cases,sort = "+",horiz = TRUE)


#Display a sorted bar chart of deaths per 1 million by State. Label the numeric ($y$) axis

f_dea1 <- f %>% group_by(State) %>% summarize(Deaths=Deaths1M)
names(f_dea1)[2] <- "Death_per_million"
BarChart(data = f_dea1,State,Death_per_million,sort = "+",horiz = TRUE)
f_dea1$`0` <- NULL

#Are deaths proportional to the number of cases in each state? To answer, plot cases against deaths per million. Create a lessR enhanced scatterplot. Also, label the outliers by the name of their corresponding state by adding ID="State" as a parameter and value to the function call.

f_dea2 <- f_dea1
Cases <-  f$Cases
f_dea3<- cbind(f_dea2,Cases)

#Now that we have binned the Death per million values and the number of cases, let us see if we can plot a scatterplot.

Plot(Death_per_million,Cases,data = f_dea3,enhance=TRUE,ID="State")
dc <- f_dea3

#Not necessary, but convenient. The worldcities data lists the state as admin_name. Change the variable name to state. One way to do this is directly in your downloaded Excel data file. The other is with R after reading the data. Your choice. The R directions are listed here. The vector names(d) lists the names of the d data frame. We want to change one of them. We need the number of the column that is labeled admin_name. To get that column number, use the base R which() function. Then refer to that specific column number in the names vector and assign a new name.

names(W)[which(names(W) == "admin_name")] <- "state"

#Now reduce the size of d by just extracting the rows of data that list cities in the USA. Do not need all the variables, so just retain the columns: city, lat, lng, iso3, state, population. Then list the first 10 rows just to check what you have left.

W2 <- W[W$country=="United States",c(1,3,4,7,8,10)]

#Maybe the best way to display on a map is to center the bubble that represents density of COVID-19 in that state. Certainly possible, but here pursue a simpler method. Display the bubble over the city in each state with the largest population. Probably best to do this at the county level, but here we have a rougher approximation at the state level.

max <- W2 %>% group_by(state) %>% filter(population==max(population)) # We can skip the next step because we already have lat and lon
ggplot(max, aes(lng, lat)) + geom_point(aes(size=population))

#Now we are ready for the main merge, to combine COVID-19 information from the dc data frame, and state and city location from the dm data frame. First check out to see what you are going to merge by listing the first five lines of each data frame

max[1:5,]

dc[1:5,]


#Now let's merge both the data frames

final <- merge(dc,max,by.x = "State",by.y = "state")


#Use the rnaturalearth function ne_states() to get the mapping polygon information in simple features format for the states of country: United States of America. Save the information into the data frame d_us. Take a look at the first row of data. Easier, but optional, to transpose the result with the base R t() function to display the contents vertically.

d_us <- ne_states(country='united states of america')

# Now convert the information in the d_us data frame into a simple features version of a data frame with the sf package function st_as_sf(). Again, display the first row. 

st_as_sf(d_us,quiet=TRUE)
d_us[1,]

# Generate the map of the USA with a circle over the largest city of each state that indicates the amount of COVID-19 deaths per 1 million residents. Do not add any labels here.

ggplot(final,aes(lng,lat)) + geom_point(aes(size=population))

#Let's load map data

US <- map_data("usa")

#Plotting the map

ggplot() + geom_polygon(data=US,aes(long,lat,group=group))


#Let's add the city/population layer

ggplot()+ geom_polygon(data=US,aes(long,lat,group=group),color="darkred",fill="powderblue") + geom_point(data=final,aes(lng,lat,size=population),color="blue",fill="blue")


#now let's try with a more effective method 

usa <- ne_states(country = "united states of america",returnclass = "sf")
citis <- st_as_sf(final,coords=c("lng","lat"),crs=st_crs(d_us),remove=FALSE)

#Finally, focus in on the Northeast. Adjust xlim() and ylim() to focus on just the NorthEast section of the USA. Make the bubbles even larger. Unfortunately, drop the labels because they do not work with the kludge, as all labels show up, even those not in this sub-section. 

options(scipen=10000)
map_fill <- "#ada49d"
map_border <- "#753234"
city_circle <- "#38160e"
city_label <- "#38160e"
ggplot() + geom_sf(data=usa,fill=map_fill,color=map_border,size=0.2) + geom_sf(aes(size=population),data=citis,alpha=.7,color=city_circle) + scale_size_area(max_size = 25) + theme_set(theme_bw()) + labs(x=NULL,y=NULL) +scale_x_continuous(limits = c(-125, -67))+
  scale_y_continuous(limits = c(25, 50)) + ggtitle("COVID-19 density plot")

