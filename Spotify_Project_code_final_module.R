
################################################## Data preprocessing ############################################################################################################


data.preprocessing<-function(){

  
# importing the spotify dataset.

setwd=("C:/Users/pablo/OneDrive/Desktop/OU/Classes/Senior Year/MIT 5032/Group Project")
spotify.data<-read.csv('spotify.data.preprocessed.csv',header = TRUE,sep=',',encoding = "UTF-8")
dim(spotify.data)


# storing the dataset in a dataframe called spotify.data

#spotify.data.for.preprocessing<-read.csv("Spotify_Partially_Processed_Data.csv",header = TRUE,sep = ",",encoding = "UTF-8")

# checking the dimensions of the column to verify the correct loading of the dataset.
dim(spotify.data.for.preprocessing)

#iterating through value in the Country column to check if its an empty string to replace it to "Global"
for (i in 1:nrow(spotify.data.for.preprocessing)) {
  if (spotify.data.for.preprocessing$Country[i] == "") {
    spotify.data.for.preprocessing$Country[i] <- "Global"
  }
}


# Creating new column for country full name 

# mapping country names with full country names.
country_mapping <- data.frame(
  country = c("Global", "ZA", "VN", "VE", "UY", "US", "UA", "TW", "TR", "TH", "SV", "SK", "SG", "SE", "SA", "RO", "PY", "PT", "PL", "PK", "PH", "PE", "PA", "NZ", "NO", "NL", "NI", "NG", "MY", "MX", "MA", "LV", "LU", "LT", "KZ", "KR", "JP", "IT", "IS", "IN", "IL", "IE", "ID", "HU", "HN", "HK", "GT", "GR", "GB", "FR", "FI", "ES", "EG", "EE", "EC", "DO", "DK", "DE", "CZ", "CR", "CO", "CL", "CH", "CA", "BY", "BR", "BO", "BG", "BE", "AU", "AT", "AR", "AE"),
  country_full_name = c("Global", "South Africa", "Vietnam", "Venezuela", "Uruguay", "United States", "Ukraine", "Taiwan", "Turkey", "Thailand", "El Salvador", "Slovakia", "Singapore", "Sweden", "Saudi Arabia", "Romania", "Paraguay", "Portugal", "Poland", "Pakistan", "Philippines", "Peru", "Panama", "New Zealand", "Norway", "Netherlands", "Nicaragua", "Nigeria", "Malaysia", "Mexico", "Morocco", "Latvia", "Luxembourg", "Lithuania", "Kazakhstan", "South Korea", "Japan", "Italy", "Iceland", "India", "Israel", "Ireland", "Indonesia", "Hungary", "Honduras", "Hong Kong", "Guatemala", "Greece", "United Kingdom", "France", "Finland", "Spain", "Egypt", "Estonia", "Ecuador", "Dominican Republic", "Denmark", "Germany", "Czech Republic", "Costa Rica", "Colombia", "Chile", "Switzerland", "Canada", "Belarus", "Brazil", "Bolivia", "Bulgaria", "Belgium", "Australia", "Austria", "Argentina", "United Arab Emirates")
)


# creating a new column 'country_full_name' in our dataset with full country names for corresponding country codes.
spotify.data.for.preprocessing$country_full_name<-country_mapping$country_full_name[match(spotify.data.for.preprocessing$Country, country_mapping$country)]



# Creating new column for continent 

#creating a datframe for the two vectors country_names and continents to map them

continent_mapping <- data.frame(
  country_names <- c("Global", "South Africa", "Vietnam", "Venezuela", "Uruguay", "United States", "Ukraine", "Taiwan", "Turkey", "Thailand", "El Salvador", "Slovakia", "Singapore", "Sweden", "Saudi Arabia", "Romania", "Paraguay", "Portugal", "Poland", "Pakistan", "Philippines", "Peru", "Panama", "New Zealand", "Norway", "Netherlands", "Nicaragua", "Nigeria", "Malaysia", "Mexico", "Morocco", "Latvia", "Luxembourg", "Lithuania", "Kazakhstan", "South Korea", "Japan", "Italy", "Iceland", "India", "Israel", "Ireland", "Indonesia", "Hungary", "Honduras", "Hong Kong", "Guatemala", "Greece", "United Kingdom", "France", "Finland", "Spain", "Egypt", "Estonia", "Ecuador", "Dominican Republic", "Denmark", "Germany", "Czech Republic", "Costa Rica", "Colombia", "Chile", "Switzerland", "Canada", "Belarus", "Brazil", "Bolivia", "Bulgaria", "Belgium", "Australia", "Austria", "Argentina", "United Arab Emirates"),
  continents <- c("Global", "Africa", "Asia", "South America", "South America", "North America", "Europe", "Asia", "Europe", "Asia", "North America", "Europe", "Asia", "Europe", "Asia", "Europe", "South America", "Europe", "Europe", "Asia", "Asia", "South America", "North America", "Oceania", "Europe", "Europe", "North America", "Africa", "Asia", "North America", "Africa", "Europe", "Europe", "Europe", "Asia", "Asia", "Asia", "Europe", "Europe", "Asia", "Asia", "Europe", "Asia", "Europe", "North America", "Asia", "North America", "Europe", "Europe", "Europe", "Europe", "Europe", "Africa", "Europe", "South America", "North America", "Europe", "Europe", "Europe", "North America", "South America", "Europe", "Europe", "North America", "Europe", "South America", "South America", "Europe", "Europe",  "Oceania", "Europe", "Europe", "Asia"  )
)

# creating a new column 'Continent' in our dataset with Continent names for corresponding country names.
spotify.data.for.preprocessing$Continent<-continent_mapping$continents[match(spotify.data.for.preprocessing$country_full_name, continent_mapping$country_names)]



# Creating a new datset with required columns only


#creating a vector with the columns that are required for our analysis.
analysis.cols<-c("Spotify_ID" , "Name", "Popularity" , "Artists"  ,  "Daily_Rank"  ,  "Country" , "country_full_name" , "Continent", "Snapshot_Date"   , "Is_Explicit"    ,    "Duration_MS"   ,  "Album_Name" ,  "Album_Release_Date"  , "Album_Release_Season",  "Energy"  , "Key" ,   "Mode" , "Tempo" )

#creating a new dataset with only the columns that are required for our analysis.
spotify.data.preprocessed<-spotify.data.for.preprocessing[ ,analysis.cols]


# Exporting to excel 

# Installing the 'openxlsx' package to export data to an excel

install.packages("openxlsx")

# Loading the 'openxlsx' package
library(openxlsx)

# writing the data to an excel file.

write.xlsx(spotify.data.for.preprocessing, "spotify.data.preprocessed.xlsx") 

}

############################################ Data preprocessing ##################################################################################################

# Install ggplot
install.packages("ggplot2")
library(ggplot2)

# Loading the pre-processed spotify dataset 

setwd("C:/Users/vishal/OneDrive/Desktop/OU/Classes/Senior Year/MIT 5032/Group Project")
getwd()
spotify.data<-read.csv('spotify_data_for_analysis.csv',header = TRUE,sep=',',encoding = "UTF-8")
dim(spotify.data)

######################################################## Hypothesis -1  #########################################################################  

  
# converting the duration from milliseconds to minutes.
duration.min <- (spotify.data$Duration_MS/1000)/60

## Subsetting by Continent
## Duration
global.duration.min <- subset(duration.min, spotify.data$Continent == "Global")
africa.duration.min <- subset(duration.min, spotify.data$Continent == "Africa")
asia.duration.min <- subset(duration.min, spotify.data$Continent == "Asia")
south.america.duration.min <- subset(duration.min, spotify.data$Continent == "South America")
north.america.duration.min <- subset(duration.min, spotify.data$Continent == "North America")
europe.duration.min <- subset(duration.min, spotify.data$Continent == "Europe")
oceania.duration.min <- subset(duration.min, spotify.data$Continent == "Oceania")
## Popularity
global.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "Global")
africa.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "Africa")
asia.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "Asia")
south.america.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "South America")
north.america.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "North America")
europe.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "Europe")
oceania.popularity <- subset(spotify.data$Popularity, spotify.data$Continent == "Oceania")


# Hypothesis -1 Visualization 


## Boxplot
ggplot(spotify.data, aes(x=Continent, y=duration.min)) + geom_boxplot() + ylab("Song Duration (in min)") + ggtitle("Song Duration by Continent")

## Histogram
qplot(global.duration.min, binwidth = .5, fill = factor(global.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "Global\nFilled by Popularity") + xlim(c(0,11))
qplot(africa.duration.min, binwidth = .5, fill = factor(africa.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "Africa\nFilled by Popularity") + xlim(c(0,11))
qplot(asia.duration.min, binwidth = .5, fill = factor(asia.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "Asia\nFilled by Popularity") + xlim(c(0,11))
qplot(south.america.duration.min, binwidth = .5, fill = factor(south.america.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "South America\nFilled by Popularity") + xlim(c(0,11))
qplot(north.america.duration.min, binwidth = .5, fill = factor(north.america.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "North America\nFilled by Popularity") + xlim(c(0,11))
qplot(europe.duration.min, binwidth = .5, fill = factor(europe.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "Europe\nFilled by Popularity") + xlim(c(0,11))
qplot(oceania.duration.min, binwidth = .5, fill = factor(oceania.popularity)) + xlab("Song Duration (in min)") + ylab("Number of Songs") + ggtitle("Distribution of Song Duration", subtitle = "Oceania\nFilled by Popularity") + xlim(c(0,11))

## Scatter plot
ggplot(spotify.data, aes(x = duration.min, y = Popularity)) + geom_point(aes(col= Continent)) + xlab("Song Duration (in min)") + ggtitle("Duration Vs Popularity")



######################################################## Hypothesis - 1  #################################################################################  



##################################################### Hypothesis - 2   #######################################################################################################

  
## In North America, songs released in the winter season have a higher popularity rating because of Christmas.

#create an aggregate of the popularity by season and continent
popularity.season.continent.aggregate <- aggregate(spotify.data$Popularity,by=list(spotify.data$Album_Release_Season,spotify.data$Continent),FUN=mean)
popularity.season.continent.aggregate

#create an aggregate of popularity only by season
popularity.by.season.aggregate <- aggregate(spotify.data$Popularity,by=list(spotify.data$Album_Release_Season),FUN=mean)
popularity.by.season.aggregate

#rename the column headers
names(popularity.season.continent.aggregate)<- c("Album_Release_Season", "Continent", "Mean_Popularity")
print(popularity.season.continent.aggregate)



# Filter data for specific continents
hyp2.selected.continents <- c("North America", "South America", "Africa", "Asia","Europe")
hyp2.filtered.data <- popularity.season.continent.aggregate[popularity.season.continent.aggregate$Continent %in% hyp2.selected.continents, ]



# Hypothesis - 2 Visualization 

# Create a stacked bar plot with continents on the x-axis and seasons as fill
ggplot(spotify.data, aes(x = Continent, fill = Album_Release_Season)) +
  geom_bar() +
  labs(title = "Song Popularity by Continent and Season",
       x = "Continent",
       y = "Popularity",
       fill = "Release Season") +
  theme_minimal()+
  scale_fill_manual(values = c("Spring" = "#073B3A", "Summer" = "#C3F550", "Autumn" = "#8ABC40", "Winter" = "#B7E892")) +  # Variations of green
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),plot.title = element_text(hjust = 0.5))



##################################################### Hypothesis - 2  #######################################################################################################


############################################################# Hypothesis - 3  ###########################################################################################################

# Extracting the subset of the dataset needed for Hypothesis 3
spotify.data.for.explicity <- subset(spotify.data, Is_Explicit == 'TRUE', select = c('Continent', 'country_full_name', 'Is_Explicit') )

# Checking the dimensions of the data subset
dim(spotify.data.for.explicity)

# Counting the number of explicit songs in the top 50 spots for each country
explicit.songs.count.by.country <- aggregate(spotify.data.for.explicity$Is_Explicit,  by = list(spotify.data.for.explicity$Continent, spotify.data.for.explicity$country_full_name),FUN = length)

# Renaming column names
colnames(explicit.songs.count.by.country) <- c("Continent", "Country", "No.of.explicit.songs")

# Counting the number of explicit songs by continent
explicit.songs.count.by.continent <- aggregate(explicit.songs.count.by.country$No.of.explicit.songs, by = list(explicit.songs.count.by.country$Continent), FUN = sum)

# Renaming column names
colnames(explicit.songs.count.by.continent) <- c('Continent', 'No.of.explicit.songs')

# Counting the number of countries in the dataset for each continent
countries.count.by.continent <- aggregate(Country ~ Continent + Country, data = explicit.songs.count.by.country, FUN = length)

# Renaming column names
colnames(countries.count.by.continent) <- c('Continent', 'No.of.Countries')

# Joining the two data frames to get both the number of explicit songs and the number of countries of a continent in a single data frame
explicit.songs.and.countries.count <- merge(explicit.songs.count.by.continent, countries.count.by.continent)

# Calculating the average number of explicit songs per country by continent
explicit.songs.and.countries.count$Avg <- ceiling(explicit.songs.and.countries.count$No.of.explicit.songs / (17 * explicit.songs.and.countries.count$No.of.Countries))

explicit.songs.and.countries.count.without.global<-explicit.songs.and.countries.count[explicit.songs.and.countries.count$Continent !='Global',]

# Calculating the median popularity score of explicit songs by continent
spotify.data.popularity <- subset(spotify.data, Is_Explicit == TRUE, select = c('Popularity', 'Continent'))

median.popularity.by.continent <- aggregate(Popularity ~ Continent, data = spotify.data.popularity, FUN = median)

median.popularity.by.continent <- median.popularity.by.continent[median.popularity.by.continent$Continent != 'Global',]

# Removing the row with the Global continent
spotify.data.popularity.without.global <- spotify.data.popularity[spotify.data.popularity$Continent != 'Global',]


# Hypothesis - 3  Visualization

# Bar plot for the average number of explicit songs by continent
ggplot(explicit.songs.and.countries.count.without.global, aes(x = reorder(Continent, -Avg), y = Avg, fill = Avg)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Avg), position = position_stack(vjust = 0.7), size = 5, fontface = "bold") +
  labs(title = "Average Number of Explicit Songs in top 50 spots by Continent", x = "Continent", y = "Songs Count") +
  theme_minimal() + ylim(0, 50) + theme(plot.title = element_text(hjust = 0.5)) + scale_fill_gradient(low = "lightcoral", high = "darkred")

# Box plot to visualize the median value of explicit songs' popularity by continent
ggplot(spotify.data.popularity.without.global, aes(x = Continent, y = Popularity, fill = Continent)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Popularity Scores of Explicit Songs Across Continents", x = "Continent", y = "Popularity Score") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))



############################################################# Hypothesis - 3  ###########################################################################################################




############################################################# Hypothesis - 4 ############################################################################


####### Part 1: Energy is correlated with tempo and key as people listen to music to uplift their mood. 

# Conclusion: The first graph portrays that Major songs are slightly more common for higher energy scores, while 
# minor songs are slightly ore common for less energetic scores. The second graph shows a small positive linear relationship. 
# between tempo and energy. Correlation coefficient is merely 0.1, meaning that as tempo increases, so does the perceived energy of the song


# Frequency of songs per energy level for minor and major songs:
breaks <- seq(0, 1, by = 0.05)

major_hist <- hist(spotify.data$Energy[spotify.data$Mode == 1], breaks = breaks, plot = FALSE)
minor_hist <- hist(spotify.data$Energy[spotify.data$Mode == 0], breaks = breaks, plot = FALSE)

# Hypothesis - 4 Visualization

plot(major_hist$mids, major_hist$counts, type = "l", col = "red", xlim = c(0, 1), ylim = c(0, max(major_hist$counts, minor_hist$counts)),
     xlab = "Energy", ylab = "Frequency",
     main = "Frequency Distribution - Major vs Minor Songs")
lines(minor_hist$mids, minor_hist$counts, col = "blue")
legend("topright", legend = c("Major", "Minor"), col = c("red", "blue"), lty = 1, cex = 0.8)



# Relationship between tempo and energy: 

ggplot(data = spotify.data, aes(x = Tempo, y = Energy)) +
  geom_point() +  # Add points for the scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add linear regression line
  labs(title = "Relationship between Energy and Tempo",
       x = "Tempo",
       y = "Energy") +
  theme_minimal()  

cor(spotify.data$Tempo, spotify.data$Energy) # finding correlation coefficient between variables




###### Part 2: More energetic songs will be more popular

# Conclusion: Since the correlation coefficient is 0.006, and as we can see by the scatter plot and line, there is not an 
# existing relationship between song energy and popularity, disproving our hypothesis. This therefore means, that songs 
# that do not score high on energy are equally as successful (or not successful) as energetic songs under this dataset's data points.
# Besides, the second graph shows that both major and minor songs are equally as popular, meaning that the variable mode is not relevant
# when composing a song.


ggplot(data = spotify.data, aes(x = Energy, y = Popularity)) +
  geom_point() +  
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Relationship between Energy and Popularity",
       x = "Energy",
       y = "Popularity") +
  theme_minimal()

cor(spotify.data$Energy,spotify.data$Popularity) # finding correlation coefficient between variables



avg_popularity <- aggregate(Popularity ~ Mode, data = spotify.data, FUN = mean)

avg_popularity$Mode_Label <- ifelse(avg_popularity$Mode == 0, "Minor", "Major")

ggplot(data = avg_popularity, aes(x = Mode_Label, y = Popularity, fill = Mode_Label)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  geom_text(aes(label = round(Popularity, 2)),
            position = position_dodge(width = 0.5),
            vjust = -0.5, size = 4, color = "black") +
  labs(title = "Average Popularity of Major vs Minor Songs",
       x = "Mode",
       y = "Average Popularity",
       fill = "Mode") +
  scale_fill_manual(values = c("red", "blue")) +
  theme_minimal() +
  theme(legend.position = "top")



################################################################## Hypothesis - 4 ############################################################################



# data.preprocessing()               
