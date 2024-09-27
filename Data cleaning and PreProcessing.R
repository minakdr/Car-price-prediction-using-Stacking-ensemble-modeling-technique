library(dbplyr)
df<- read.csv("D:\\Projects\\car price\\Car details v3.csv")
summary(df)

#part 1 : Pre processing and data cleaning 

#check for duplicated rows
duplicate_rows <- df[duplicated(df) | duplicated(df, fromLast = TRUE), ]
#remove duplicated rows 
df<-df[!duplicated(df),]

#We check the percentage of missing values per column
colMeans(is.na(df)) * 100  
df <- na.omit(df)

#the brand name may be usefull in our analysis , so I will extract it from the name
library(stringr)
df$car_brand_name <- str_extract(df$name, "^[^\\s]+")
#I will drop the name column 
df <- subset(df, select = -name)



#cathegoriacl values 
#numerization of columns
table(df$owner)
df$owner <-as.numeric(factor(df$owner)) #first owner:1 , #second owner: 2..., test drive:5
table(df$fuel)
df$fuel<-as.numeric(factor(df$fuel)) #diesel:1 , petrol:2, LPG:3 , CNG:4
table(df$transmission)
df$transmission<-as.numeric(factor(df$transmission)) #Manual:1 , Automatic:2
table(df$seller_type)
df$seller_type<-as.numeric(factor(df$seller_type)) #Individual:1 ,Dealer:2 ,Trustmark:3
table(df$car_brand_name)

#I create a mapping because there are so many cars and i wna to tkep track of them
car_brands <- c(
  "Ambassador" = 1,
  "Ashok" = 2,
  "Audi" = 3,
  "BMW" = 4,
  "Chevrolet" = 5,
  "Daewoo" = 6,
  "Datsun" = 7,
  "Fiat" = 8,
  "Force" = 9,
  "Ford" = 10,
  "Honda" = 11,
  "Hyundai" = 12,
  "Isuzu" = 13,
  "Jaguar" = 14,
  "Jeep" = 15,
  "Kia" = 16,
  "Land" = 17,
  "Lexus" = 18,
  "Mahindra" = 19,
  "Maruti" = 20,
  "Mercedes-Benz" = 21,
  "MG" = 22,
  "Mitsubishi" = 23,
  "Nissan" = 24,
  "Opel" = 25,
  "Renault" = 26,
  "Skoda" = 27,
  "Tata" = 28,
  "Toyota" = 29,
  "Volkswagen" = 30,
  "Volvo" = 31
)

df$car_brand_name <- car_brands[df$car_brand_name ]

#Now , we remove the unit of the relaining features

df$mileage<-as.numeric(str_extract(df$mileage, "\\d+" ))
df$engine<-as.numeric(str_extract(df$engine, "\\d+" ))
df$max_power<-as.numeric(str_extract( df$max_power, "\\d+"  ))

#we can extract 2 features from the torque column , torque value and revolution per minute (RPM)
df$torque<-str_replace(df$torque, "at","@")
df$torque_value<-str_extract( df$torque ,"^[^@]+")
df$rpm<-str_extract(df$torque, "(?<=@ ).*")

#we romve the torque column
df<-subset(df, select=-torque)

# Function to assign the correct torque unit
assign_torque_unit <- function(torque, rpm) {
  if (grepl("kgm", rpm)) {
    return(paste0(torque, " kgm"))
  } else {
    return(paste0(torque, " Nm"))
  }
}

df$torque_value <- mapply(assign_torque_unit, df$torque_value, df$rpm)


#Function to convert Nm to kgm to have harmonized data
convert_nm_to_kgm <- function(torque) {
  #check if torque is in kgm (case insensitive)
  if (grepl("kgm", torque, ignore.case = TRUE)) {
    return(torque)  # Return as is if it's already kgm
  }
  #check if torque is in nm (case insensitive)
  if (grepl("nm", torque, ignore.case = TRUE)) {
    #remove 'Nm' and convert to numeric
    torque_value <- as.numeric(gsub("nm", "", torque, ignore.case = TRUE))
    
    #convert to kgm
    torque_kgm <- torque_value * 0.10197
    
    return(paste0(round(torque_kgm, 2), " kgm"))  
  }
  
  return(NA)  # Return NA for unexpected formats
}

df$torque_value_in_kgm <- sapply(df$torque_value, convert_nm_to_kgm)

df<-subset(df , select = -torque_value)
#remove the unit from torque value
df$torque_value_in_kgm <- as.numeric(gsub("[^0-9\\.]", "", df$torque_value_in_kgm))
#if ther is any missing value in torque value , we will replace it using the mean of all the other values
mean_value <- mean(df$torque_value_in_kgm, na.rm = TRUE)
df$torque_value_in_kgm[is.na(df$torque_value_in_kgm)] <- mean_value



#we remove all the brakets in rpm to only keep the numbers
df$rpm<-str_replace( df$rpm, "\\(.*?\\)","")

#we only keep the singular values in rpm , and if there is two , we calculate and apply the mean 

handle_rpm_values <- function(rpm_value) {
  #Check if the value contains a range (e.g., "1500-3000rpm")
  if (grepl("-", rpm_value)) {
    #Extract the two numbers from the range and remove non-numeric characters like "rpm"
    values <- as.numeric(unlist(strsplit(gsub("[^0-9-]", "", rpm_value), "-")))
    
    #Calculate the mean of the two values
    mean_value <- mean(values, na.rm = TRUE)
    
    return(mean_value)  #Return the mean
  } else {
    #Remove non-numeric characters from singular values 
    return(as.numeric(gsub("[^0-9]", "", rpm_value)))
  }
}


df$rpm <- sapply(df$rpm, handle_rpm_values)
df <- na.omit(df)

#Part 2 : Visualization

cor_matrix<-cor(df)

#Interpretation : 
# Year and Selling Price:
#   correlation: 0.4297 (positive). This suggests that as the year increases, the selling price tends to increase, indicating newer models are generally sold for higher prices.
# Year and Mileage:
#   correlation: 0.3685 (positive). Newer vehicles tend to have higher mileage, possibly indicating they are more used.
# Selling Price and Max Power:
#   correlation: 0.6906 (strong positive). Higher max power is associated with higher selling prices, suggesting that more powerful vehicles are valued more.
# Fuel and Engine:
#   correlation: -0.5136 (negative). This indicates that certain types of fuel may be associated with lower engine power or efficiency.
# Torque Value and Max Power:
#   correlation: 0.5705 (positive). This indicates a strong relationship, suggesting that vehicles with higher torque tend to also have higher power.
# Owner and Selling Price:
#   correlation: -0.2183 (negative). This indicates that the number of previous owners may have a slight negative impact on selling price, which is common as vehicles with more owners might be perceived as less desirable.


library(reshape2)
library(ggplot2)

#melt the correlation matrix for ggplot ( make it long format which is more suiable for plotting)
melted_corr <- melt(cor_matrix)

#Plot the heatmap
ggplot(melted_corr, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 10, hjust = 1)) +
  coord_fixed()


#residual plot 

model <- lm(selling_price ~ ., data = df)

#create a data frame for residuals
residuals_df <- data.frame(
  fitted = fitted(model),
  residuals = residuals(model)
)

#create a residual plot
ggplot(residuals_df, aes(x = fitted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Residual Plot",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal()


#We save the clean data in a csv file 

write.csv(df, 'CleanedAndPreProcessedData.csv', row.names = FALSE)


#the columns car band name and rpm and torque value in km arent affecting the selling price , its better to remove them 
df<-subset(df , select = -rpm  )
df<-subset(df , select = -torque_value_in_kgm  )
df<-subset(df , select = -car_brand_name  )
df <- na.omit(df)


#We save the clean data in a csv file 

write.csv(df, 'CleanedAndPreProcessedData.csv', row.names = FALSE)




