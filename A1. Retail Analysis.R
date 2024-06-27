#' Retail Case A1 
#' Purpose: Assignment Submission
#' Talisha Thakshana Rajalakshmi
#' Jan 22, 2024

# Library
library(ggplot2)
library(dplyr)
library(data.table)
library(lubridate)

# Working Directory
setwd("~/Downloads/Hult_Visualizing_Analyzing_Data_with_R-main/personalFiles")

# Loading Data & Viewing
data <- read.csv("2024-01-23_sampled_590000_rows_a1_EDA_case.csv")
head(data)

# 1. Sales trends across time - seasons, months, weeks, weekends, etc.

#(A)

# Converting the Date column
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Aggregating monthly sales
monthly_sales <- data %>%
  group_by(Month) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop')

# Displaying the first few rows
head(monthly_sales)

# Plotting
ggplot(monthly_sales, aes(x = Month, y = Total_Sales)) +
  geom_line(color = "#8B0000", size = 1) + 
  theme_minimal() +
  theme(
    text = element_text(face = "bold", color = "black"), 
    axis.title = element_text(face = "bold", size = 12), 
    axis.text = element_text(face = "bold", size = 10), 
    plot.title = element_text(face = "bold", size = 14), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Monthly Sales Trends", x = "Month", y = "Total Sales")

# (B)

# Converting the 'Date' column to a Date object
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Extracting Year and Month from the Date using lubridate
data <- data %>%
  mutate(Year = year(Date), Month = month(Date))

# Aggregating sales data by Year and Month
sales_trends <- data %>%
  group_by(Year, Month) %>%
  summarize(TotalSales = sum(Sale..Dollars.), .groups = 'drop')

# Plotting sales trends over time
ggplot(sales_trends, aes(x = Month, y = TotalSales, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(title = "Sales Trends Over Time", x = "Month", y = "Total Sales in Dollars") +
  facet_wrap(~Year, scales = "free_y") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold", color = "black"), # Make all text bold
    axis.title = element_text(face = "bold", size = 12), # Bold axis titles
    axis.text = element_text(face = "bold", size = 10), # Bold axis text
    plot.title = element_text(face = "bold", size = 14), # Bold plot title
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

#(C)

# Converting the 'Date' column to a Date object
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Define Seasons based on Month
data$Month <- as.integer(format(data$Date, "%m"))
data$Season <- ifelse(data$Month %in% c(12, 1, 2), "Winter",
                      ifelse(data$Month %in% c(3, 4, 5), "Spring",
                             ifelse(data$Month %in% c(6, 7, 8), "Summer", "Fall")))

# Convert Season to a factor and order it
data$Season <- factor(data$Season, levels = c("Winter", "Spring", "Summer", "Fall"))

# Dark red palette for the seasons
dark_red_palette <- c("Winter" = "#8B0000", "Spring" = "#A52A2A", "Summer" = "#B22222", "Fall" = "#DC143C")

# Aggregating seasonal sales
seasonal_sales <- data %>%
  group_by(Season) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop')

# Plotting Seasonal sales trend
ggplot(seasonal_sales, aes(x = Season, y = Total_Sales, fill = Season)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = dark_red_palette) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold", color = "black"),
    axis.title = element_text(face = "bold", size = 12),
    axis.text = element_text(face = "bold", size = 10),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(title = "Seasonal Sales Trends", x = "Season", y = "Total Sales")

#(D)

# Converting the Date column
data$Date <- as.Date(data$Date, format = "%Y-%m-%d")

# Day_of_Week variable
data$Day_of_Week <- weekdays(data$Date)

# Ordering the days of the week
data$Day_of_Week <- factor(data$Day_of_Week, 
                           levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Updating weekly_sales
weekly_sales <- data %>%
  group_by(Day_of_Week) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop')

# Color palette
dark_red_palette <- c("#8B0000", "#A52A2A", "#B22222", "#DC143C", "#FF0000", "#FF6347", "#FF7F50")

# Plotting Weekly sales trend
ggplot(weekly_sales, aes(x = Day_of_Week, y = Total_Sales, fill = Day_of_Week)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = dark_red_palette) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.title = element_text(face = "bold", size = 12, margin = margin(t = 10, b = 10)),
    axis.text = element_text(face = "bold", size = 10),
    plot.title = element_text(face = "bold", size = 14),
    legend.title = element_text(face = "bold"),
    legend.text = element_text(face = "bold"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    panel.background = element_rect(fill = "white", colour = "white") 
  ) +
  labs(title = "Weekly Sales Trends", x = "Day of the Week", y = "Total Sales")


# 2. Dominant store locations and their performance characteristics

# My Analysis 

# Aggregating data by Store Number and Store Name
store_sales <- data %>%
  group_by(Store.Number, Store.Name) %>%
  summarize(
    Total_Sales = sum(Sale..Dollars.),
    Total_Bottles_Sold = sum(Bottles.Sold), 
    Average_Sale_Per_Transaction = mean(Sale..Dollars.),
    Total_Transactions = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    Average_Sale_Per_Bottle = Total_Sales / Total_Bottles_Sold
  )

# Viewing the top-performing stores by total sales
top_stores_by_sales <- store_sales %>%
  arrange(desc(Total_Sales))
head(top_stores_by_sales)

# Viewing the top stores by average sale price per bottle
top_stores_by_avg_bottle_sale <- store_sales %>%
  arrange(desc(Average_Sale_Per_Bottle))
head(top_stores_by_avg_bottle_sale)

# Counting the unique number of products sold by each store
store_product_variety <- data %>%
  group_by(Store.Number, Store.Name) %>%
  summarize(
    Unique_Products_Sold = n_distinct(Item.Description),
    .groups = 'drop'
  )

# Viewing the stores with the most product variety
top_stores_by_product_variety <- store_product_variety %>%
  arrange(desc(Unique_Products_Sold))
head(top_stores_by_product_variety)

# Combining the metrics into one data frame
store_performance <- store_sales %>%
  left_join(store_product_variety, by = c("Store.Number", "Store.Name"))

# Viewing the combined store performance data
head(store_performance)

#Visualizations for the above analysis

# Dark red color palette
dark_red_palette <- c("#7C0A02", "#8B0000", "#9B111E", "#A52A2A", "#B22222", 
                      "#C3272B", "#D73B3E", "#E74C3C", "#F44336", "#FF4D4D")

# Plotting the top 10 stores by total sales 
ggplot(head(store_performance, 10), aes(x = reorder(Store.Name, Total_Sales), y = Total_Sales, fill = Store.Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = dark_red_palette) +
  coord_flip() +  
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    legend.position = "none"  
  ) +
  labs(title = "Top 10 Stores by Total Sales", x = "Store Name", y = "Total Sales")

# Dark red color palette
dark_red_palette <- c("#7C0A02", "#8B0000", "#9B111E", "#A52A2A", "#B22222", 
                      "#C3272B", "#D73B3E", "#E74C3C", "#F44336", "#FF4D4D")

# Plotting the top 10 stores by average sale price per bottle as a bubble plot
ggplot(head(store_performance, 10), aes(x = reorder(Store.Name, Average_Sale_Per_Bottle), y = Average_Sale_Per_Bottle, size = Average_Sale_Per_Bottle, color = Store.Name)) +
  geom_point(alpha = 0.7) +  
  scale_color_manual(values = dark_red_palette) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 90, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none" 
  ) +
  labs(x = "Store Name", y = "Average Sale Price per Bottle", title = "Top 10 Stores by Average Sale Price per Bottle") +
  guides(size = FALSE) 


# Defining a dark red color palette 
dark_red_palette <- c("#7C0A02", "#8B0000", "#9B111E", "#A52A2A", "#B22222", 
                      "#C3272B", "#D73B3E", "#E74C3C", "#F44336", "#FF4D4D")

# Plotting the top 10 stores by unique products sold 
ggplot(head(store_performance, 10), aes(x = reorder(Store.Name, Unique_Products_Sold), y = Unique_Products_Sold, fill = Store.Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = dark_red_palette) + 
  coord_flip() + 
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "none"  
  ) +
  labs(title = "Top 10 Stores by Unique Products Sold", x = "Store Name", y = "Unique Products Sold")



# 3. Prominent liquor categories and their sales composition.


# Grouping data by Category Name
grouped_data <- data %>%
  group_by(Category.Name) %>%
  summarise(Total_Sales = sum(Sale..Dollars.),
            Average_Bottle_Retail_Price = mean(State.Bottle.Retail),
            Total_Bottles_Sold = sum(Bottles.Sold))

# Arranging the data in descending order of Total Sales
sorted_data <- arrange(grouped_data, desc(Total_Sales))

# Filtering for the top 10 categories by Total Sales
top_10_sorted_data <- head(sorted_data, 10)

# Creating a bar plot for the top 10 categories
ggplot(top_10_sorted_data, aes(x=reorder(Category.Name, Total_Sales), y=Total_Sales, fill=Category.Name)) +
  geom_bar(stat="identity") + 
  scale_fill_manual(values=c("#8B0000", "#A52A2A", "#B22222", "#C71585", "#CD5C5C", "#D2691E", "#DC143C", "#E9967A", "#FF0000", "#FF6347")) +
  labs(title="Top 10 Liquor Categories by Total Sales",
       x="Category Name",
       y="Total Sales (Dollars)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, face = "bold"), 
    axis.text.y = element_text(face = "bold"), 
    title = element_text(face = "bold"), 
    axis.ticks = element_blank(), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.title = element_text(hjust = 0.5) 
  )

# 4. Consumer preferences in terms of product features such as bottle volume, price segments, etc.


# Bottle Volume Analysis

# Grouping data by bottle volume and summarize sales and number of transactions
bottle_volume_summary <- data %>%
  group_by(Bottle.Volume..ml.) %>%
  summarise(Total.Sales = sum(Sale..Dollars.), Transactions = n()) %>%
  arrange(desc(Total.Sales))

# Filtering the data to include only bottle volumes up to 2000 ml
bottle_volume_summary_filtered <- bottle_volume_summary %>%
  filter(Bottle.Volume..ml. <= 2000) %>%
  arrange(desc(Total.Sales)) %>%
  top_n(6, Total.Sales)

# Plotting the graph with the updated dataset 
ggplot(bottle_volume_summary_filtered, aes(x = factor(Bottle.Volume..ml., levels = unique(Bottle.Volume..ml.)), y = Total.Sales, fill = factor(Bottle.Volume..ml.))) +
  geom_bar(stat = "identity", width = 1) +  
  scale_fill_brewer(palette = "Reds") +  
  theme_minimal() +
  theme(
    text = element_text(face = "bold"),  
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),  
    axis.text = element_text(face = "bold"), 
    axis.title = element_text(face = "bold"),  
    plot.title = element_text(face = "bold") 
  ) +
  labs(title = "Top 6 Bottle Volumes", x = "Bottle Volume (ml)", y = "Total Sales ($)")


# Price Segment Analysis

# Creating price segments
data <- data %>%
  mutate(Price.Segment = cut(State.Bottle.Retail,
                             breaks = c(-Inf, 10, 20, 30, 40, Inf),
                             labels = c("Under $10", "$10-20", "$20-30", "$30-40", "Above $40")))

# Summarizing data by price segment
price_segment_summary <- data %>%
  group_by(Price.Segment) %>%
  summarise(Total.Sales = sum(Sale..Dollars.), Transactions = n()) %>%
  arrange(desc(Total.Sales))

# Plotting the summary
ggplot(price_segment_summary, aes(x = Price.Segment, y = Total.Sales, fill = Price.Segment)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Reds") +
  theme_minimal() +
  theme(
    text = element_text(face = "bold"), 
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.text = element_text(face = "bold"), 
    axis.title = element_text(face = "bold"), 
    plot.title = element_text(face = "bold")   
  ) +
  labs(title = "   Total Sales by Price Segment", x = "Price Segment", y = "Total Sales ($)")


# 5. Vendor performance and their contribution to total sales and liters


# Converting Bottle Volume from ml to liters
data$Bottle.Volume.liters <- data$Bottle.Volume..ml. / 1000

# Calculating Total Volume Sold in liters for each invoice
data$Total.Volume.Sold.liters <- data$Bottle.Volume.liters * data$Bottles.Sold

# Aggregating data by Vendor Name
vendor_performance <- data %>%
  group_by(Vendor.Name) %>%
  summarise(Total.Sales = sum(Sale..Dollars.), 
            Total.Volume.Sold = sum(Total.Volume.Sold.liters)) %>%
  arrange(desc(Total.Sales))

# Filtering for the top 10 vendors
top_vendors <- head(vendor_performance, 10)

# Defining a dark red color palette
dark_red_palette <- c("#8B0000", "#A52A2A", "#B22222", "#C71585", "#CD5C5C", "#D2691E", "#DC143C", "#E9967A", "#F08080", "#FA8072")

# Creating a bar plot for the top 10 vendors
ggplot(top_vendors, aes(x = reorder(Vendor.Name, Total.Sales), y = Total.Sales, fill = Vendor.Name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = dark_red_palette) +  
  coord_flip() + 
  labs(title = "Top 10 Vendors by Total Sales",
       x = "Vendor Name",
       y = "Total Sales (Dollars)") +
  theme_minimal() +
  theme(legend.position = "none", 
        text = element_text(face = "bold"),  
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 


# 6. Impact of geographical and other factors on sales.


# Total Sales by City
city_sales <- data %>%
  group_by(City) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop') %>%
  arrange(desc(Total_Sales))

# Top 10 Cities by Sales
top_cities <- head(city_sales, 10)

# Total Sales by County
county_sales <- data %>%
  group_by(County) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop') %>%
  arrange(desc(Total_Sales))

# Top 10 Counties by Sales
top_counties <- head(county_sales, 10)

# Colour Palette
dark_red_palette <- c("#8B0000", "#A52A2A", "#B22222", "#C3272B", "#D73B3E", 
                      "#E74C3C", "#F44336", "#FF4D4D", "#CD5C5C", "#DC143C")

# Plotting Top Cities with the custom dark red palette
ggplot(top_cities, aes(x=reorder(City, Total_Sales), y=Total_Sales, fill=City)) +
  geom_bar(stat="identity") +  
  scale_fill_manual(values = dark_red_palette) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1),  
    text = element_text(face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  labs(x="City", y="Total Sales", title="Top 10 Cities by Sales")

# Defining a custom dark red color palette
dark_red_palette <- c("#8B0000", "#A52A2A", "#B22222", "#C3272B", "#D73B3E", 
                      "#E74C3C", "#F44336", "#FF4D4D", "#CD5C5C", "#DC143C")

# Plotting Top Counties with the custom dark red palette
ggplot(top_counties, aes(x=reorder(County, Total_Sales), y=Total_Sales, fill=County)) +
  geom_bar(stat="identity") +  
  scale_fill_manual(values = dark_red_palette) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1),  
    text = element_text(face = "bold"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    plot.background = element_rect(fill = "white", colour = "white")
  ) +
  labs(x="County", y="Total Sales", title="Top 10 Counties by Sales")


# Analyzing Sales by Category
category_sales <- data %>%
  group_by(Category.Name) %>%
  summarize(Total_Sales = sum(Sale..Dollars.), .groups = 'drop') %>%
  arrange(desc(Total_Sales))

# Defining custom dark red color palette
dark_red_palette <- c("#8B0000", "#A52A2A", "#B22222", "#C3272B", "#D73B3E", 
                      "#E74C3C", "#F44336", "#FF4D4D", "#CD5C5C", "#DC143C")

# Plotting for Category Sales with customizations
ggplot(head(category_sales, 10), aes(x=reorder(Category.Name, Total_Sales), y=Total_Sales, fill=Category.Name)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values = dark_red_palette) +  
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 1, face = "bold"),  
    text = element_text(face = "bold"),  
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    legend.position = "none"  
  ) +
  labs(x="Category Name", y="Total Sales", title="Top Liquor Categories by Sales")

