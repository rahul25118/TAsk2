library(tidyverse)
library(lubridate)

# Check structure
glimpse(`Sample_._Superstore[1]`)

# Clean column names if needed
names(`Sample_._Superstore[1]`) <- make.names(names(`Sample_._Superstore[1]`))

# Convert dates
`Sample_._Superstore[1]`$Order.Date <- mdy(`Sample_._Superstore[1]`$Order.Date)
`Sample_._Superstore[1]`$Ship.Date <- mdy(`Sample_._Superstore[1]`$Ship.Date)

# Check missing values
sum(is.na(`Sample_._Superstore[1]`))
colSums(is.na(`Sample_._Superstore[1]`))
#Removes all rows with any missing (NA) values and stores the clean data into a new object
superstore_clean <- na.omit(`Sample_._Superstore[1]`)

superstore_clean %>%
  mutate(Month = floor_date(Order.Date, "month")) %>%
  group_by(Month) %>%
  summarise(Total_Sales = sum(Sales)) %>%
  ggplot(aes(Month, Total_Sales)) +
  geom_line(color = "steelblue", size = 1.2) +
  labs(title = "Monthly Sales Trend", x = "Month", y = "Sales") +
  theme_minimal() #Creates a line plot showing total sales by month. This helps identify seasonal trends or sales growth over time.

superstore_clean %>%
  group_by(Region) %>%
  summarise(Sales = sum(Sales)) %>%
  ggplot(aes(reorder(Region, Sales), Sales, fill = Region)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Sales by Region", x = "Region", y = "Sales") +
  theme_minimal() #Produces a horizontal bar chart showing total sales by each region. Helps identify top-performing regions.

superstore_clean %>%
  group_by(Category, Sub.Category) %>%
  summarise(Sales = sum(Sales)) %>%
  ggplot(aes(Sub.Category, Sales, fill = Category)) +
  geom_col() +
  facet_wrap(~Category, scales = "free") +
  labs(title = "Category vs Sub-Category Sales", x = "Sub-Category", y = "Sales") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) #Creates facet bar plots showing how sub-categories perform within each main product category.

ggplot(superstore_clean, aes(Discount, Profit)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Effect of Discount on Profit", x = "Discount", y = "Profit") +
  theme_minimal() #A scatter plot showing the relationship between discount and profit. A linear regression line is added to show the trend — helpful for identifying whether discounts reduce or increase profit.

superstore_clean %>%
  group_by(Segment) %>%
  summarise(Sales = sum(Sales)) %>%
  ggplot(aes(reorder(Segment, Sales), Sales, fill = Segment)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Sales by Customer Segment", x = "Segment", y = "Sales") +
  theme_minimal() #A bar chart that compares sales across customer segments like Consumer, Corporate, and Home Office.
