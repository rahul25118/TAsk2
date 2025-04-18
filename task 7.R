#load data
sales <- read.csv("data.csv")
#install important libraries
install.packages("RSQLite")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("readr")
#load library
library(readr)
library(RSQLite)
library(readr)
library(dplyr)
library(ggplot2)
# View the structure and first few rows
str(sales)
head(sales)
#Create SQLite connection
conn <- dbConnect(SQLite(), dbname = "sales.db")

#Write data into a table called 'sales'
dbWriteTable(conn, "sales", sales, overwrite = TRUE)

#SQL query for summary using Description as product name
query <- "
SELECT Description AS product,
       SUM(Quantity) AS total_qty,
       SUM(Quantity * UnitPrice) AS revenue
FROM sales
WHERE Quantity > 0 AND UnitPrice > 0
GROUP BY Description
ORDER BY revenue DESC
LIMIT 10
"

#Get summary into dataframe
sales_summary <- dbGetQuery(conn, query)

#Print the summary
print("Top 10 Products by Revenue:")
print(sales_summary)

#Plot the bar chart
ggplot(sales_summary, aes(x = reorder(product, revenue), y = revenue, fill = product)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Products by Revenue",
       x = "Product",
       y = "Revenue") +
  theme_minimal()
#disconncet query from r
dbDisconnect(conn)
