# Install required package
install.packages("sqldf")
library(sqldf)

# Load data
data <- `data[1]`

#SELECT, WHERE, ORDER BY
sqldf("SELECT InvoiceNo, Description, Quantity 
       FROM data 
       WHERE Quantity > 10 
       ORDER BY Quantity DESC 
       LIMIT 10")

#gROUP BY with aggregation
sqldf("SELECT Country, COUNT(DISTINCT CustomerID) AS UniqueCustomers,
              SUM(Quantity * UnitPrice) AS TotalSales 
       FROM data 
       GROUP BY Country 
       ORDER BY TotalSales DESC")

#Subquery – Customers who spent above average
sqldf("
SELECT CustomerID
FROM data
GROUP BY CustomerID
HAVING SUM(Quantity * UnitPrice) > (
  SELECT AVG(TotalSpent) 
  FROM (
    SELECT CustomerID, SUM(Quantity * UnitPrice) AS TotalSpent 
    FROM data 
    GROUP BY CustomerID
  )
)
")

#Aggregate functions – Top products
sqldf("SELECT Description, SUM(Quantity) AS TotalSold 
       FROM data 
       GROUP BY Description 
       ORDER BY TotalSold DESC 
       LIMIT 10")

#Create a View (store in a variable)
top_products <- sqldf("SELECT Description, SUM(Quantity) AS TotalSold 
                       FROM data 
                       GROUP BY Description 
                       ORDER BY TotalSold DESC 
                       LIMIT 10")


#Save queries' outputs as CSV
write.csv(top_products, "Top_Products.csv")
