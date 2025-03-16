

# Kết nối đến MySQL
library(DBI)
library(RMySQL)

# Kết nối đến cơ sở dữ liệu MySQL
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "DESKTOP-P1JTV74", 
                 host = "http://127.0.0.1:4040/", 
                 username = "khanh", 
                 password = "123456")

# Thực hiện truy vấn
data <- dbGetQuery(con,"SELECT * FROM `df_combined` LIMIT 1000")

# Đóng kết nối
dbDisconnect(con)