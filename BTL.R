# 🚩 Kiểm tra dữ liệu trước khi tổng hợp
cat("\n🔍 Kiểm tra số dòng dữ liệu trước khi tổng hợp:\n")
cat("Số dòng dữ liệu: ", nrow(df_combined), "\n")

if (nrow(df_combined) == 0) {
  stop("❌ Lỗi: Dữ liệu trống sau khi lọc, không thể tính toán doanh số!")
}

# 🚩 Kiểm tra cột Sales có tồn tại không
if (!"Sales" %in% colnames(df_combined)) {
  stop("❌ Lỗi: Cột 'Sales' không tồn tại trong df_combined!")
}

# 🚩 Kiểm tra kiểu dữ liệu của cột Sales
if (!is.numeric(df_combined$Sales)) {
  cat("⚠️ Cảnh báo: Chuyển đổi cột Sales sang numeric...\n")
  df_combined$Sales <- as.numeric(df_combined$Sales)
}

# 🚩 Kiểm tra dữ liệu bị loại bỏ do NA
cat("\n🔍 Kiểm tra giá trị NA trước khi lọc:\n")
print(colSums(is.na(df_combined[, c("Quantity.Ordered", "Price.Each")])))

df_combined <- df_combined %>%
  filter(!is.na(Quantity.Ordered) & !is.na(Price.Each))

cat("✅ Số dòng còn lại sau khi lọc: ", nrow(df_combined), "\n")

# 🚩 Kiểm tra cột Month có dữ liệu hợp lệ không
cat("\n🔍 Kiểm tra Month có bị NA không:\n")
print(table(df_combined$Month, useNA = "always"))

if (sum(is.na(df_combined$Month)) > 0) {
  cat("⚠️ Cảnh báo: Tạo lại cột Month...\n")
  df_combined$Month <- format(as.Date(df_combined$Order.Date), "%m")
}

# 🚩 Kiểm tra cột Hours
cat("\n🔍 Kiểm tra Hours có bị NA không:\n")
print(table(df_combined$Hours, useNA = "always"))

# 🚩 Kiểm tra dữ liệu trước khi tổng hợp doanh số
cat("\n✅ Kiểm tra xong, tiến hành tổng hợp doanh số...\n")

sales_value_month <- aggregate(Sales ~ Month, data = df_combined, sum)

cat("\n🔹 Tổng hợp doanh số theo tháng:\n")
print(sales_value_month)

cat("\n✅ Hoàn tất kiểm tra và tổng hợp dữ liệu!\n")
