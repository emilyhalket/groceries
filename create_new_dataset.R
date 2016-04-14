# --------------
# Script to read in and modify data for intial reports

setwd('/Users/emilyhalket/Desktop/professional_development/company_specific/etsy/gma_homework-master/')
library(dplyr)


prod_class <- read.table('./raw_data/product_class.csv', sep = ",", header = TRUE)
prod_df <- read.table('./raw_data/product.csv', sep = ",", header = TRUE)
transact <- read.table('./raw_data/transactions.csv', sep = ",", header = TRUE)


#----- deal with typo in fact_count of transactions -------- 

transact$fact_count[transact$fact_count == "1);"] <- "1" 
transact$fact_count <- droplevels(transact$fact_count)

#--------create new data_frame by combining data across sheets-------


prod_df_subset <- prod_df %>% select(product_class_id:product_name, shelf_width:shelf_depth)

prod_info <-
  left_join(transact,prod_df_subset, by = "product_id" ) %>%
  left_join(prod_class, by = "product_class_id") %>%
  mutate(profit = store_sales - store_cost)

#-------- create column of department based on my definitions--------
prod_info$dept <-prod_info$product_department
prod_info$dept[prod_info$dept == 'Canned Products'] <- 'Canned Foods'
prod_info$dept[prod_info$dept == 'Snacks'] <- 'Snack Foods'
prod_info$dept <- droplevels(prod_info$dept)  


# --- make new categories to keep from mixing redundant category name  ---------

## use look up table that shows category matching

lookup <- read.table('./manipulated_dept_cat_lookup.csv', sep = ",", header = TRUE)

prod_info <- left_join(prod_info,lookup)
prod_info$new_category <- as.character(prod_info$new_category)
prod_info$new_category <- ifelse(is.na(prod_info$new_category), prod_info$product_category, prod_info$new_category)



# create new department groupings for visualizations
dept <- c('Dairy',
              'Deli',
              'Eggs',
              'Frozen Foods',
              'Meat',
              'Produce',
              'Seafood',
              'Baked Goods',
              'Breakfast Foods',
              'Canned Foods',
              'Snack Foods',
              'Starchy Foods',
              'Baking Goods')


food_dept <- c(rep('Perish', 8), rep('Non Perish', 5))

plot_groups <- data.frame(dept, food_dept)

prod_info <- left_join(prod_info, plot_groups)


# use family & department values to group category factor appropriately
prod_info$family_level <- as.integer(factor(prod_info$product_family))
prod_info$dept_level <- as.integer(factor(prod_info$dept))

# -------- select subset for CSV ---------

prod_info %>%
  select(product_id, customer_id, store_id, promotion_id,
         month_of_year, quarter, store_sales, store_cost, profit, unit_sales, 
         product_class_id, brand_name, product_name, product_subcategory, 
         product_family, family_level, dept, dept_level, new_category, food_dept) %>%
  write.csv('reorganized_foodmart_data_for_visualizations.csv', row.names = FALSE)



