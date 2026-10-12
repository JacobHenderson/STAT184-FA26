library(dplyr); library(tidyr); library(tibble)
library(readr); library(stringr); library(forcats); library(purrr)

# PROBLEM 1: Clean, validate, and summarize
sales_raw <- tibble(
  sale_id = 1:8,
  customer_id = c("C1", "C2", "C1", "C3", "C2", "C4", "C5", "C2"),
  item_raw = c(" Pen", "PEN ", "Notebook", " pen ",
               "FOLDER", "Folder ", " ERASER ", "pen"),
  amount_raw = c("$10.00", "$20.00", NA, "$15.00",
                 "$30.00", "$25.00", NA, "$20.00"),
  discount_raw = c("0%", "10%", "5%", "0%", "20%", "10%", "0%", "0%"),
  code_raw = c("ST-101", "ON-102", "ST-103", "ON-104",
               "bad", "ST-106", "ON-107X", "on-108"),
  tags = c("gift;urgent", "online", "gift", "member",
           "online;member", "gift", "member", "gift;member")
)

# PART A: Text and numerical transformations
# Create sales_clean from sales_raw, keeping all original columns and all eight sale rows.
# item_clean: remove leading/trailing spaces, collapse extra inner spaces, make lowercase
#   (the item column in sales_raw is named item_raw, so item_raw is used here);
# amount and discount_rate: use parse_number(), discount stored as a proportion;
# net_amount = amount * (1 - discount_rate), missing amounts stay NA;
# size: case_when() with "unrecorded", "large" (net amount >= 20), and "small";
sales_clean <- sales_raw |>
  mutate(
    item_clean = str_to_lower(str_squish(item_raw)),
    amount = parse_number(amount_raw),
    discount_rate = parse_number(discount_raw) / 100,
    net_amount = amount * (1 - discount_rate),
    size = case_when(
      is.na(net_amount) ~ "unrecorded",
      net_amount >= 20 ~ "large",
      TRUE ~ "small"
    )
  )

# Display sale_id, item_clean, amount, discount_rate, net_amount, and size
sales_clean |>
  select(sale_id, item_clean, amount, discount_rate, net_amount, size)

# str_squish() removes spaces at both ends and reduces repeated inner spaces to one space
# parse_number() ignores "$" and "%", and NA input stays NA, so missing amounts are preserved
# The missing amounts (sales 3 and 7) give a missing net_amount, which is why they are "unrecorded" and not "small"


# PART B: Validate codes and split strings
# Create sales_codes from sales_clean with code_clean (trimmed, uppercase) and valid_code
# valid_code is TRUE only for ST or ON, then "-", then exactly three digits;
sales_codes <- sales_clean |>
  mutate(
    code_clean = str_to_upper(str_trim(code_raw)),
    valid_code = str_detect(code_clean, "^(ST|ON)-[0-9]{3}$")
  )
# Save and display invalid rows as invalid_codes;
invalid_codes <- sales_codes |>
  filter(!valid_code) |>
  select(sale_id, code_raw, code_clean)
invalid_codes
# "bad" and "ON-107X" are invalid; "on-108" becomes "ON-108" after cleaning, so it is valid
# sales_codes still has all eight rows

# Create valid_code_parts from the valid rows only, splitting code_clean into channel and number
# (cols_remove = FALSE keeps code_clean so that four columns can be displayed);
valid_code_parts <- sales_codes |>
  filter(valid_code) |>
  separate_wider_delim(
    code_clean,
    delim = "-",
    names = c("channel", "number"),
    cols_remove = FALSE
  )
valid_code_parts |>
  select(sale_id, code_clean, channel, number)

# Create sale_tags from just sale_id and tags in sales_clean, one row per tag;
sale_tags <- sales_clean |>
  select(sale_id, tags) |>
  separate_longer_delim(tags, delim = ";")
sale_tags

# separate_wider_delim() splits one column into several new columns, while separate_longer_delim() splits it into several new rows.


# PART C: Grouped proportions and factor categories
# Use the unsplit sales_clean.
# customer_summary: sales_n, known_n, large_n, and large_share by customer_id;
# a group with no known amounts gets large_n = 0 and large_share = NA_real_
customer_summary <- sales_clean |>
  summarise(
    sales_n = n(),
    known_n = sum(!is.na(net_amount)),
    large_n = sum(net_amount >= 20, na.rm = TRUE),
    large_share = if_else(known_n == 0, NA_real_, large_n / known_n),
    .by = customer_id
  ) |>
  arrange(customer_id)
customer_summary

# C1's large_share: the numerator is the number of C1 sales with a known net amount of at least 20 (0, since sale 1 is 10),
# the denominator is the number of C1 sales with a known net amount (1, since sale 3 is missing and is not counted),
# so large_share = 0 / 1 = 0.

# Create sales_factors with item_factor (ordered by frequency) and item_group (top 2 items, the rest as "Other")
sales_factors <- sales_clean |>
  mutate(
    item_factor = fct_infreq(factor(item_clean)),
    item_group = fct_lump_n(factor(item_clean), n = 2, other_level = "Other")
  )
levels(sales_factors$item_factor)
levels(sales_factors$item_group)
fct_count(sales_factors$item_factor)
fct_count(sales_factors$item_group)

# item_factor keeps all four items (pen 4, folder 2, eraser 1, notebook 1) and only changes the order of the levels
# item_group keeps pen (4) and folder (2) and combines eraser and notebook into "Other" (2)
# Changing level order only rearranges the same categories and does not change any counts;
# combining categories merges several levels into one, so there are fewer categories and the counts change.


# PROBLEM 2: Missing Values and Joins
reports_raw <- tibble(
  form = c("A", "A", "A", "B", "B", "B"),
  line = c(1, 2, 3, 1, 2, 3),
  customer_id = c("C1", NA, "C2", "C3", NA, "C4"),
  round = c(1, 3, 1, 1, 2, 1),
  primary_amount = c(10, -999, 20, NA, 0, 30),
  backup_amount = c(NA, 15, 25, NA, 99, NA)
)

customers <- tibble(
  customer_id = c("C1", "C2", "C3", "C5"),
  region = c("North", "South", NA, "West")
)

# PART A: Clean and Add missing reports
# Create reports_labeled: sort by form and line, fill missing customer IDs downward within each form, then ungroup
reports_labeled <- reports_raw |>
  arrange(form, line) |>
  group_by(form) |>
  fill(customer_id, .direction = "down") |>
  ungroup()
reports_labeled

# Create reports_clean: primary_clean changes -999 to NA, and amount uses primary_clean first, then backup_amount
reports_clean <- reports_labeled |>
  mutate(
    primary_clean = na_if(primary_amount, -999),
    amount = coalesce(primary_clean, backup_amount)
  )
reports_clean |>
  select(form, line, customer_id, round, primary_amount, backup_amount, amount)

# coalesce() returns the first non-missing value, so the backup is only used when the primary value is missing.
# A recorded primary value, including 0, is a real measurement and is not missing, so it is kept:
# form B line 2 keeps its recorded 0 instead of the backup value of 99.

# From reports_clean keep customer_id, round, and amount; mark original rows BEFORE complete()
reports_marked <- reports_clean |>
  select(customer_id, round, amount) |>
  mutate(recorded_row = TRUE)
# Use complete() so every customer has rounds 1:3; added rows get recorded_row = FALSE and amount = NA
reports_complete <- reports_marked |>
  complete(customer_id, round = 1:3, fill = list(recorded_row = FALSE))
reports_complete

# Report the number of original rows, added rows, and missing amounts before and after completion
nrow(reports_marked)                        # original rows before completion
sum(reports_complete$recorded_row)          # original rows after completion
sum(!reports_complete$recorded_row)         # added rows
sum(is.na(reports_marked$amount))           # missing amounts before completion
sum(is.na(reports_complete$amount))         # missing amounts after completion

# Explicit missing amount: the row exists but the amount is NA
reports_marked |> filter(is.na(amount))
# Implicit missing row: the customer-round combination is absent before completion
reports_complete |> filter(!recorded_row)

# There are 6 original rows and 6 added rows (12 total), and missing amounts go from 1 to 7.
# Explicit: C3 round 1 exists as a row, but its amount is NA because both the primary and backup amounts are missing.
# Implicit: C1 round 2 has no row at all before completion (it only appears after complete()).


# PART B: Join and check
# Use reports_clean, not reports_complete.
# Check customers$customer_id for duplicate or missing IDs;
anyDuplicated(customers$customer_id)
sum(duplicated(customers$customer_id))
sum(is.na(customers$customer_id))
# Check whether any customer_id-round combination appears more than once in reports_clean;
reports_clean |>
  count(customer_id, round) |>
  filter(n > 1)
any(duplicated(reports_clean[, c("customer_id", "round")]))
# customers has no duplicate or missing IDs, and reports_clean has no repeated customer_id-round combination (0 rows returned),
# so a many-to-one join is appropriate.

# Add customer_found = TRUE to customers, then left join to reports_clean
customers_marked <- customers |>
  mutate(customer_found = TRUE)
reports_joined <- reports_clean |>
  left_join(
    customers_marked,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ) |>
  mutate(customer_found = replace_na(customer_found, FALSE))
reports_joined |>
  select(customer_id, round, amount, region, customer_found)

# Use anti_join() in both directions to list IDs found in only one table;
only_in_reports <- reports_clean |>
  anti_join(customers, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)
only_in_customers <- customers |>
  anti_join(reports_clean, by = join_by(customer_id), na_matches = "never") |>
  distinct(customer_id)
only_in_reports
only_in_customers
# C4 is only in reports_clean, and C5 is only in customers.
# C3 and C4 both have a missing region, but customer_found = TRUE for C3 means C3 has a record in customers whose region is unknown,
# while customer_found = FALSE for C4 means C4 has no customer record at all.
# A missing region alone cannot tell these two situations apart.


# PART C: Check and reshape reports
# customers_bad is not defined in the assignment, so it is created here:
# it is customers plus a second, conflicting record for C2 (South and East)
customers_bad <- tibble(
  customer_id = c("C1", "C2", "C2", "C3", "C5"),
  region = c("North", "South", "East", NA, "West")
)

# Left join reports_clean to customers_bad, allowing many-to-many for this check only
reports_duplicated <- reports_clean |>
  left_join(
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-many",
    na_matches = "never"
  )
# Compare row counts and total amount (ignoring NA) before and after joining
nrow(reports_clean)
nrow(reports_duplicated)
sum(reports_clean$amount, na.rm = TRUE)
sum(reports_duplicated$amount, na.rm = TRUE)
# Identify the repeated report
reports_duplicated |>
  count(customer_id, round) |>
  filter(n > 1)
reports_duplicated |>
  filter(customer_id == "C2")
# The row count goes from 6 to 7 and the total amount from 75 to 95.
# The repeated report is C2 round 1 (amount 20): C2 has two records in customers_bad, so that one report is matched twice
# and its amount is counted twice, which raises the total by 20.

# What would happen with relationship = "many-to-one"?
many_to_one_check <- tryCatch(
  left_join(
    reports_clean,
    customers_bad,
    by = join_by(customer_id),
    relationship = "many-to-one",
    na_matches = "never"
  ),
  error = function(e) conditionMessage(e)
)
many_to_one_check
# With "many-to-one", dplyr stops with an error because the C2 report matches more than one row in customers_bad.
# Allowing "many-to-many" does NOT fix the conflicting customer information: it only silences the check,
# and C2 still has two regions (South and East) and a duplicated report.
# The conflict has to be fixed in customers_bad itself (keep one correct record per customer) before joining.

# Create reports_wide from reports_complete, one row per customer with round_1, round_2, and round_3
reports_wide <- reports_complete |>
  select(customer_id, round, amount) |>
  pivot_wider(
    names_from = round,
    names_prefix = "round_",
    values_from = amount
  )
reports_wide

# Pivot back to long form with values_drop_na = TRUE and count the rows
reports_wide |>
  pivot_longer(
    cols = starts_with("round_"),
    names_to = "round",
    values_to = "amount",
    values_drop_na = TRUE
  ) |>
  nrow()
# Prediction: 5 rows remain, because only 5 of the 12 customer-round amounts are non-missing (10, 15, 20, 0, and 30).
# Dropping missing values does not recover the unknown amounts: it just deletes every NA row,
# including the explicit missing C3 round 1 amount and the 6 added rows, so the unknown values are still unknown
# and we can no longer tell which rounds were originally reported (the 0 for C3 round 2 is kept because zero is not missing).


# PROBLEM 3: Two-sample t-tests and simulation
# PART A: Write a t-test function
# H0: mu_x = mu_y versus HA: mu_x != mu_y (two-sided), independent samples
two_sample_t <- function(x, y, equal_var = FALSE, alpha = 0.05) {
  # Sample summaries
  nx <- length(x)
  ny <- length(y)
  mean_x <- mean(x)
  mean_y <- mean(y)
  var_x <- var(x)
  var_y <- var(y)
  
  if (equal_var) {
    # Pooled two-sample t-test
    method <- "Pooled"
    sp2 <- ((nx - 1) * var_x + (ny - 1) * var_y) / (nx + ny - 2)
    se <- sqrt(sp2 * (1 / nx + 1 / ny))
    df <- nx + ny - 2
  } else {
    # Welch's two-sample t-test
    method <- "Welch"
    se <- sqrt(var_x / nx + var_y / ny)
    df <- (var_x / nx + var_y / ny)^2 /
      ((var_x / nx)^2 / (nx - 1) + (var_y / ny)^2 / (ny - 1))
  }
  
  t_stat <- (mean_x - mean_y) / se
  p_value <- 2 * pt(abs(t_stat), df = df, lower.tail = FALSE)
  reject <- p_value < alpha
  
  return(list(
    method = method,
    t_stat = t_stat,
    df = df,
    p_value = p_value,
    reject = reject
  ))
}

# Run the function once with default arguments and once with equal_var = TRUE
x_check <- c(12, 15, 14, 10, 13)
y_check <- c(9, 11, 8, 10, 7, 12, 9, 8)

my_welch <- two_sample_t(x_check, y_check)
my_pooled <- two_sample_t(x_check, y_check, equal_var = TRUE)
my_welch
my_pooled

# Reference calculations; compare with the function results
ref_welch <- t.test(x_check, y_check, var.equal = FALSE)
ref_pooled <- t.test(x_check, y_check, var.equal = TRUE)
ref_welch
ref_pooled

# Differences between my function and t.test()
diff_welch <- c(
  t_stat = abs(my_welch$t_stat - unname(ref_welch$statistic)),
  df = abs(my_welch$df - unname(ref_welch$parameter)),
  p_value = abs(my_welch$p_value - ref_welch$p.value)
)
diff_pooled <- c(
  t_stat = abs(my_pooled$t_stat - unname(ref_pooled$statistic)),
  df = abs(my_pooled$df - unname(ref_pooled$parameter)),
  p_value = abs(my_pooled$p_value - ref_pooled$p.value)
)
diff_welch
diff_pooled
diff_welch < 1e-8
diff_pooled < 1e-8
# All differences are smaller than 1e-8 (every comparison is TRUE),
# so my function matches t.test() for both the Welch and the pooled test.


# PART B: Perform simulation loop
B <- 1000
n_x <- 20
n_y <- 40
alpha <- 0.05

scenarios <- data.frame(
  scenario = c("equal_null", "unequal_null",
               "equal_shift", "unequal_shift"),
  mean_x = c(0, 0, 0, 0),
  mean_y = c(0, 0, 1, 1),
  sd_x = c(1, 3, 1, 3),
  sd_y = c(1, 1, 1, 1)
)

# Allocate storage in advance: one row per scenario, repetition, and method
n_rows <- nrow(scenarios) * B * 2
simulation_results <- data.frame(
  scenario = character(n_rows),
  replicate = integer(n_rows),
  method = character(n_rows),
  t_stat = numeric(n_rows),
  df = numeric(n_rows),
  p_value = numeric(n_rows),
  reject = logical(n_rows),
  stringsAsFactors = FALSE
)

# Run once immediately before the simulation, not inside a loop.
set.seed(2026)

row_i <- 1
for (s in seq_len(nrow(scenarios))) {
  for (b in seq_len(B)) {
    # Fresh independent samples for this scenario and repetition
    x <- rnorm(n_x, mean = scenarios$mean_x[s], sd = scenarios$sd_x[s])
    y <- rnorm(n_y, mean = scenarios$mean_y[s], sd = scenarios$sd_y[s])
    
    # Apply both tests to the same samples
    welch_res <- two_sample_t(x, y, equal_var = FALSE, alpha = alpha)
    pooled_res <- two_sample_t(x, y, equal_var = TRUE, alpha = alpha)
    
    simulation_results$scenario[row_i] <- scenarios$scenario[s]
    simulation_results$replicate[row_i] <- b
    simulation_results$method[row_i] <- welch_res$method
    simulation_results$t_stat[row_i] <- welch_res$t_stat
    simulation_results$df[row_i] <- welch_res$df
    simulation_results$p_value[row_i] <- welch_res$p_value
    simulation_results$reject[row_i] <- welch_res$reject
    row_i <- row_i + 1
    
    simulation_results$scenario[row_i] <- scenarios$scenario[s]
    simulation_results$replicate[row_i] <- b
    simulation_results$method[row_i] <- pooled_res$method
    simulation_results$t_stat[row_i] <- pooled_res$t_stat
    simulation_results$df[row_i] <- pooled_res$df
    simulation_results$p_value[row_i] <- pooled_res$p_value
    simulation_results$reject[row_i] <- pooled_res$reject
    row_i <- row_i + 1
  }
}

# Display the first six rows and the total row count
head(simulation_results, 6)
nrow(simulation_results)    # 4 scenarios x 1000 repetitions x 2 methods = 8000

# Summarize the results: one row per scenario and method
simulation_summary <- simulation_results |>
  summarise(
    mean_t = mean(t_stat),
    sd_t = sd(t_stat),
    rejection_rate = mean(reject),
    .by = c(scenario, method)
  ) |>
  mutate(across(c(mean_t, sd_t, rejection_rate), \(v) round(v, 3)))
simulation_summary

# Plot: rejection rate by scenario and method (dashed line is alpha = 0.05)
rejection_matrix <- tapply(
  simulation_results$reject,
  list(simulation_results$method,
       factor(simulation_results$scenario, levels = scenarios$scenario)),
  mean
)
barplot(
  rejection_matrix,
  beside = TRUE,
  ylim = c(0, 1),
  col = c("gray30", "gray80"),
  legend.text = rownames(rejection_matrix),
  args.legend = list(x = "topleft", bty = "n"),
  xlab = "Scenario",
  ylab = "Rejection rate",
  main = "Rejection Rates by Scenario and Method"
)
abline(h = alpha, lty = 2)

# Short report:
# In the null scenarios, Welch's rejection rate stays near 0.05 in both cases, and the pooled test is also near 0.05 when the variances are equal, but the pooled test rejects roughly 0.14 to 0.15 of the time (about three times alpha) when the smaller sample x has the larger variance, which is a Type I error problem.
# When the means differ by one unit, rejection rates (power) rise: both tests are near 0.95 with equal variances, while with unequal variances Welch's power drops to roughly 0.3 and the pooled test's is higher at roughly 0.5.
# The pooled test's extra power in the unequal-variance shift scenario is not a real advantage because it comes along with an inflated Type I error, so power only matters for a test that also keeps its Type I error rate near alpha.
# With B = 1000, the rejection rates have random simulation error of about +/- 0.014 near 0.05 (and larger near 0.5), so small differences like 0.04 versus 0.06 are consistent with random variation.
