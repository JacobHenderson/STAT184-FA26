# PROBLEM 1: Objects, data structures, and subsetting
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)

# PART A: Building Data Structures
# Convert section to a factor with levels in the order "A", "B";
section <- factor(section, levels=c("A", "B"))
# Create a data frame named students containing all five vectors;
students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)
# Create score_matrix, with students as rows, quizzes as columns, student IDs as row names, and quiz1 and quiz2 as column names;
score_matrix <- cbind(quiz1, quiz2)
rownames(score_matrix) <- student_id
colnames(score_matrix)<- c("quiz1", "quiz2")
# Create course_record, a list containing course = "R Programming", scores = students, and a named vector cutoffs = c(pass = 70, excellent = 90).

course_record = list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass=70, excellent=90)
)

# Use typeof(), class(), length(), str(), and dim() where appropriate to inspect these objects.

typeof(student_id)
class(student_id)
length(student_id)

typeof(section)
class(section)
length(section)

str(students)
dim(students)

str(score_matrix)
dim(score_matrix)

str(course_record)
length(course_record)

#  In four or fewer sentences, compare a vector, list, matrix, factor, and data frame:
# A vector is a one-dimensional collection of values of the same basic type.
# A factor is a vector, used to represent categorical data with defined levels.
# A matrix and a data frame are TWO-dimensional data structures, but a matrix's elements have the same type while a data frame's columns can differ in type.
# A list is a collection of objects, which can have varying types & structures.

# PART B: Subsetting
# Using the objects from Part A...
# extract student S04’s second quiz score from score_matrix;
score_matrix["S04", "quiz2"]
# extract the first two matrix rows while preserving matrix dimensions;
score_matrix[1:2, , drop = FALSE]
# extract course from course_record once with [, once with [[, and once with $;
course_record["course"]
course_record[["course"]]
course_record$course
# briefly explain the difference between ‘[’, ‘[[’ and ‘$’.
# [ returns a subset that may contain multiple elements & can preserve structure
# [[ extracts a single element from a list (or similar object)
# $ extracts a named element using its name directly


# PART C: Vectorized calculations
# Without a loop...
# Add average, the row-wise mean of the two quizzes, to students;
students$average <- rowMeans(students[, c("quiz1", "quiz2")])
# Add excellent, equal to TRUE when average >= 90;
students$excellent <- students$average >= 90
# Select section A students whose average is at least 80;
section_A_80 <- students[
  students$section == "A" & students$average >= 80,
]
# Return only student_id, section, and average for that subset;
section_A_80[, c("student_id", "section", "average")]
# Create a named numeric vector of all student averages, using student IDs as names.
student_averages <- students$average
names(student_averages) <- students$student_id
student_averages

# Briefly explain why your calculations are vectorized:
# They're vectorized because R performs the operations on entire vectors/rows at once, rather than using an explicit loop


# PROBLEM 2: Importing, cleaning, and summarizing data

csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"

# PART A: Importing data
# Use read.csv() with its text argument to create measurements. Treat both blank fields and the text "NA" as missing.
measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)
# inspect the result with head(), str(), dim(), and names();
head(measurements)
str(measurements)
dim(measurements)
names(measurements)
# count missing values in each column;
colSums(is.na(measurements))
# create measurements_complete using complete.cases();
measurements_complete <- measurements[complete.cases(measurements), ]
# report the sample IDs removed by the complete-case filter;
measurements$sample_id[!complete.cases(measurements)]
# explain why x == NA is not a valid missing-value test:
# NA represents an unknown/missing value, so comparisons involving NA return NA
# is.na(x) is the proper missing-value test


# PART B: Transform, filter, and summarize
# Without a loop argument...
# Convert site and status to factors and report their levels;
measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)
# Create temp_f = temp_c * 9 / 5 + 32;
measurements$temp_f <- measurements$temp_c * 9 / 5 + 32
# Create ph_below_7, preserving missing pH values;
measurements$ph_below_7 <- ifelse(
  is.na(measurements$ph),
  NA,
  measurements$ph < 7
)
# Select complete observations from North or South whose status is "ok";
selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
]
# Return only sample_id, site, temp_c, temp_f, and ph;
selected_measurements[
  , c("sample_id", "site", "temp_c", "temp_f", "ph")
]
# Calculate the overall mean Celsius temperature and the South-site mean, ignoring missing values
mean(measurements$temp_c, na.rm = TRUE)
mean(measurements$temp_c[measurements$site == "South"], na.rm = TRUE)

# PART C: Matrix Operations
A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)
# Compute A * B and A %*% B. Report the dimensions of both results and explain briefly how element-wise multiplication differs from matrix multiplication
A * B              # Multiplies corresponding elements of the two matrices
A %*% B            # performs matrix multiplication using rows of A and columns of B
dim(A * B)
dim(A %*% B)
# Both results have dimensions 2x2


# PROBLEM 3: Functions
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)
# PART A: Write a grading function
# Write grade_one() with default grade boundaries:
grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
  ) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}
    
  
# Use if, else if, and else to return grades A through F. Return NA_character_ when the score is missing.

    
# Now test the function using the code below, and report its output.
grade_one(NA) # NA
grade_one(90) # "A"
grade_one(80) # "B"
grade_one(85) # "B"
grade_one(74) # "C"


# PART B: Applying functions with loop
# Create a character vector named grades with the same length as scores, initially filled with NA_character_.
# Use one for loop with seq_along(scores) to call grade_one(scores[i]) and store the result in grades[i].
# Because grade_one() already handles missing values, no separate missing-value condition is needed.
# After the loop, assign student_id as the names of grades, display the completed vector, and explain in one
# sentence what i represents during the loop.
# You may begin with:
grades <- rep(NA_character_, length(scores))
for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}
names(grades) <- student_id
grades
# i represents the index of the scores vector's current score.


# PART C-1: Function behavior
summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  
  if (na.rm) {
    mean_value <- mean(x, na.rm = TRUE)
    sd_value <- sd(x, na.rm = TRUE)
    min_value <- min(x, na.rm = TRUE)
    max_value <- max(x, na.rm = TRUE)
  } else {
    mean_value <- mean(x, na.rm = FALSE)
    sd_value <- sd(x, na.rm = FALSE)
    min_value <- min(x, na.rm = FALSE)
    max_value <- max(x, na.rm = FALSE)
  }
  
  result <- c(
    total_count = total_count,
    missing_count = missing_count,
    mean = mean_value,
    sd = sd_value,
    minimum = min_value,
    maximum = max_value
  )
  
  result[c("mean", "sd", "minimum", "maximum")] <-
    round(result[c("mean", "sd", "minimum", "maximum")], digits)
  
  return(result)
}
  
summarize_scores(scores)

# It should return a named numeric vector containing total count, missing count, mean, standard deviation,
#  minimum, and maximum. Round the four statistics to digits places. Call it once with defaults and once
# with named arguments.
# Then test the function using the code below:

summarize_scores( x = scores, na.rm = TRUE, digits = 2 )



# PART C-2: Function behavior
# Also write plot_scores <- function(x, ...) so that it plots scores against their positions.
# Then test the function using the code below:
plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(scores, type = "b", pch = 19, xlab = "Position", ylab = "Score", main = "Student Scores" )

