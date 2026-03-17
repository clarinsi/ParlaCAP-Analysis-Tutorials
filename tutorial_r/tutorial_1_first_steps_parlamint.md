# Tutorial 1 - First Steps with ParlaMint Dataset

Welcome to this **hands-on tutorial exploring the ParlaMint dataset**, a rich multilingual corpus of European parliamentary debates. This tutorial is designed for researchers, data scientists and political science enthusiasts who want to analyze parliamentary proceedings across different European countries.

The **ParlaMint dataset** is a comprehensive and multilingual corpus of transcribed speeches from different European parliaments. It serves as a valuable resource for computational social science and digital humanities research.

At its core, ParlaMint contains the full text of parliamentary speeches annotated with rich metadata, including the speaker's role (e.g. *Member of Parliament*), political party affiliation, date of the session and the length of each speech. This foundational data was significantly expanded with **Comparative Agendas Project categories** and **sentiment scores**. 

First, the ParlaCAP component categorizes all available speech segments into a specific policy domain from the **Comparative Agendas Project (CAP)**, e.g. healthcare, education or foreign affairs. CAP is an international research collaboration that has developed a standardized coding scheme for classifying government activities by policy topic. Originally created for the U.S. Policy Agendas Project in 1993, it has since been adapted for over 20 countries. The coding scheme includes 21 major topic categories (such as Macroeconomics, Health, Defense) and approximately 213 subtopics, enabling systematic cross-national comparison of policy attention. The CAP coding scheme used here follows the Master Codebook available at comparativeagendas.net. „CAP_category“ in our tutorials corresponds to the CAP "major topic" level (21 categories).

Second, the **ParlaSent** extension provides detailed **sentiment scores** for each segment, allowing researchers to analyze a layer of sentiments (how issues are discussed). This combination allows researchers to examine both attention and tone-dimensions that the original CAP framework intentionally kept separate.

The dataset's power lies in its comparative design. It covers **multiple countries** and spans different time periods, enabling cross-parliament analysis. This makes ParlaMint very useful for **comparative political research**, allowing users to systematically study the differences in e.g. political discourse or policy priorities, across nations. Besides that, its structure makes it ideal for technical applications like **topic modeling** and **sentiment analysis** on a large scale.

*Source: Erjavec, Tomaž; et al. (2025). Multilingual comparable corpora of parliamentary debates ParlaMint 5.0. Slovenian language resource repository CLARIN.SI. ISSN 2820-4042. http://hdl.handle.net/11356/2004*

---

## 1. Setup

Before we begin working with the data, we need to prepare our R environment. This involves two steps: first, ensuring the needed packages are installed, and second, loading them into our session so we can use them.

### Package Installation

If you are running this code in a new environment (e.g. RStudio, a new R project, or an R Markdown notebook), you may need to install some packages first. You can do this by running the command below. In R, we use the `install.packages()` function to download and install packages from CRAN (the Comprehensive R Archive Network).

```r
# Uncomment and run the following lines if you haven't installed these packages yet
# install.packages("tidyverse")
# install.packages("data.table")
```

### Loading Libraries

The following lines of code load the necessary tools for our analysis. In this step, we *gather* all the tools that we need before starting our analysis.

- **tidyverse**: A collection of R packages designed for data science, including `dplyr` (data manipulation), `readr` (reading data), and `ggplot2` (visualization)
- **data.table**: A high-performance package for working with large datasets efficiently

```r
library(tidyverse)
library(data.table)
```

---

## Note: Dataset Size and Memory Management

Before we load the data, it's important to understand its scale. We are working with 28 country-specific datasets, which together represent a very large corpus of parliamentary speech. The individual files in your directory vary in size. We can group them into four broad categories:

- **Small datasets (less than 50 MB)**: Including countries like Sweden (SE) and Spain's regional parliaments (ES-CT, ES-GA, ES-PV).
- **Medium Datasets (50 - 150 MB)**: This is the largest category, encompassing the majority of countries such as the Czech Republic (CZ), Denmark (DK), Greece (GR), and Poland (PL).
- **Large datasets (150 - 250 MB)**: This group includes larger parliamentary bodies like those of France (FR), the United Kingdom (GB) and the Netherlands (NL).
- **The largest dataset (> 350 MB)**: The corpus for Turkey (TR) is the single largest file at 370 MB.

**The combined size of all these files is over 2.5 GB**. However, when loaded into an R data frame in memory, this can expand significantly, potentially requiring 8-10 GB of RAM or more if loaded without smart filtering. To prevent your computer from crashing or becoming slow, we need to work efficiently with the data and use these two important strategies:

1. **Optimized data types**: In the code below, we explicitly tell R whether a column contains text, categories (factors) or numbers. This significantly reduces the amount of RAM required. For example, storing a limited set of values like 'Coalition' and 'Opposition' as a factor is much more efficient than storing them as character strings.

2. **Selective column loading**: Instead of loading all columns from each file, we specify exactly which columns we need. This reduces memory usage significantly.

The code below implements these strategies to ensure our analysis can run smoothly on a standard laptop.

---

## 2. Data Loading & Filtering

The following code is structured in 5 main steps to load and filter the data efficiently:

**1. Define countries and file paths (preparation)**: We create a list of all country codes and set up the path to our data directory.

**2. & 3. Memory optimization**: Here, the code specifies exactly which columns of the datasets to load and defines the data types to reduce RAM usage.

**4. Loading & processing loop**: This is the core of the script. For each country in our list, this code:
   - **Reads the dataset file (4.1.)** using `fread()` from data.table, which is optimized for large files
   - **Adds a 'country' label and filters speeches (4.2.)** to keep only those given by regular Members of Parliament (MP), removing technical speakers, ministers and others to focus on core parliamentary debate
   - **Cleans the data (4.3.)** by removing rows where the policy topic (CAP_category) or sentiment is missing

**5. Combine the final datasets**: This part comes after all countries have been processed and it combines everything into a final master data frame `filtered_all`.

```r
# ---- 1. Define the list of countries and base directory ----
countries <- c(
  "AT", "BA", "BE", "BG", "CZ", "DK", "EE", "ES", "ES-CT", "ES-GA", "ES-PV",
  "FR", "GB", "GR", "HR", "HU", "IS", "IT", "LV", "NL", "NO", "PL", "PT",
  "RS", "SE", "SI", "TR", "UA"
)  # change country codes according to your available datasets

base_dir <- "speeches_no_text"

# ---- 2. Choose what columns to read (including CAP and sentiment columns) ----
cols_to_keep <- c(
  "id",
  "date",
  "lang_code",
  "lang",
  "speaker_role",
  "speaker_MP",
  "speaker_minister",
  "speaker_party",
  "speaker_party_name",
  "party_status",
  "party_orientation",
  "speaker_id",
  "speaker_name",
  "speaker_gender",
  "speaker_birth",
  "word_count",
  "CAP_category",
  "sent3_category",
  "sent6_category",
  "sent_logit"
)

# ---- 3. Define column types to reduce memory ----
# In R, we specify column types using a named vector or col_types specification
col_types <- cols(
  id = col_character(),
  date = col_character(),
  lang_code = col_factor(),
  lang = col_factor(),
  speaker_role = col_factor(),
  speaker_MP = col_factor(),
  speaker_minister = col_factor(),
  speaker_party = col_factor(),
  speaker_party_name = col_factor(),
  party_status = col_factor(),
  party_orientation = col_factor(),
  speaker_id = col_factor(),
  speaker_name = col_factor(),
  speaker_gender = col_factor(),
  speaker_birth = col_integer(),
  word_count = col_integer(),
  CAP_category = col_factor(),
  sent3_category = col_factor(),
  sent6_category = col_factor(),
  sent_logit = col_double()
)

# ---- 4. Create a list to accumulate filtered data frames ----
all_data <- list()

for (country in countries) {
  # Build the file path for this country
  file_path <- file.path(
    base_dir,
    paste0("cda1055_dat_ParlaCAP-", country, "_speeches_no_text.tsv.zip")
  )
  
  # Check if file exists before trying to read it
  if (!file.exists(file_path)) {
    message(paste("File not found, skipping:", country))
    next
  }
  
  # ---- 4.1. Read the file using data.table's fread (fast and memory-efficient) ----
  # fread can read directly from zip files and is very fast for large files.
  # 
  # NOTE: The command used to unzip the file differs by operating system.
  # Choose the option that matches your system below.
  
  # -- Option A: macOS / Linux --
  # Uses the built-in `unzip` command. This is the default for non-Windows systems.
  chunk <- fread(
    cmd = paste("unzip -p", shQuote(file_path)),
    select = cols_to_keep,
    sep = "\t",
    strip.white = TRUE,
    showProgress = FALSE
  )
  
  # -- Option B: Windows with 7-Zip (recommended for Windows users) --
  # The built-in `unzip` command is not natively available on Windows and will
  # cause an error. Instead, you can use 7-Zip, a free tool.
  # Steps:
  #   1. Download 7-Zip from https://www.7-zip.org/
  #   2. Install it to the default location: C:/Program Files/7-Zip/
  #   3. Uncomment the block below and comment out Option A above.
  #
  # chunk <- fread(
  #   cmd = paste(
  #     shQuote("C:/Program Files/7-Zip/7z.exe"),
  #     "x -so",
  #     shQuote(file_path)
  #   ),
  #   select = cols_to_keep,
  #   sep = "\t",
  #   strip.white = TRUE,
  #   showProgress = FALSE
  # )
  
  # -- Option C: Windows with tar (available on Windows 10 build 17063 and later) --
  # If you have a recent version of Windows, you may have `tar` available natively.
  # Uncomment the block below as an alternative to 7-Zip (Option B).
  #
  # chunk <- fread(
  #   cmd = paste("tar -xOf", shQuote(file_path)),
  #   select = cols_to_keep,
  #   sep = "\t",
  #   strip.white = TRUE,
  #   showProgress = FALSE
  # )
  
   # Convert "-" and "" to real NA values in character/factor columns
  char_cols <- names(chunk)[sapply(chunk, function(x) is.character(x) || is.factor(x))]
  for (col in char_cols) {
    chunk[get(col) %in% c("-", "", " "), (col) := NA]
  }
  
  # Add country column
  chunk[, country := country]
  
  # ---- 4.2. Filter MPs with regular role ----
  filtered_chunk <- chunk[speaker_MP == "MP" & speaker_role == "Regular"]
  
  # ---- 4.3. Drop rows where CAP_category or sentiment is empty ----
  filtered_chunk <- filtered_chunk[
    !is.na(CAP_category) & CAP_category != "" &
    !is.na(sent3_category) & sent3_category != "" &
    !is.na(sent6_category) & sent6_category != ""
  ]
  
  # ---- 4.4. Accumulate filtered data ----
  if (nrow(filtered_chunk) > 0) {
    all_data[[country]] <- filtered_chunk
  }
  
  # Print progress
  message(paste("Processed:", country, "- Rows:", nrow(filtered_chunk)))
}

# ---- 5. Combine all accumulated data into one data frame ----
filtered_all <- rbindlist(all_data, use.names = TRUE)

# Clean up to free memory
rm(all_data, chunk, filtered_chunk)
gc()  # garbage collection to free memory

# Print the dimensions of our final dataset
print(paste("All filtered:", nrow(filtered_all), "rows,", ncol(filtered_all), "columns"))
```

**What does this output mean?** The message "All filtered: (rows, columns)" tells us how many parliamentary speeches we have after filtering and how many variables describe each speech.

---

## 2.1. Final Filtering

Before we begin exploring our dataset, we will make two final adjustments to the 'CAP_category' variable to ensure our analysis of policy topics will be clean and meaningful.

First, we want to exclude the two catch-all categories, '**Mix**' (for speeches that cover multiple topics) and '**Other**' (for topics that don't fit into the main taxonomy). The 'Mix' category indicates speeches that address multiple policy topics without a clear primary focus, while 'Other' captures content that doesn't fit standard CAP categories. For many research questions, these categories are too vague to provide interpretable results. The line below filters our data frame to **remove** any speeches that have been classified into these two categories.

```r
# Remove "Mix" and "Other" categories
filtered_all <- filtered_all[!CAP_category %in% c("Mix", "Other")]
```

After this filtering, our dataset no longer contains these categories. However, R internally still remembers that 'Mix' and 'Other' are possible values for the 'CAP_category' column (as factor levels). This is like an archive having empty folders for documents you've already removed. To tidy this up and make our analysis more efficient, we use this command:

```r
# Remove unused factor levels from CAP_category
filtered_all[, CAP_category := droplevels(as.factor(CAP_category))]
```

**What does `droplevels()` do?** In R, factor variables remember all possible categories (levels), even after filtering. The `droplevels()` function removes any levels that no longer have any observations, keeping our data clean.

---

## Extra: Applying Filters to Other Variables

*This same filtering can easily be adapted to focus your analysis on other key variables, such as gender, party affiliation or political orientation, by changing the column name and the values you wish to keep or exclude.*

```r
# Example 1: Keep only speeches from female speakers
# filtered_all <- filtered_all[speaker_gender == "F"]
# filtered_all[, speaker_gender := droplevels(as.factor(speaker_gender))]

# Example 2: Focus analysis on a specific set of parties
# parties_to_keep <- c("Social Democratic Party", "Green Party", "Conservative Party")
# filtered_all <- filtered_all[speaker_party_name %in% parties_to_keep]
# filtered_all[, speaker_party_name := droplevels(as.factor(speaker_party_name))]

# Example 3: Exclude speeches from politically left parties
# filtered_all <- filtered_all[!party_orientation %in% c("Left", "Far-left")]
# filtered_all[, party_orientation := droplevels(as.factor(party_orientation))]
```

---

## 3. Initial Data Exploration: Getting to Know Your Data

Now that we have loaded and filtered our data into the `filtered_all` data frame, it's time to get acquainted with it. In traditional research, this would be like a first skim through a new archive box - checking what documents are inside, how many, and getting a general sense of the content before moving on to a deep analysis. In data science, we do this with simple functions that give us a high-level overview.

We start with the most basic question: **What does this data actually look like?**

### 3.1. head()

The `head()` function is our go-to tool for this. It allows us to **peek at the first few rows** of our dataset. This shows us a sample of our actual data, the values in each column and confirms that our data loaded correctly.

```r
head(filtered_all, 10)

```

When you run this code, you will see a table output. The first row shows the **names of all our columns** (like 'country', 'date', 'speaker_party', 'CAP_category') and below it, you will see the actual data for the first 10 parliamentary speeches.

**Tip**: If using RStudio, you can also use `view(head(filtered_all, 10))` to open an interactive spreadsheet-like viewer of your data.

---

### 3.2. dim() and nrow()/ncol()

Our next question is: **How much data are we working with?**

The `dim()` function answers this by outputting a pair of numbers: the number of rows and the number of columns.

```r
dim(filtered_all)
```

You can also get these values separately:

```r
# Number of rows (speeches)
nrow(filtered_all)

# Number of columns (variables)
ncol(filtered_all)
```

The output, e.g. `2754914` rows and `21` columns, would tell us that we are analyzing **2,754,914 individual speeches**, each described by **21 different variables**.

---

### 3.3. summary()

The `summary()` function provides a quick **statistical overview** of the data in your data frame. For factor (categorical) variables, it shows counts of each level.

```r
# Summary of specific columns
summary(filtered_all[, .(country, CAP_category, sent3_category)])
```

For each chosen column ('country', 'CAP_category' and 'sent3_category'), `summary()` shows:
- For factors: counts of each category (most frequent ones listed)
- For numeric columns: min, max, median, mean, and quartiles
---

### 3.4. unique()

The `unique()` function extracts a clean list of all possible values in a column (e.g. 'CAP_category'). It extracts every distinct category without any duplicates or counts.

```r
unique(filtered_all$CAP_category)
```

To see the results in a cleaner numbered list format:

```r
# Display unique CAP categories as a numbered list
data.frame(CAP_category = unique(filtered_all$CAP_category))
```

Here, we do the same but instead of 'CAP_category', we look at all unique values in the column 'party_status':

```r
unique(filtered_all$party_status)
```

... or 'party_orientation':

```r
unique(filtered_all$party_orientation)
```

---

### 3.5. table() - Equivalent to value_counts()

To understand the data on a deeper level, we use `table()`. This function **counts how many times each unique value appears** in a column. Also, it shows us what values are the most common.

```r
# Count occurrences of each CAP category
table(filtered_all$CAP_category)
```

To get results sorted by frequency (most common first), like pandas' `value_counts()`:

```r
# Sorted frequency table (descending order)
sort(table(filtered_all$CAP_category), decreasing = TRUE)
```

In the case of 'CAP_category', this command answers the question: **"Which policy topics dominate the parliamentary agenda?"**. The output is a ranked list, showing the most frequently debated topics at the top.

*The same method can be applied to other columns, such as 'party_orientation' to see the left-right balance of speeches:*

```r
sort(table(filtered_all$party_orientation), decreasing = TRUE)
```

---

### 3.6. Checking for Missing Values

Before any analysis, we must verify that our key variables are complete. The `is.na()` function checks each value in a column to see if it's *empty*. By combining it with `any()`, it outputs a single, clear answer.

```r
# Check if there are ANY missing values in CAP_category
any(is.na(filtered_all$CAP_category))
```

This code answers a simple yes/no question: **"Are there any gaps in our policy topic data?"**. The output will be:

- **FALSE**: Nice! This means there are **no missing values**; every speech has been assigned a policy topic.
- **TRUE**: This would indicate that **at least one value is missing** and requires further investigation or reloading the notebook.

You can also check other columns:

```r
# Check other important columns for missing values
any(is.na(filtered_all$sent_logit))
any(is.na(filtered_all$word_count))
```

To get a complete overview of missing values across all columns:

```r
# Count missing values in each column
colSums(is.na(filtered_all))
```

---

## Conclusion

In this first tutorial, we took the essential steps toward working with the ParlaMint dataset. We learned how to:

- **Set up the environment** by installing and loading the necessary R packages
- **Load and filter the data efficiently**, using memory optimization techniques with `data.table` to handle very large files
- **Clean and refine the dataset** by removing unwanted categories
- **Explore the data** with foundational R functions such as `head()`, `dim()`, `summary()`, `unique()` and `table()` to get a first overview of the structure and content

By the end of this tutorial, you now have a **clean, filtered dataset** of parliamentary debates that is ready for deeper analysis.

## What's Next?

In the next tutorial (**Tutorial 2**), we will take this filtered dataset and explore **topic attention and sentiment distributions** in depth - visualizing which CAP categories dominate parliamentary debates or how sentiment varies by topic and country.

---
