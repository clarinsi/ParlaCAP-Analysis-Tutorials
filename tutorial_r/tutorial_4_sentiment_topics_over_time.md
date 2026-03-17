# Tutorial 4 - Sentiment & Attention to Topics over Time

In the previous tutorials, we explored **what topics are discussed in different parliaments** and **how positively or negatively they are discussed**. We also learned how to compare countries using cosine distances to see which parliaments are most similar in sentiment or topic focus.

In this tutorial, we take a closer look at **how these patterns change over time**. Specifically, we will:
- **Load and read the datasets**
- **Select a timeframe**
- **Calculate and visualize sentiment over time for one selected country (e.g. RS)**
   1) across **all topics**
   2) for **one specific topic** (e.g. 'Health')
- **Analyze average sentiment scores by country and CAP category** using heatmaps
- **Avg. weekly word count**, which reflect parliamentary attention
   - across selected countries (e.g. RS, HU, HR) for one chosen topic (e.g. 'Health')

This approach allows us to answer questions such as:
- *Are there changes in sentiment across CAP categories over time in the Serbian parliament?*
- *Did sentiment change noticeably during known impactful events, like COVID outbreak, in Serbia?*
- *How does the sentiment change towards 'Health' differ in parliamentary debates in Serbia (RS), Croatia (HR) and Hungary (HU)?*
- *How did attention to 'Health' evolve in Serbia, Hungary and Croatia? Are there noticeable patterns in weekly word count that suggest changes in parliamentary activity?*

We will approach these questions using **time series analysis**, a method that lets us track trends over regular intervals (e.g. weekly). For example, we can calculate the **average sentiment per week** for each country or for a specific topic. This allows us to see not just the overall sentiment, but also **when peaks or dips occur**, which may correspond to political events, crises or debates.

In a similar way, we will compute **weekly word counts** by CAP category, which helps us understand **how much attention parliaments devote to each topic over time**. By plotting these trends as heatmaps, we can quickly identify **patterns or shifts in focus** both for individual countries and across all countries.

---

## 1. Setup & Data Loading

This setup (installing/loading) libraries and data loading & filtering steps are the **same as in Tutorial 1**. For detailed explanations, see **Tutorial 1, Sections 1-2**.

```r
# Uncomment and run the following lines if you haven't installed these packages yet
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("lubridate")
# install.packages("pheatmap")
# install.packages("viridis")
```

```r
library(tidyverse)
library(data.table)
library(lubridate)
library(pheatmap)
library(viridis)
```

```r
# Set consistent theme for all plots
theme_set(theme_minimal(base_size = 14))
```

```r
# ---- 1. Define the list of countries and base directory ----
countries <- c(
 "AT", "BA", "BE", "BG", "CZ", "DK", "EE", "ES", "ES-CT", "ES-GA", "ES-PV",
 "FR", "GB", "GR", "HR", "HU", "IS", "IT", "LV", "NL", "NO", "PL", "PT",
 "RS", "SE", "SI", "TR", "UA"
)

base_dir <- "speeches_no_text"

# ---- 2. Choose what columns to read ----
cols_to_keep <- c(
 "id", "date", "lang_code", "lang", "speaker_role", "speaker_MP",
 "speaker_minister", "speaker_party", "speaker_party_name", "party_status",
 "party_orientation", "speaker_id", "speaker_name", "speaker_gender",
 "speaker_birth", "word_count", "CAP_category", "sent3_category",
 "sent6_category", "sent_logit"
)

# ---- 3. Load and filter data ----
all_data <- list()

for (country in countries) {
 file_path <- file.path(
   base_dir,
   paste0("cda1055_dat_ParlaCAP-", country, "_speeches_no_text.tsv.zip")
 )
 
 if (!file.exists(file_path)) {
   message(paste("File not found, skipping:", country))
   next
 }
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
  
  char_cols <- names(chunk)[sapply(chunk, function(x) is.character(x) || is.factor(x))]
  for (col in char_cols) {
    chunk[get(col) %in% c("-", "", " "), (col) := NA]
  }
 
 chunk[, country := country]
 
 filtered_chunk <- chunk[speaker_MP == "MP" & speaker_role == "Regular"]
 
 filtered_chunk <- filtered_chunk[
   !is.na(CAP_category) & CAP_category != "" &
   !is.na(sent3_category) & sent3_category != "" &
   !is.na(sent6_category) & sent6_category != ""
 ]
 
 if (nrow(filtered_chunk) > 0) {
   all_data[[country]] <- filtered_chunk
 }
 
 message(paste("Processed:", country, "- Rows:", nrow(filtered_chunk)))
}

filtered_all <- rbindlist(all_data, use.names = TRUE)
rm(all_data, chunk, filtered_chunk)
gc()

print(paste("All filtered:", nrow(filtered_all), "rows,", ncol(filtered_all), "columns"))
```

```r
# Remove "Mix" and "Other" categories
filtered_all <- filtered_all[!CAP_category %in% c("Mix", "Other")]
filtered_all[, CAP_category := droplevels(as.factor(CAP_category))]
```

---

## 2. Time Range Analysis

Before analyzing sentiment or topic frequency, it's important to **know the time span of our data**. Each country's dataset covers different periods and some dates might be missing. By converting the date column in our data frame to a **Date object**, we make sure that R can handle the dates properly.

After parsing through the converted dates in our data frame, we can check the **earliest and latest speech for each country** and identify the **overlapping period** that is common across all countries. Knowing the overlapping range can help us keep our later cross-country comparisons **fair and meaningful** because we will compare the same time frame where we have valid data for all parliaments.

```r
# ---- 1. Make sure the date in the dataset is parsed to Date ----
filtered_all[, date := as.Date(date)]
filtered_all <- filtered_all[!is.na(date)]

country_list <- unique(filtered_all$country)

min_dates <- c()
max_dates <- c()

for (ctry in country_list) {
 df_country <- filtered_all[country == ctry]
 
 min_date <- min(df_country$date)
 max_date <- max(df_country$date)
 
 min_dates <- c(min_dates, min_date)
 max_dates <- c(max_dates, max_date)
 
 # Print per-country ranges
 cat(sprintf("%s: from %s to %s\n", ctry, min_date, max_date))
}

# ---- 2. Get overlapping range across all countries ----
latest_min_date <- max(as.Date(min_dates, origin = "1970-01-01"))
earliest_max_date <- min(as.Date(max_dates, origin = "1970-01-01"))

cat(sprintf("\nOverlapping date range across all countries: %s to %s\n",
           latest_min_date, earliest_max_date))
```

---

## 3. Group data by time

Now that we know the overlapping date range across all countries, we can **focus our analysis on a specific time frame**. You have two options:
1) Use the **overlapping time frame** that we identified in **Section 2**.
2) Select a **custom time frame** of your choice, if you want to zoom in on specific events or months.

The code below lets you define a custom start and end date, and then filters the dataset to only include speeches within this period.

*Note: If you want to change the date range for a later analysis, you can simply **copy the code blocks below**, modify the values of 'custom_start', 'custom_end', and rename the filtered data to a new variable, like 'filtered_all_custom2', to avoid overwriting the previous filtered dataset. Then run the grouping step code on that new dataset ('filtered_all_custom2'). This way, you can run multiple analyses on different time frames without affecting earlier results.*

```r
# ---- Define a date range ----
custom_start <- as.Date("2020-02-14")  # <-- change to whatever you want
custom_end <- as.Date("2022-02-14")    # <-- change to whatever you want

filtered_all_custom <- filtered_all[date >= custom_start & date <= custom_end]

cat(sprintf("Original speeches: %s\n", format(nrow(filtered_all), big.mark = ",")))
cat(sprintf("Filtered speeches (from %s to %s): %s\n",
           custom_start, custom_end,
           format(nrow(filtered_all_custom), big.mark = ",")))

cat(sprintf("Date range in filtered data: %s to %s\n",
           min(filtered_all_custom$date),
           max(filtered_all_custom$date)))
```

---

### 3.1. Grouping speeches by country, topic and week

Now that we have a dataset filtered to our chosen time frame, we can **organize the speeches by country and CAP category** and summarize them on a **weekly basis**. With the resulting data frame, we can then calculate **total words** spoken per country-topic-week and **average sentiment** per country-topic-week.

```r
# ---- 1. Add week column ----
filtered_all_custom[, week := floor_date(date, "week")]

# ---- 2. Group by country, CAP category and week ----
grouped_df <- filtered_all_custom[, .(
 total_words = sum(word_count, na.rm = TRUE),
 mean_sent = mean(sent_logit, na.rm = TRUE)
), by = .(country, CAP_category, week)]

#Sort by country, CAP category and week
setorder(grouped_df, country, CAP_category, week) 

# ---- 3. Print the first 10 rows ----
head(grouped_df, 10)
```

*Looking at the first 10 rows of our grouped data, we can see that each row represents now a **specific country, CAP category and week**. The columns show:*
- *'total_words': the total number of words spoken in that country about that CAP category during that week.*
- *'mean_sent': the average sentiment score of the speeches for that country-topic-week.*

*Note: Weeks with 'total_words = 0' are gaps in the data for that topic. Also, we should interpret the 'NA' values in 'mean_sent' as 'no speeches to measure sentiment'. When visualizing trends over time, these gaps can appear as missing values or blank cells in heatmaps.*

---

## 4. Changes in sentiment across CAP categories over time. Example: Serbia (RS)

Now that we have filtered the speeches to a specific time frame and grouped them by week, we can narrow our analysis to **one country** to see detailed patterns in sentiment across different topics.

The question **Which CAP categories in Serbia (RS) show the most positive or negative sentiment over time?** was chosen because it allows us to **zoom in on a single country** and examine the **tone of parliamentary discussions at a more granular level**. By focusing on one parliament we can **identify topic-specific trends** (categories that have consistently more pos. or neg. sentiment), **spot temporal dynamics** (spikes or drops that might correspond to important events) and **highlight outliers or unusual patterns**.

*Note: As the overall time frame, we chose the two most recent years in the available data for RS (14.02.2020 - 14.02.2022) which also cover the first couple of months of the COVID outbreak.*

```r
# ---- 1. Choose a target country ----
target_country <- "RS"  # Change this to any country code: "DE", "IT", "GB", etc.

# ---- 2. Filter for the country ----
country_df <- filtered_all_custom[country == target_country]
country_df[, week := floor_date(date, "week")]
country_df[, week_label := format(week, "%Y-%m-%d")]

# ---- 3. Group by week and CAP category, calculate mean sentiment ----
weekly_sentiment <- country_df[, .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                               by = .(week_label, CAP_category)]

# ---- 4. Pivot to wide format for heatmap ----
heatmap_data <- dcast(weekly_sentiment, CAP_category ~ week_label, value.var = "mean_sent")
heatmap_matrix <- as.matrix(heatmap_data[, -1])
rownames(heatmap_matrix) <- heatmap_data$CAP_category

# ---- 5. Plot the heatmap ----
pheatmap(
 heatmap_matrix,
 color = colorRampPalette(c("red", "yellow", "green"))(100),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = paste("Fig. 1, Weekly Average Sentiment per Topic -", target_country,
              "\n", custom_start, "-", custom_end,
              "\n(Green = More Positive, Red = More Negative)"),
 fontsize_row = 8,
 fontsize_col = 5,
 angle_col = 90,
 na_col = "grey90"
)
```

*Figure 1, visualized the **weekly average sentiment** of parliamentary debates across all **21 major policy topics** over a two-year period. The key finding is that sentiment across almost all topics was significantly more negative in **early 2020**, which also marks the onset of the COVID-19 pandemic. This drop in sentiment across topics is followed by cluster of missing data for majority of topics, indicating that the parliament might have been in lockdown too (similar gaps can also be visible during the official summer breaks of a parliament) or focused much more on a few topics like 'Government Operations' as a reaction to the COVID outbreak.*

*Connected to that, the 'Health' topic shows one of the most dramatic and sustained negative shifts. It turns dark red and stays predominantly negative for the entire period. This could reflect the ongoing crisis. Economy-related topics, such as 'Macroeconomics' or 'Social Welfare' also show strong negative reactions which could indicate the economic uncertainty of the early pandemic. 'Government Operations' also turns very red in this period.*

---

### 4.1. Time series for a specific topic (Example: Serbia (RS))

Figure 1 shows sentiment across all topics in the selected country (Serbia), highlighting general trends. To get a clearer overview of topic-specific dynamics over time (and for better readability of the dates), we can examine the weekly sentiment for one chosen CAP category.

In the following part, 'Health' was chosen because it showed one of the most pronounced sentiment shifts during the chosen time period, reflecting the parliament's response to the COVID-19 outbreak.

```r
# ---- 1. Choose target country and CAP category ----
target_country <- "RS"
category <- "Health"

# ---- 2. Filter the dataset for the selected country and category ----
category_df <- filtered_all_custom[country == target_country & CAP_category == category]
category_df[, week := floor_date(date, "week")]

# ---- 3. Group by week and calculate mean sentiment ----
category_over_time <- category_df[, .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                 by = week][order(week)]
category_over_time[, week_label := format(week, "%Y-%m-%d")]

# ---- 4. Pivot for single-row heatmap ----
heatmap_data <- as.matrix(t(category_over_time$mean_sent))
colnames(heatmap_data) <- category_over_time$week_label
rownames(heatmap_data) <- category

# ---- 5. Plot Heatmap ----
pheatmap(
 heatmap_data,
 color = colorRampPalette(c("red", "yellow", "green"))(100),
 breaks = seq(0, 4.5, length.out = 101),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = paste("Fig. 2, Weekly Average Sentiment —", category, "in", target_country,
              "\n", custom_start, "-", custom_end),
 fontsize_col = 6,
 angle_col = 90,
 na_col = "grey90"
)
```

*Figure 2 displays a concise overview of the weekly avg. sentiment of the Serbian parliament regarding all speeches related to 'Health'. We can see a sharp and lasting negative shift at the beginning of our chosen time frame (March 2020), coinciding with the outbreak of COVID-19. After this initial crash, the 'Health' topic remains largely negative, with a particular sharp drop in May 2021, followed by a noticeable rebound in the weeks that followed. This pattern—where a shock event (COVID-19) triggers increased attention AND more negative sentiment goes well with what we know about 'bad news' driving agenda change, several studies have found that negative news has stronger agenda-setting effects. Also, short-term fluctuations followed by rebounds are characteristic of what CAP researchers call 'attention cycling'. Focus to an issue cannot be sustained indefinitely and attention eventually returns toward baseline levels. To investigate in detail whether there were events causing the sentiment spike, one could examine **concordances** of the speeches - but that would go beyond the scope of this tutorial.*

At the same time, this approach can also be flipped around: if you want to explore how specific events (e.g. elections, protests, war etc.) influenced the tone of debates in parliament, you can simply choose a CAP category and trace how its sentiment shifted over time.

---

### 4.2. How does the sentiment change towards 'Health' differ in parliamentary debates in RS, HR and HU? (cross-country comparison)

So far, we have explored sentiment within Serbia's parliament to understand how debates on 'Health' evolved over time. While this shows the internal dynamics of one country, it doesn't tell us whether similar shifts occurred elsewhere. To put Serbia's trajectory into context, we can compare it with neighboring countries like **Croatia (HR)** and **Hungary (HU)**.

```r
# ---- 1. Choose the CAP category you want to focus on ----
category <- "Health"

# ---- 2. Filter dataset for the category and selected countries ----
countries_to_compare <- c("RS", "HR", "HU")
category_df <- filtered_all_custom[
 CAP_category == category & country %in% countries_to_compare
]

# ---- 3. Add week column ----
category_df[, week := floor_date(date, "week")]
category_df[, week_label := format(week, "%Y-%m-%d")]

# ---- 4. Group by country + week and calculate mean sentiment ----
weekly_sentiment <- category_df[, .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                by = .(country, week_label)]

# ---- 5. Pivot for heatmap ----
heatmap_data <- dcast(weekly_sentiment, country ~ week_label, value.var = "mean_sent")
heatmap_matrix <- as.matrix(heatmap_data[, -1])
rownames(heatmap_matrix) <- heatmap_data$country

# ---- 6. Plot Heatmap ----
pheatmap(
 heatmap_matrix,
 color = colorRampPalette(c("red", "yellow", "green"))(100),
 breaks = seq(0, 4.5, length.out = 101),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = paste("Fig. 3, Weekly Average Sentiment Heatmap —", category,
              "\n", paste(countries_to_compare, collapse = ", "),
              "\n", custom_start, "-", custom_end),
 fontsize_col = 5,
 angle_col = 90,
 na_col = "grey90"
)
```

*Figure 3 highlights that Serbia's sharp drop, the big gap in the data and subsequent recovery of sentiment in weekly sentiment on 'Health' debates is unique in this cross-country comparison. There are also gaps in the Hungary (HU) and Croatia (HR) datasets but it is likely that these reflect the annual parliamentary summer break. Prior to this, both countries exhibit consistently negative discussions on health. Croatia's parliament shows more fluctuations between positive and negative sentiment, while Hungary maintains a persistently negative baseline, with a pronounced drop in late winter 2021. This drop coincides with the second wave of COVID-19 and rising case numbers and deaths, which likely contributed to the heightened negativity in parliamentary speeches. To confirm this theory, a closer look into the actual parliamentary speeches, e.g. a keyword analysis, might be necessary.*

---

In the next part of the tutorial, we will shift our focus from **sentiment analysis** - how parliaments reacted emotionally to major events like the COVID-19 pandemic - to **topic attention**, that is, how much speech was devoted to specific topics and how has this changed over time? This approach will help answer questions such as:

## 5. How did attention to 'Health' evolve in Serbia, Hungary and Croatia?

In order to reply to this question we define the right CAP category and the countries we want to investigate in the first part of our code (below). Then we have to make sure that the 'date' column in our data frame is treated as actual dates ('Date') instead of text. Then we create a new column called 'week', which groups all speeches by the week they were delivered.

After that we sum up the **total number of words spoken** about the chosen topic for each country and each week. Then, the data is reshaped so that **countries become rows** and **weeks become columns**. Additionally, normalize the weekly word counts by dividing each week's word counts by the total for that week across all countries. This prevents a country with a generally larger parliament from dominating the chart. Finally, we create a heatmap.

```r
# ---- 1. Choose the CAP category and countries to focus on ----
category <- "Health"  # <-- change to any CAP category you want
countries_to_plot <- c("RS", "HR", "HU")  # <-- change or add countries here

# ---- 2. Filter the dataset for the chosen category and countries ----
category_df <- filtered_all_custom[
 CAP_category == category & country %in% countries_to_plot
]

# ---- 3. Add week column ----
category_df[, week := floor_date(date, "week")]
category_df[, week_label := format(week, "%Y-%m-%d")]

# ---- 4. Group by country + week and sum word counts ----
weekly_wordcount <- category_df[, .(word_count = sum(word_count, na.rm = TRUE)),
                                by = .(country, week_label)]

# ---- 5. Pivot to get countries as rows, weeks as columns ----
heatmap_data <- dcast(weekly_wordcount, country ~ week_label,
                     value.var = "word_count", fill = 0)
heatmap_matrix <- as.matrix(heatmap_data[, -1])
rownames(heatmap_matrix) <- heatmap_data$country

# ---- 6. Normalize per week for fair cross-country comparison ----
col_sums <- colSums(heatmap_matrix, na.rm = TRUE)
col_sums[col_sums == 0] <- 1  # Avoid division by zero
heatmap_norm <- sweep(heatmap_matrix, 2, col_sums, "/")

# ---- 7. Plot the heatmap ----
pheatmap(
 heatmap_norm,
 color = colorRampPalette(c("lightyellow", "lightblue", "darkblue"))(100),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = paste("Fig. 4, Weekly Attention to", category, "Across Selected Countries",
              "\n(", paste(countries_to_plot, collapse = ", "), ")"),
 fontsize_col = 5,
 angle_col = 90,
 na_col = "grey90"
)
```

*Figure 4 shows that the Hungarian parliament consistently devoted the most attention to the 'Health' topic among the three selected countries, with particularly strong activity in the first half and autumn of 2020. Croatia's parliament also exhibits noticeable spikes in discussion on health during the same periods. Overall, both Hungary and Croatia display higher levels of attention to this topic than Serbia throughout the year.*

---

## 6. Conclusion

In this tutorial, we examined parliamentary debates **over time** from two perspectives:

1) **Sentiment**: Weekly average sentiment can highlight how events (like COVID-19) shaped the tone. This can reflect in sharp drops or a prolonged negative baseline, as we saw in 'Health' debates. The cross-country comparisons revealed differences in how much sentiment fluctuates and how strongly opinions were expressed.

2) **Topic attention**: Weekly word counts showed which parliaments focused most on specific topics. Hungary consistently emphasized 'Health', Croatia had spikes at key moments, and Serbia's attention remained lower.

Together, these analyses demonstrate how tracking sentiment and attention over time provides insights into both **what parliaments discussed** and **how much attention they devoted** to each topic.

## What's Next?

In the next tutorial (5), we will build on all previous tutorials by looking at the coalition-opposition behavior across all parliaments to uncover structural patterns in their behavior.

---
