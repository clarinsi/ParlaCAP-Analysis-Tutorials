# Tutorial 5 - Coalition vs. Opposition Dynamics

This tutorial pulls together the skills and methods we developed in the previous tutorials - analyzing **topic distributions and sentiment** - and applies them to a new focus: **party alignment**. By comparing **coalition and opposition** behaviors, we not only deepen our understanding of a single parliament (like we did in **Tutorial 2, Section 3.4**), but also scale the analysis across multiple countries. This demonstrates the **transferability of our workflow** and allows us to uncover patterns that are **robust across different parliamentary systems**.

What we will do:
1) Compute **mean sentiment** by country and CAP category separately for coalition and opposition parties, visualize with heatmaps and examine cross-country patterns using cosine distance and dendrograms.
2) Compute **topic shares** (proportion of words per CAP category) for coalition and opposition parties, visualize them with heatmaps and explore cross-country patterns using cosine distance and dendrograms.
3) Analyze **relative word count differences** between coalition and opposition to see which parliaments or CAP categories are dominated by each party type.

With this approach, we aim to answer **possible research questions**, such as:
- *Do Coalitions and Oppositions across parliaments agree on what's important?*
- *Do Coalitions and Oppositions across parliaments express similar tones on the same topic?*

---

## 1. Setup & Data Loading

This setup (installing/loading) libraries and data loading & filtering steps are the **same as in Tutorial 1**. For detailed explanations, see **Tutorial 1, Sections 1-2**.

```r
# Uncomment and run the following lines if you haven't installed these packages yet
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("proxy")
# install.packages("pheatmap")
# install.packages("dendextend")
```

```r
library(tidyverse)
library(data.table)
library(proxy)
library(pheatmap)
library(dendextend)
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

## 2. Check for Missing Data

For some countries, the dataset might not include annotated speeches from opposition. In these cases, we cannot look at the sentiment scores for the opposition and we cannot compare their tone to the coalition parties. This should be kept in mind when interpreting the heatmaps and summary statistics, since the absence of coalition/opposition data may bias cross-country comparisons.

With the help of the code below, you can easily test which countries have no annotated opposition speeches.

```r
# Count party_status per country
party_counts <- filtered_all[, .N, by = .(country, party_status)]
party_counts_wide <- dcast(party_counts, country ~ party_status, value.var = "N", fill = 0)

# Find countries with zero Opposition speeches
if ("Opposition" %in% names(party_counts_wide)) {
  missing_opposition <- party_counts_wide[Opposition == 0, country]
} else {
  missing_opposition <- party_counts_wide$country
}

cat("Countries with NO Opposition-annotated speeches:\n")
print(missing_opposition)
```

---

## 3. Mean Sentiment across CAP Categories Filtered by Party Status

In **Tutorial 2, Section 3.4**, we first explored how sentiment varies across CAP categories when filtered by party status. There, we focused on a single country. Here, we extend that analysis to **all countries together**, using tools from previous tutorials, such as heatmaps, cosine distances and dendrograms, to uncover broader structural patterns in European parliamentary speech.

Our main interest in this section is in comparing the **tone of coalition and opposition parties** when they address the same policy areas. To do this, we compute the mean sentiment scores for each country and CAP category, both overall and separately for coalition and opposition parties. We then summarize these scores with descriptive statistics (with an option to export them to TSV for further research) and visualize them using heatmaps (as in Tutorial 2).

By looking across countries, we can test the hypothesis from **Tutorial 2, Section 3.4**, that governments tend to frame issues in a more positive light, while opposition parties adopt a more critical or negative tone across policy debates.

```r
# ---- 1. Compute mean sentiment by country and CAP category ----

# Overall (both parties combined)
overall_sentiment <- filtered_all[, .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                   by = .(country, CAP_category)]
overall_wide <- dcast(overall_sentiment, country ~ CAP_category, value.var = "mean_sent")

# Coalition
coalition_sentiment <- filtered_all[party_status == "Coalition",
                                     .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                     by = .(country, CAP_category)]
coalition_wide <- dcast(coalition_sentiment, country ~ CAP_category, value.var = "mean_sent")

# Opposition
opposition_sentiment <- filtered_all[party_status == "Opposition",
                                      .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                      by = .(country, CAP_category)]
opposition_wide <- dcast(opposition_sentiment, country ~ CAP_category, value.var = "mean_sent")

# ---- 1.1 Ensure all countries are represented ----
all_countries <- unique(filtered_all$country)

# Function to reindex and fill missing values
reindex_df <- function(df, all_countries) {
  missing_countries <- setdiff(all_countries, df$country)
  if (length(missing_countries) > 0) {
    missing_rows <- data.table(country = missing_countries)
    for (col in names(df)[-1]) {
      missing_rows[[col]] <- NA_real_
    }
    df <- rbindlist(list(df, missing_rows), fill = TRUE)
  }
  df <- df[order(country)]
  return(df)
}

coalition_wide <- reindex_df(coalition_wide, all_countries)
opposition_wide <- reindex_df(opposition_wide, all_countries)

# Convert to matrices
coalition_matrix <- as.matrix(coalition_wide[, -1])
rownames(coalition_matrix) <- coalition_wide$country
coalition_matrix[is.na(coalition_matrix)] <- 0

opposition_matrix <- as.matrix(opposition_wide[, -1])
rownames(opposition_matrix) <- opposition_wide$country
opposition_matrix[is.na(opposition_matrix)] <- 0

overall_matrix <- as.matrix(overall_wide[, -1])
rownames(overall_matrix) <- overall_wide$country

# ---- 2. Print summary statistics ----
print_stats <- function(mat, name) {
  values <- as.vector(mat)
  values <- values[!is.na(values) & values != 0]
  cat(sprintf("\n%s Mean Sentiment:\n", name))
  cat(sprintf("Mean: %.3f\n", mean(values)))
  cat(sprintf("Std: %.3f\n", sd(values)))
  cat(sprintf("Min: %.3f\n", min(values)))
  cat(sprintf("Max: %.3f\n", max(values)))
}

cat("MEAN SENTIMENT SUMMARY ACROSS ALL COUNTRIES\n")
cat(strrep("=", 50), "\n")

print_stats(overall_matrix, "Overall (All Parties)")
print_stats(coalition_matrix, "Coalition")
print_stats(opposition_matrix, "Opposition")

# ---- 3. Create detailed mean sentiment by country ----
country_stats <- data.table(
  country = all_countries,
  overall_mean = rowMeans(overall_matrix, na.rm = TRUE),
  coalition_mean = rowMeans(coalition_matrix, na.rm = TRUE),
  opposition_mean = rowMeans(opposition_matrix, na.rm = TRUE)
)
country_stats[, difference := coalition_mean - opposition_mean]
country_stats[, opposition_missing := country %in% missing_opposition]

cat("\n", strrep("=", 50), "\n")
cat("DETAILED MEAN SENTIMENT BY COUNTRY\n")
cat(strrep("=", 50), "\n")
print(country_stats)

# ---- 3.1 Save as TSV ----
output_path <- "detailed_mean_sentiment_by_country.tsv"
fwrite(country_stats, output_path, sep = "\t")
cat(sprintf("\nSaved detailed sentiment table to %s\n", output_path))

# ---- 4. Determine shared color scale ----
all_values <- c(as.vector(coalition_matrix), as.vector(opposition_matrix))
all_values <- all_values[all_values != 0 & !is.na(all_values)]
vmin <- min(all_values)
vmax <- max(all_values)

# ---- 4.1. Heatmap for Coalition sentiment ----
pheatmap(
  coalition_matrix,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("red", "yellow", "green"))(100),
  breaks = seq(vmin, vmax, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 1a: Coalition Mean Sentiment by Country and CAP Category",
  fontsize = 8,
  fontsize_number = 6,
  angle_col = 45,
  na_col = "grey90"
)

# ---- 4.2. Heatmap for Opposition sentiment ----
pheatmap(
  opposition_matrix,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("red", "yellow", "green"))(100),
  breaks = seq(vmin, vmax, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 1b: Opposition Mean Sentiment by Country and CAP Category",
  fontsize = 8,
  fontsize_number = 6,
  angle_col = 45,
  na_col = "grey90"
)

# ---- 5. Print actual data tables ----
cat("\n", strrep("=", 50), "\n")
cat("COALITION SENTIMENT DATA TABLE\n")
cat(strrep("=", 50), "\n")
print(round(coalition_matrix, 3))

cat("\n", strrep("=", 50), "\n")
cat("OPPOSITION SENTIMENT DATA TABLE\n")
cat(strrep("=", 50), "\n")
print(round(opposition_matrix, 3))
```

*In the overall statistics, we see that the **mean sentiment** across all countries and categories is around **2.0**. When broken down by party status, the **coalition** has a higher mean sentiment while the opposition is lower. This difference is also visible in the heatmaps: **Fig. 1a** appears generally greener than **Fig. 1b**, reflecting the more positive tone of coalition speeches.*

*The coalition heatmap (Fig. 1a) allows us to identify specific CAP categories and countries with particularly positive or negative sentiment. For example, the coalition in **Latvia (LV)** and **Great Britain (GB)** shows very positive sentiment overall. However, it's important to note that this could partly result from the limited number of annotated coalition speeches in some countries.*

With the code below, you can check for a selected country how many annotated speeches we have for each 'party_status'.

*The opposition heatmap (Fig. 1b) shows a generally more negative tone. Several countries have missing opposition data, including **Estonia (EE), Iceland (IS), Norway (NO) and Sweden (SE)**. The most positive opposition sentiment is observed in **Latvia (LV)** and **Bosnia (BA)**, while the most negative occurs in **Spain-Galicia (ES-GA)** and **Turkey (TR)** which is not surprising, considering that their coalitions had negative sentiment scores as well, indicating that their parliaments discuss in a more negative tone overall.*

```r
# ---- Select a country ----
selected_country <- "GB"  # change to any country you want

# Filter the dataset for that country
country_data <- filtered_all[country == selected_country]

# Count number of speeches per party_status
annotation_counts <- country_data[, .N, by = party_status]

cat(sprintf("Annotation counts for %s:\n", selected_country))
print(annotation_counts)
```

If you see that a country has **very few coalition/opposition-annotated speeches but a large number for '-'**, the *mean sentiment for coalition* may not be representative and can appear artificially high or low.

---

## 3.1. Cosine Distance of Sentiment Patterns by Party Status and CAP Category

With the help of cosine distances (see Tutorial 3, section 3 for more details), we can now **compute the similarity of sentiment patterns between parliaments**, party statuses and CAP categories. By representing each country-party combination as a vector of mean sentiment across CAP categories, the cosine distance measures the angle between these vectors, giving us a sense of relative similarity independent of absolute sentiment scores.

This method allows us to:
- Identify countries whose coalition or opposition parties express similar sentiment patterns.
- Detect outlier countries with unusually positive or negative tones in specific policy areas.
- Compare coalition vs. opposition patterns across countries to see if certain CAP categories consistently trigger more positive or negative sentiment depending on party status.

The resulting heatmap and dendrogram provide a nice visual overview of these relationships.

```r
# ---- 1. Compute cosine distance matrices ----
coalition_dist <- as.matrix(dist(coalition_matrix, method = "cosine"))
opposition_dist <- as.matrix(dist(opposition_matrix, method = "cosine"))

# ---- 2. Convert to data frames for plotting ----
coalition_dist_df <- as.data.frame(coalition_dist)
opposition_dist_df <- as.data.frame(opposition_dist)

# ---- 3. Plot cosine distance heatmaps ----

# Exclude countries without coalition/opposition data
exclude_countries <- c("EE", "IS", "NO", "SE")

# Filter matrices to remove these countries
coalition_dist <- coalition_dist[!rownames(coalition_dist) %in% exclude_countries,
                                  !colnames(coalition_dist) %in% exclude_countries]

opposition_dist <- opposition_dist[!rownames(opposition_dist) %in% exclude_countries,
                                    !colnames(opposition_dist) %in% exclude_countries]

# Coalition
pheatmap(
  coalition_dist,
  color = colorRampPalette(c("green", "yellow", "red"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 2a, Cosine Distance: Coalition Sentiment Patterns Across Countries",
  fontsize = 8,
  na_col = "grey90"
)

# Opposition
pheatmap(
  opposition_dist,
  color = colorRampPalette(c("green", "yellow", "red"))(100),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 2b, Cosine Distance: Opposition Sentiment Patterns Across Countries",
  fontsize = 8,
  na_col = "grey90"
)

# ---- 4. Create dendrograms ----

# Coalition
hc_coalition <- hclust(as.dist(coalition_dist), method = "ward.D")
plot(hc_coalition, main = "Fig. 3a, Dendrogram: Coalition Sentiment Patterns",
     xlab = "", sub = "", cex = 0.7)

# Opposition
hc_opposition <- hclust(as.dist(opposition_dist), method = "ward.D")
plot(hc_opposition, main = "Fig. 3b, Dendrogram: Opposition Sentiment Patterns",
     xlab = "", sub = "", cex = 0.7)
```
*Looking at heatmaps, Galicia (ES-GA) is the largest outlier with sentiment patterns that differ from almost every other country. Bosnia, Turkey, and Catalonia also stand apart from the rest. Most of Europe shows similar coalition sentiment (the large green area), meaning governing parties across these countries frame policy topics in comparable ways.
For opposition parties, Galicia fits in better here than it did for coalitions. Overall patterns show that opposition parties vary more from country to country than coalition parties do. Comparing the two dendrograms reveals that coalition and opposition parties cluster differently across countries. Governing and opposing parties adopt distinct rhetorical strategies that apparently don't follow some clear-cut cross-national pattern, this is especially visible in Turkey or in Spanish parliaments. 
---

## 3.2. Nearest Neighbors

In this section, we compute the **three nearest neighbors** for each country based on the cosine distances of mean sentiment across CAP categories. We do this separately for coalition and opposition parties, taking care to exclude countries with missing opposition data to avoid misleading results.

```r
# ---- Helper function to find nearest neighbors ----
find_nearest_neighbors <- function(dist_matrix, n = 3) {
  neighbors <- data.frame(country = rownames(dist_matrix))
  
  for (i in 1:nrow(dist_matrix)) {
    distances <- dist_matrix[i, ]
    distances[i] <- Inf  # Exclude self-comparison
    
    nearest_idx <- order(distances)[1:n]
    neighbors[i, paste0("NN", 1:n)] <- colnames(dist_matrix)[nearest_idx]
  }
  
  return(neighbors)
}

# ---- 1. Coalition nearest neighbors ----
nearest_neighbors_coalition <- find_nearest_neighbors(coalition_dist, 3)

cat("Coalition: 3 Nearest Neighbors per Country\n")
print(nearest_neighbors_coalition)

# ---- 2. Opposition nearest neighbors (exclude missing opposition countries) ----
# Remove countries with missing opposition data
opposition_matrix_clean <- opposition_matrix[!rownames(opposition_matrix) %in% missing_opposition, ]
opposition_dist_clean <- as.matrix(dist(opposition_matrix_clean, method = "cosine"))

nearest_neighbors_opposition <- find_nearest_neighbors(opposition_dist_clean, 3)

cat("\nOpposition: 3 Nearest Neighbors per Country\n")
print(nearest_neighbors_opposition)
```

*The nearest-neighbor analysis reveals some patterns of similarity across countries. For the **coalition**, some countries cluster with geographic partners, such as **DK & NO** or **FR & BE**. In the **opposition**, countries like **AT, DK and SI** repeatedly appear as neighbors, indicating broadly similar opposition sentiment across these parliaments, whereas outliers like **ES-CT, LV and TR** form more isolated clusters. Overall, the results confirm that while many countries share comparable sentiment patterns within coalition and opposition, certain countries stand out as distinctive. These patterns can guide a potentially more focused comparison and serve as a foundation for research on typical and exceptional parliamentary behavior.*

---

## 4. Topic Share across CAP Categories (Filtered by Party Status)

In this section, we analyze the **topic share across CAP categories**, based on word counts in speeches. By comparing coalition and opposition parties, we can identify which policy areas each party type emphasized most, detect patterns of attention across parliaments, and assess whether differences in sentiment are accompanied by differences in topic focus. This analysis provides a complementary view to the sentiment study and helps build a fuller picture of parliamentary discourse.

```r
# ---- 1. Compute topic share (proportion of words) by country and CAP category ----

# Overall (both parties combined)
overall_topic <- filtered_all[, .(total_words = sum(word_count)), by = .(country, CAP_category)]
overall_topic_wide <- dcast(overall_topic, country ~ CAP_category, value.var = "total_words", fill = 0)
overall_topic_matrix <- as.matrix(overall_topic_wide[, -1])
rownames(overall_topic_matrix) <- overall_topic_wide$country
overall_topic_matrix <- overall_topic_matrix / rowSums(overall_topic_matrix)

# Coalition
coalition_topic <- filtered_all[party_status == "Coalition",
                                 .(total_words = sum(word_count)),
                                 by = .(country, CAP_category)]
coalition_topic_wide <- dcast(coalition_topic, country ~ CAP_category, value.var = "total_words", fill = 0)
coalition_topic_wide <- reindex_df(coalition_topic_wide, all_countries)
coalition_topic_matrix <- as.matrix(coalition_topic_wide[, -1])
rownames(coalition_topic_matrix) <- coalition_topic_wide$country
coalition_topic_matrix[is.na(coalition_topic_matrix)] <- 0
row_sums <- rowSums(coalition_topic_matrix)
row_sums[row_sums == 0] <- 1
coalition_topic_matrix <- coalition_topic_matrix / row_sums

# Opposition
opposition_topic <- filtered_all[party_status == "Opposition",
                                  .(total_words = sum(word_count)),
                                  by = .(country, CAP_category)]
opposition_topic_wide <- dcast(opposition_topic, country ~ CAP_category, value.var = "total_words", fill = 0)
opposition_topic_wide <- reindex_df(opposition_topic_wide, all_countries)
opposition_topic_matrix <- as.matrix(opposition_topic_wide[, -1])
rownames(opposition_topic_matrix) <- opposition_topic_wide$country
opposition_topic_matrix[is.na(opposition_topic_matrix)] <- 0
row_sums <- rowSums(opposition_topic_matrix)
row_sums[row_sums == 0] <- 1
opposition_topic_matrix <- opposition_topic_matrix / row_sums

# ---- 2. Print summary statistics ----
print_topic_stats <- function(mat, name) {
  values <- as.vector(mat)
  values <- values[!is.na(values)]
  cat(sprintf("\n%s Topic Share (proportion of words):\n", name))
  cat(sprintf("Mean: %.3f\n", mean(values)))
  cat(sprintf("Std: %.3f\n", sd(values)))
  cat(sprintf("Min: %.3f\n", min(values)))
  cat(sprintf("Max: %.3f\n", max(values)))
}

cat("TOPIC SHARE SUMMARY ACROSS ALL COUNTRIES\n")
cat(strrep("=", 50), "\n")
print_topic_stats(overall_topic_matrix, "Overall (All Parties)")
print_topic_stats(coalition_topic_matrix, "Coalition")
print_topic_stats(opposition_topic_matrix, "Opposition")

# ---- 3. Detailed topic share by country ----
country_topic_stats <- data.table(
  country = all_countries,
  overall_mean = rowMeans(overall_topic_matrix, na.rm = TRUE),
  coalition_mean = rowMeans(coalition_topic_matrix, na.rm = TRUE),
  opposition_mean = rowMeans(opposition_topic_matrix, na.rm = TRUE)
)

cat("\n", strrep("=", 50), "\n")
cat("DETAILED TOPIC SHARE BY COUNTRY\n")
cat(strrep("=", 50), "\n")
print(country_topic_stats)

# ---- 3.1 Save as TSV ----
output_path <- "detailed_topic_share_by_country.tsv"
fwrite(country_topic_stats, output_path, sep = "\t")
cat(sprintf("\nSaved detailed topic share table to %s\n", output_path))

# ---- 4. Determine shared color scale ----
all_values <- c(as.vector(coalition_topic_matrix), as.vector(opposition_topic_matrix))
all_values <- all_values[!is.na(all_values)]
vmin <- min(all_values)
vmax <- max(all_values)

# ---- 4.1 Heatmap for Coalition topic share ----
pheatmap(
  coalition_topic_matrix,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("lightyellow", "lightblue", "darkblue"))(100),
  breaks = seq(vmin, vmax, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 4a: Coalition Topic Share by Country and CAP Category",
  fontsize = 8,
  fontsize_number = 6,
  angle_col = 45,
  na_col = "grey90"
)

# ---- 4.2 Heatmap for Opposition topic share ----
pheatmap(
  opposition_topic_matrix,
  display_numbers = TRUE,
  number_format = "%.2f",
  color = colorRampPalette(c("lightyellow", "lightblue", "darkblue"))(100),
  breaks = seq(vmin, vmax, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 4b: Opposition Topic Share by Country and CAP Category",
  fontsize = 8,
  fontsize_number = 6,
  angle_col = 45,
  na_col = "grey90"
)

# ---- 5. Compute cosine distances for dendrograms ----
coalition_topic_dist <- as.matrix(dist(coalition_topic_matrix, method = "cosine"))
opposition_topic_dist <- as.matrix(dist(opposition_topic_matrix, method = "cosine"))

# ---- 5.1 Dendrogram for Coalition Topic Share ----
hc_coalition_topic <- hclust(as.dist(coalition_topic_dist), method = "ward.D")
plot(hc_coalition_topic,
     main = "Fig. 5a: Dendrogram of Coalition Topic Share Patterns Across Countries",
     xlab = "", sub = "", cex = 0.7)

# ---- 5.2 Dendrogram for Opposition Topic Share ----
# Remove countries with missing opposition data
opposition_topic_clean <- opposition_topic_matrix[!rownames(opposition_topic_matrix) %in% missing_opposition, ]
opposition_topic_dist_clean <- as.matrix(dist(opposition_topic_clean, method = "cosine"))

hc_opposition_topic <- hclust(as.dist(opposition_topic_dist_clean), method = "ward.D")
plot(hc_opposition_topic,
     main = "Fig. 5b: Dendrogram of Opposition Topic Share Patterns Across Countries",
     xlab = "", sub = "", cex = 0.7)
```

*Looking at the statistics summary, we see that the mean proportion of words per CAP category is quite low (mean: ~0.048) with some variation. Coalition parties show a very similar pattern overall, while opposition parties have a lower average share.*

*The coalition heatmap (Fig. 4a) highlights that topics like **Macroeconomics, Government Operations, Health and Law and Crime** dominate in most countries which aligns with our findings from previous tutorials. A few outliers stand out, such as **ES-CT for Civil Rights** and **BA for Government Operations**. The opposition heatmap (Fig. 4b) shows a very similar distribution across CAP categories, which indicates that both coalition and opposition tend to focus on largely the same policy areas.*

*To see **how similar countries are to each other** regarding the topic share across topics and party status, we look at the dendrograms. In the coalition dendrogram (Fig. 5a), we observe **many small clusters of closely aligned parliaments**, such as **Denmark (DK) & Sweden (SE), Serbia (RS) & Ukraine (UA) and Greece (GR) & Portugal (PT)**. At the same time, some countries stand apart, like **Bosnia & Herzegovina (BA) and ES-CT**, indicating that their topic focus is less similar to the rest of the coalition parties.*

---

## 4.2. Nearest Neighbors

To make these relationships, observed in the heatmaps and dendrograms from Section 4.1, more precise and quantitative, we can identify the **three nearest neighbors for each country** using cosine distances.

```r
# ---- 1. Coalition nearest neighbors based on topic share ----
nearest_neighbors_coalition_topic <- find_nearest_neighbors(coalition_topic_dist, 3)

cat("Coalition: 3 Nearest Neighbors per Country (Topic Share)\n")
print(nearest_neighbors_coalition_topic)

# ---- 2. Opposition nearest neighbors based on topic share ----
nearest_neighbors_opposition_topic <- find_nearest_neighbors(opposition_topic_dist_clean, 3)

cat("\nOpposition: 3 Nearest Neighbors per Country (Topic Share)\n")
print(nearest_neighbors_opposition_topic)
```

*The nearest neighbors analysis shows that among the coalition countries, we can observe regional clustering as well: **AT and HU** form a close group, as well as **HR & SI** and **BG & CZ**. Some countries, such as **ES-CT and TR** are more distinct which aligns with our previous observations.*

*For the opposition, a similar pattern can be observed, though there is generally more variation between countries, which is likely due to differences in opposition size or focus.*

*These nearest neighbors complement the dendrograms, quantifying similarities that were visually apparent in the clusters. This offers a concrete basis for further parliamentary analyses.*

---

## 5. Relative Word Count Differences by Party Status and CAP Category

In this section, we compare how much coalition and opposition parties speak across CAP categories. Using the relative difference metric (Coalition - Opposition / (Coalition + Opposition)), we can see which parliaments or CAP categories are dominated by coalition speeches, which by opposition and which are more balanced. The heatmap provides a quick visual overview with red indicating a stronger presence of coalition speeches and blue indicating opposition dominance and white indicating roughly equal participation. This will help contextualize the results from sentiment (section 3) and topic share (section 4) analyses by highlighting the actual volume of debate contributions per coalition and opposition.

```r
# ---- 0. Identify countries with missing opposition data ----
cat("Countries with no opposition data:", paste(missing_opposition, collapse = ", "), "\n")

# ---- 1. Filter out missing opposition countries from main data ----
filtered_all_clean <- filtered_all[!country %in% missing_opposition]

# ---- 2. Pivot table: country x CAP_category, separate columns for Coalition/Opposition ----
pivot_df <- filtered_all_clean[party_status %in% c("Coalition", "Opposition"),
                                .(word_count = sum(word_count)),
                                by = .(country, CAP_category, party_status)]

pivot_wide <- dcast(pivot_df, country + CAP_category ~ party_status,
                    value.var = "word_count", fill = 0)

# ---- 3. Calculate relative difference (Coalition - Opposition) / Total ----
pivot_wide[, total := Coalition + Opposition]
pivot_wide[, rel_diff := (Coalition - Opposition) / (total + 1e-10)]

# ---- 4. Pivot for heatmap ----
rel_pivot <- dcast(pivot_wide, country ~ CAP_category, value.var = "rel_diff")
rel_matrix <- as.matrix(rel_pivot[, -1])
rownames(rel_matrix) <- rel_pivot$country

# ---- 5. Plot heatmap ----
pheatmap(
  rel_matrix,
  color = colorRampPalette(c("blue", "white", "red"))(100),
  breaks = seq(-1, 1, length.out = 101),
  cluster_rows = FALSE,
  cluster_cols = FALSE,
  main = "Fig. 6, Relative Word Count Differences by Party Status\n(Red: Coalition emphasis | Blue: Opposition emphasis | White: Balanced)",
  fontsize = 8,
  angle_col = 45,
  na_col = "grey90"
)
```

*Figure 6 shows that coalition parties speak more in **Poland (PL), Portugal (PT), Italy (IT) and the Netherlands (NL)**, while opposition parties dominate in **Austria (AT), Czech Republic (CZ), Latvia (LV) and Turkey (TR)**. Topic-level differences are smaller, though opposition parties speak more on International Affairs while coalitions speak more on Social Welfare across multiple countries.This indicates that volume disparities are mostly country-specific rather than topic-specific, probably due to parliamentary procedures. *

---

## 6. Conclusion

This notebook demonstrates how parliamentary discourse can be systematically analyzed by party status across multiple parliaments. By combining **sentiment analysis, topic share, cosine distances, dendrograms, nearest neighbors and relative word counts**, we identified both general patterns and country-specific behaviors. Coalition parties generally exhibit a more positive tone and speak more in certain countries, while opposition parties tend to be more critical and dominate speech volume elsewhere. Topic distributions are largely similar between coalition and opposition, but small differences emerge in outlier countries. Overall, these methods provide a framework that can be useful for comparing parliamentary behavior and highlighting **shared patterns** and **distinctive cases** across European parliaments.
