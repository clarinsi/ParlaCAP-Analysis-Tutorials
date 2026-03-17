# Tutorial 2 - Topic & Sentiment Distributions

The main goal of this tutorial is to perform **Exploratory Data Analysis (EDA)** on the ParlaMint dataset, focusing on a descriptive overview of **policy topics (CAP categories)** and **sentiment** in parliamentary debates.

**Possible research questions are:**
- *Which CAP categories dominate parliamentary debates, and does the distribution differ across countries?*
- *How does average sentiment vary by topic and by country?*
- *Do party affiliation or coalition status influence the tone of debates?*

To answer these questions, the notebook introduces a series of visualizations and summary statistics:
- **Histograms and barplots** of CAP category distributions across parliaments
- **Sentiment distributions** for individual topics within a single country and across all countries
- **Comparisons of distributions by party status** (Coalition vs. Opposition)
- **Tables summarizing average sentiment** by topic and country, with export options (TSV) for further research
- **Scatterplots** linking topic prominence and sentiment, highlighting correlations between how much a topic is discussed and the tone in which it is discussed

---

## 1. Setup & Data Loading

This setup (installing/loading) libraries and data loading & filtering steps are the **same as in Tutorial 1**. For detailed explanations, see **Tutorial 1, Sections 1-2**.

```r
# Uncomment and run the following lines if you haven't installed these packages yet. 
# install.packages("tidyverse")
# install.packages("data.table")
# install.packages("scales")
# install.packages("RColorBrewer")
```

```r
library(tidyverse)
library(data.table)
library(scales)
library(RColorBrewer)
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
 
 # Filter MPs with regular role
 filtered_chunk <- chunk[speaker_MP == "MP" & speaker_role == "Regular"]
 
 # Drop rows where CAP_category or sentiment is empty
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

# Combine all data
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

## 2. Policy attention profiles

### 2.1. Selected countries

Instead of looking at all 20+ parliaments combined, we focus on a few **selected countries** to make comparisons easier.

By limiting the scope to 2-3 countries at a time, we can spot similarities and differences without overwhelming the plots.

```r
selected_countries <- c("RS", "HR", "HU")
filtered_subset <- filtered_all[country %in% selected_countries]
```
# Aggregate by country AND CAP category

```r
cap_counts <- filtered_subset[, .(total_words = sum(word_count)), by = .(country, CAP_category)]

ggplot(cap_counts, aes(x = reorder(CAP_category, -total_words), y = total_words, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribution of CAP Categories by Country",
    x = "CAP Category",
    y = "Total Word Count",
    fill = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  ```
  
# Aggregate by country AND CAP category (proportions)
  
```r
# Calculate proportions within each country
cap_counts <- filtered_subset[, .(total_words = sum(word_count)), by = .(country, CAP_category)]
cap_counts[, proportion := total_words / sum(total_words), by = country]

ggplot(cap_counts, aes(x = reorder(CAP_category, -proportion), y = proportion, fill = country)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Proportion of Parliamentary Debate by CAP Category",
    subtitle = "Comparing RS, HR, and HU",
    x = "CAP Category",
    y = "Proportion of Total Words",
    fill = "Country"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

*Figures show similarities and differences in policy priorities across the three Central/Southeastern European parliaments. Macroeconomics and Government Operations are the top two categories in all three countries (28-33% of parliamentary debate). Differences appear in secondary priorities: Croatia shows stronger focus on Civil Rights and Domestic Commerce, in Hungary we see emphasis on Education and Health, while Serbian parliamentary attention goes to International Affairs. This pattern fits well with findings on 'core functions of government.' [Jennings et al. (2011)](https://doi.org/10.1177/0010414011405165) found that topics like macroeconomics, government operations, and defense consistently dominate executive agendas across countries, reflecting what they term 'issue intrusion'—certain issues impose themselves on all governments regardless of ideology or institutional context. *

---

### 2.2. Per-country (example: Serbia)

To get even more concrete insights, we zoom in on **one country**. Here, we use **Serbia (RS)** as an example because:
- It has a relatively large number of speeches in the dataset.
- It represents a non-EU parliament, which can be contrasted with EU member states.
- Due to its structural similarities with neighboring countries, it provides an interesting case for regional comparison.

```r
# Example: focus on one country
country_code <- "RS"
filtered_by_country <- filtered_all[country == country_code]

# Count speeches by CAP category
cap_counts_country <- filtered_by_country[, .N, by = CAP_category]
cap_counts_country <- cap_counts_country[order(CAP_category)]

# Plot
ggplot(cap_counts_country, aes(x = reorder(CAP_category, -N), y = N)) +
 geom_bar(stat = "identity", fill = "#66c2a5") +
 labs(
   title = paste("Fig. 2, CAP Category Distribution (based on speech counts) -", country_code),
   x = "CAP Category",
   y = "Number of Speeches"
 ) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
```

*In this example, it shows that the Serbian parliament devotes most of its time to debate "Government Operations" and "Macroeconomics", as well as "Law and Crime", similar to the distribution of 2.1.. If you want to research the distribution for other countries, just change the country abbreviation in the code above.*

---

## 3. Sentiment by Party & Orientation

After examining the **distribution of CAP categories** across selected countries and focused on Serbia (RS) as an illustrative example, we will explore the **sentiment of the debates**. This section examines how positive, neutral or negative tones are distributed across topics and parties, helping us understand the emotional dynamics of parliamentary discussions.

### 3.1. Sentiment distribution across topics for a single country

Before analyzing sentiment by party or coalitions status, it is useful to look at the **overall sentiment distribution across CAP topics** in a single country. Here, we will look at **Serbia (RS)**, which we already selected for the previous analysis of CAP category distribution (see 2.2.).

```r
# ---- 1. Filter data for one country ----
country_code <- "RS"  # change to your country of interest
country_data <- filtered_all[country == country_code]

# ---- 2. Aggregate word counts by CAP_category and sentiment ----
cap_sent_counts <- country_data[, .(word_count = sum(word_count)),
                                by = .(CAP_category, sent3_category)]

# ---- 3. Ensure CAP_category is ordered consistently ----
cap_order <- sort(unique(country_data$CAP_category))
cap_sent_counts[, CAP_category := factor(CAP_category, levels = cap_order)]

# ---- 4. Define custom color palette ----
custom_palette <- c("Negative" = "#fc8d62", "Neutral" = "#8da0cb", "Positive" = "#66c2a5")

# ---- 5. Plot ----
ggplot(cap_sent_counts, aes(x = CAP_category, y = word_count, fill = sent3_category)) +
 geom_bar(stat = "identity", position = "dodge") +
 scale_fill_manual(values = custom_palette) +
 labs(
   title = paste("Fig. 3, Sentiment Distribution Across CAP Topics in", country_code),
   x = "CAP Category",
   y = "Total Word Count",
   fill = "Sentiment"
 ) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

*For Serbia, we can identify that the distribution of negative and neutral tone is quite balanced across topics. In the cases of the most discussed topics, such as 'Government Operations', 'Macroeconomics' and 'Law and Crime', we see that negative sentiment tends to dominate these debates. These patterns could be investigated further, for example by comparing these trends with other countries in the dataset (see 3.2.) or by analyzing how sentiment varies across **party status** (Coalition vs. Opposition) (see 3.4.) or **party orientation** (see 3.5.).*

---

### 3.2. Sentiment Distribution across topics for selected countries

After exploring sentiment patterns within a single country (RS), we can now expand the analysis to **multiple countries** - for examples, **Serbia (RS), Croatia (HR) and Hungary (HU)**.

The next code aims to visualize the distribution of positive, neutral and negative sentiment for each CAP topic across the selected parliaments.

By comparing the topic distribution across parliaments, we can identify:
- Which topics tend to evoke more negative or positive language overall.
- How sentiment patterns differ from the single-country perspective, revealing both common trends and country-specific deviations.

```r
# ---- 1. Filter for selected countries & aggregate word counts ----
selected_countries <- c("RS", "HR", "HU")
cap_sent_country <- filtered_all[country %in% selected_countries,
                                 .(total_words = sum(word_count)),
                                 by = .(country, CAP_category, sent3_category)]

# ---- 2. Ensure CAP_category is ordered consistently ----
cap_order <- sort(unique(cap_sent_country$CAP_category))
cap_sent_country[, CAP_category := factor(CAP_category, levels = cap_order)]

# ---- 3. Define custom color palette ----
custom_palette <- c("Negative" = "#fc8d62", "Neutral" = "#8da0cb", "Positive" = "#66c2a5")

# ---- 4. Plot with facets ----
ggplot(cap_sent_country, aes(x = CAP_category, y = total_words, fill = sent3_category)) +
 geom_bar(stat = "identity", position = "dodge") +
 facet_wrap(~country, scales = "free_y", ncol = 2) +
 scale_fill_manual(values = custom_palette) +
 labs(
   title = paste("Fig. 4, Word Distribution by CAP Category and Sentiment in",
                 paste(selected_countries, collapse = ", ")),
   x = "CAP Category",
   y = "Total Words",
   fill = "Sentiment"
 ) +
 theme(
   axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
   strip.text = element_text(size = 12, face = "bold")
 )
```

*These plots show that the distribution of sentiment can vary from parliament to parliament. Interestingly, the neutral and negative tones dominate the debates across most topics.*

*'Government Operations', 'Macroeconomics' and 'Law and Crime' consistently have the highest word counts and are mostly characterized by negative and neutral sentiment.*

*Comparing countries, Serbia (RS) shows a slightly higher proportion of neg. tone in 'Macroeconomics' and 'Law and Crime', while Hungary (HU) has more neutral contributions in 'Public Welfare' and 'Health'.*

These trends and country-specific differences can be further explored by looking at the distribution of one country across parties, orientations or coalition status.

---

### 3.3. By party in one country (example: Serbia)

The next code examines **sentiment distribution** by **party** in a single country - here, Serbia - to identify which parties contribute more positive, neutral or negative tone. As shown in Fig. 4, Serbia displays a wider range of sentiment across CAP categories than the other countries, making it an interesting case to see whether this pattern might show at the party level as well.

In the code, we apply a threshold of 1% (see section 4 in the code) of the total word count to focus on parties with a meaningful contribution. This removes smaller parties with minimal speaking time which makes the visualization clearer (it doesn't show all 40+ parties) and easier to interpret.

```r
# ---- 1. Filter the data for one country ----
country_code <- "RS"
country_data <- filtered_all[country == country_code]

# ---- 2. Aggregate word count by party and sentiment ----
agg_counts <- country_data[, .(word_count = sum(word_count)),
                           by = .(speaker_party_name, sent3_category)]

# ---- 3. Calculate total words per party ----
party_totals <- agg_counts[, .(total = sum(word_count)), by = speaker_party_name]

# ---- 4. Set threshold (e.g., 1% of total words in country) ----
threshold <- 0.01 * sum(party_totals$total)
significant_parties <- party_totals[total >= threshold, speaker_party_name]

# ---- 5. Filter agg_counts to include only significant parties ----
agg_counts_filtered <- agg_counts[speaker_party_name %in% significant_parties]

# ---- 6. Sort parties by total word count ----
party_order <- party_totals[total >= threshold][order(-total), speaker_party_name]
agg_counts_filtered[, speaker_party_name := factor(speaker_party_name, levels = party_order)]

# ---- 7. Define custom palette ----
custom_palette <- c("Negative" = "#fc8d62", "Neutral" = "#8da0cb", "Positive" = "#66c2a5")

# ---- 8. Plot ----
ggplot(agg_counts_filtered, aes(x = speaker_party_name, y = word_count, fill = sent3_category)) +
 geom_bar(stat = "identity", position = "dodge") +
 scale_fill_manual(values = custom_palette) +
 labs(
   title = paste("Fig. 5, Sentiment Distribution by Party in", country_code),
   x = "Party",
   y = "Total Word Count",
   fill = "Sentiment"
 ) +
 theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

*As shown in Fig. 5, the nationalist right-orientated **Srpska radikalna stranka** (*Serbian Radical Party*, SRS) stands out both in word count and in negative sentiment, with roughly two thirds of their speeches carrying a negative tone. In contrast, the major ruling center-right **Srpska napredna stranka** (*Serbian Progressive Party*, SNS) debates in a much less negative manner: neutral tone dominates and the party contributes the largest share of positive speech overall. The more negative tone of opposition parties is in line with CAP research on parliamentary questions. Opposition parties use parliamentary activities as 'attention-seeking devices' to highlight government failures and controversial issues, while governing parties have more positive framing of their policy achievements. In the next section, we will see that this reflects a broader pattern: ruling parties tend to express more positive sentiment, while opposition parties are generally more negative.*

---

### 3.4. Coalition vs. Opposition (example: Serbia)

Before analyzing the sentiment of coalition and opposition parties, it is essential to understand their **relative contribution** to the parliamentary debate. Fig. 6a displays the total word count allocated to each party status group ("Opposition" / "Coalition" / "-") in the Serbian parliament.

This visualization helps to establish the balance of the discoursive power. A dominant coalition might set the overall tone of debates, while a vocal opposition could shape discourse through criticism. Understanding this distribution is important to interpreting the subsequent sentiment analysis (Fig. 6b) because the tone of a group that speaks frequently carries more weight than that of a group with minimal speaking time.

*Note: The "-" category appears for speeches where party status could not be determined or does not clearly fall into "Coalition" or "Opposition". This may include independent MPs, temporary members, or cases where metadata is missing. Including these speeches in the analysis can slightly influence the overall sentiment distribution, as their contributions may not reflect the typical behavior of coalition and opposition parties. When interpreting results, it's important to consider that the "-" category represents a heterogenous group and may introduce additional variability.*

```r
# ---- 1. Select the countries ----
selected_countries <- c("RS")
filtered_subset <- filtered_all[country %in% selected_countries]

# ---- 2. Calculate total word count by party status ----
status_total_words <- filtered_subset[!is.na(party_status), .(word_count = sum(word_count)), by = party_status]

# ---- 3. Plot total word count by party status ----
ggplot(status_total_words, aes(x = party_status, y = word_count, fill = party_status)) +
 geom_bar(stat = "identity") +
 scale_fill_brewer(palette = "Set2") +
 labs(
   title = paste("Fig. 6a, Total Word Count by Party Status in", paste(selected_countries, collapse = ", ")),
   x = "Party Status",
   y = "Total Word Count"
 ) +
 theme(legend.position = "none")
```

*As Figure 6a indicates, the Coalition contributes the most in the debates at the Serbian parliaments. This is different to a lot of other parliaments where usually the opposition is speaking more than the coalition.*

---

Having established the speaking volume of each group, we can now analyze the **sentiment distribution across party status (Fig. 6b)**. This helps to answer questions such as *Is the opposition or the coalition more negative in its tone?*

Here, we focus on one selected countries rather than the full dataset. At the top, there is an optional (commented-out) sections that allows you to change or add more countries you want to analyze together or even filter by a specific party, making it possible to explore coalition vs. opposition dynamics in greater detail.

```r
# Check unique sentiment categories
unique(filtered_all$sent3_category)
```

```r
# ---- 1. Select the countries ----
selected_countries <- c("RS")
filtered_subset <- filtered_all[country %in% selected_countries]

# ---- 2. Aggregate ----
status_word_counts <- filtered_subset[, .(word_count = sum(word_count)),
                                      by = .(party_status, sent3_category)]

# ---- 2.1 Filter out "-" status and neutral ----
status_word_counts <- status_word_counts[
 party_status != "-" & !is.na(party_status) &
 sent3_category != "Neutral"
]

# ---- 3. Calculate percent within each party_status ----
status_word_counts[, total := sum(word_count), by = party_status]
status_word_counts[, percentage := (word_count / total) * 100]

# ---- 4. Define sentiment order and palette ----
hue_order <- c("Negative", "Positive")
custom_palette <- c("Negative" = "#fc8d62", "Positive" = "#66c2a5")

status_word_counts[, sent3_category := factor(sent3_category, levels = hue_order)]

# ---- 5. Plot ----
ggplot(status_word_counts, aes(x = party_status, y = percentage, fill = sent3_category)) +
 geom_bar(stat = "identity", position = "dodge") +
 scale_fill_manual(values = custom_palette) +
 labs(
   title = paste("Fig. 6b, Sentiment Distribution by Party Status in",
                 paste(selected_countries, collapse = ", "), "(Normalized)"),
   x = "Party Status",
   y = "Percentage of Words (%)",
   fill = "Sentiment"
 )
```

*Figure 6b shows us that the Opposition in the Serbian parliament contributes a larger share of its speech to negative sentiment, whereas the Coalition contributes a relatively higher share to positive sentiment.*

*Future research could investigate whether this negativity is structural and can be found in a similar way in other parliaments. Additionally, examining how these patterns vary by topic or over time could provide deeper insights into parliamentary discourse dynamics.*

---

## 4. Average Sentiment by Country & Topic

To complement the previous distribution plots, we now summarize sentiment patterns quantitatively. For each country and topic, we calculate the average sentiment along with the total word count. The results are presented in a table, which can also be exported as a TSV file for further analysis.

```r
# ---- 1. Compute weighted avg sentiment and total word count ----
agg <- filtered_all[, .(
 avg_sentiment = weighted.mean(sent_logit, word_count, na.rm = TRUE),
 total_words = sum(word_count)
), by = .(country, CAP_category)]

# ---- 2. Round the sentiment ----
agg[, avg_sentiment := round(avg_sentiment, 2)]

# ---- 3. Ensure total_words is integer ----
agg[, total_words := as.integer(total_words)]

# ---- 4. Sort by country and CAP_category ----
agg <- agg[order(country, CAP_category)]

# ---- 5. Display the table ----
print(agg)

# ---- 6. Export to TSV for further research ----
output_path <- "avg_sentiment_by_country_topic.tsv"
fwrite(agg, output_path, sep = "\t")
cat(paste("\nTSV file saved as:", output_path, "\n"))
```

*A table like this - with **average sentiment** and **total word count** per *country and topic* - opens up several useful analyses and visualizations. For example, you could rank topics by sentiment within each country to see which topics are debated more positively or negatively. You could also compare countries and identify if certain topics (e.g. 'Macroeconomics' or 'Health') are framed more negatively in one country than another.*

Having summarized average sentiment and word counts by country and topic also allows us to shift to a more comparative perspective. In the next step, we will examine how strongly each topic features in parliamentary debates (its share of total words) and how that attention relates to the tone of discussion. Basically, we will try to investigate the question *Are topics that are more commonly debated also debated more negatively?*

---

## 5. Topic Share vs. Sentiment across Countries (example: RS)

By plotting topic share against average sentiment, we can visualize whether topics that dominate parliamentary discourse tend to be framed more positively, neutrally or negatively.

The code below aggregates the data of one selected country - here, **Serbia** - and gives us an overview of the overall discourse patterns in this country's parliament and to highlight topics that are in general more 'negative' or 'positive'.

```r
# ---- 1. Optional: Filter for a single country (e.g., Serbia = "RS") ----
country_filter <- "RS"  # set to NULL to use all countries

if (!is.null(country_filter)) {
 data_subset <- filtered_all[country == country_filter]
} else {
 data_subset <- filtered_all
}

# ---- 2. Aggregate across all countries ----
agg <- data_subset[, .(
 mean_sent = mean(sent_logit, na.rm = TRUE),
 total_words = sum(word_count)
), by = CAP_category]

# ---- 3. Calculate topic share across all countries ----
total_words_all <- sum(agg$total_words)
agg[, topic_share_pct := (total_words / total_words_all) * 100]

# ---- 4. Scatter plot with labels
library(ggrepel)

ggplot(agg, aes(x = topic_share_pct, y = mean_sent)) +
  geom_point(size = 4, color = "#66c2a5") +
  geom_smooth(method = "lm", se = FALSE, color = "gray", linetype = "dashed") +
  geom_text_repel(aes(label = CAP_category), size = 3, max.overlaps = 20) +
  labs(
    title = paste("Topic Share vs. Average Sentiment in", country_filter),
    x = "Topic Share (%)",
    y = "Average Sentiment (sent_logit)"
  )
```

*Figure 7 shows a downward facing trend line, suggesting that topics receiving more attention tend to be discussed with a more negative tone. We can identify 'Government Operations', 'Macroeconomics' and 'Law and Crime' as the main drivers of this pattern which matches with our findings from Fig. 4 where we saw that these topics were not only the most discussed but also always dominated by negative speech. We know from agenda research that issues that receive sustained attention often do so because they represent unresolved problems or conflicts and they bring more critical discourse.*

---

## 6. Conclusion

This tutorial has provided a comprehensive exploratory analysis of the ParlaMint dataset, while addressing fundamental questions regarding the distribution of policy topics and sentiment across parliaments. The key findings reveal interesting patterns and indicate country-specific nuances in parliamentary discourse. In summary, we looked at:

- **CAP category patterns**: Topics such as 'Macroeconomics', 'Government Operations' and 'Law and Crime' dominate debates across the selected parliaments (RS, HR, HU).
- **Sentiment trends**: Neutral and negative sentiment generally dominate parliamentary debates. Positive tone is identified as the least common, indicating that parliaments are primarily criticism and heated discussions as a form of expression (rather than praise or affirmation).
- **Party & coalition influence**: In the analysis of Serbia (RS), coalition parties dominate the quantity of speech and tend to employ a more neutral tone, while opposition parties speak less overall and contribute a disproportionally high share of negative sentiment.
- **Topic prominence vs. sentiment**: The scatterplot reveals that topics receiving greater attention are discussed more negatively, though some countries might display a more balanced distribution if looked at separately.

## What's Next?

In the next Tutorial (**Tutorial 3**), we will build on the descriptive analyses from this Tutorial and move towards a more comparative approach, using cosine distance to systematically measure how similar or different parliamentary discourses are across countries.
