# Tutorial 3 - Cross-Parliament Comparison

In **Tutorial 2**, we used descriptive statistics and visualizations to explore the topical and sentimental landscapes of individual parliaments. We asked *what* topics are discussed, *how* they are discussed and identified some interesting patterns, such as the coalition-opposition sentiment divide. These findings however, were largely qualitative and country-specific.

This tutorial brings our analysis on another level into the area of **quantitative comparison and clustering**, which represent core tasks in **Computational Linguistics** and **NLP** (Natural Language Processing). We move from asking *"what is the pattern in country X?"* to a more systematic, data-driven question: *"How similar or different are these parliamentary discourses from one another?"*

To answer this, we introduce a fundamental NLP method for comparing distributions: **cosine distance in a vector space model**. In this framework, each country is represented as a high-dimensional vector where:
- Each dimension corresponds to a **CAP category** (for topical analysis)
- The value in each dimension can be the **total word count** (to model topic prevalence) or the **average sentiment** (to model emotional tone).

By measuring the cosine distance of the angle between these country-vectors, we can quantify their similarity in discourse profile, independent of the amount of text that was produced. This allows us to:
- **Cluster** parliaments into groups with similar discoursive focus or emotional tone.
- **Identify nearest neighbors** to find which countries have the most similar parliamentary attention patterns.

**Possible research questions:**
- *Which countries have the most similar sentiment patterns across CAP categories?*
- *How similar are countries in their topic distributions based on word counts?*
- *For a specific CAP category, which countries exhibit the most similar sentiment patterns?*

This tutorial includes:
- **Introduction to Cosine Distance**
- **Load, read and filter the Data**
- **Compute cosine distance between countries based on sentiment**
   1. Example: Serbia (RS) & Croatia (HR)
   2. All countries combined
   3. The 3 nearest neighbors
- **Compute cosine distance between CAP category distributions (based on word counts)**
   1. Example: Serbia (RS) & Croatia (HR)
   2. All countries combined
   3. The 3 nearest neighbors

---

## 1. Introduction to Cosine Distance

**Cosine Distance** is way of comparing **patterns** between countries, regardless of size. Instead of asking *"which country's parliament has more speeches?"*, it asks *"how similar are their profiles across topics or sentiments?"*.

We turn each country's CAP-category-distribution or CAP x sentiment score - values into a vector and then compare the *angle* between those vectors.
- If two countries talk about the same topics in similar proportions (or show similar sentiment patterns), their cosine distance is **small** (close together).
- If their focus is very different, the distance is **large** (far apart).

This method lets us:
- Cluster countries into groups with similar discourse (via dendrograms)
- Identify each parliament's "nearest neighbor"
- Compare similarity across all MPs, coalitions or oppositions
- Zoom into specific topics (e.g. *Environment*) to see which countries align or diverge

In practical terms, cosine distance adds a comparative layer as it can reveal whether European parliaments are converging toward similar policy priorities over time (perhaps due to EU integration) or diverging based on national contexts. 
By clustering countries based on their distributions we can also discover whether policy priorities align with geographic proximity, EU membership, institutional similarities or historical legacies. We can ask do post-Yugoslav parliaments share similar policy profiles? Do Nordic countries cluster together?

---

### Example - How Cosine Distance Works

We look at two countries (A and B) and we want to find out how similar they are in how much they talk about 3 topics: *Education, Health, Environment*.

| Country | Education | Health | Environment |
|---------|--------|--------|-------------|
| A       | 50     | 30     | 20          |
| B       | 100    | 60     | 40          |

--> If we just compared **word counts**, Country B seems like people there talk more about each topic - but that doesn't tell us how **similar** the countries are.

So, we convert them into proportions (distributions):

| Country | Education | Health | Environment |
|---------|--------|--------|-------------|
| A       | 0.50     | 0.30     | 0.20          |
| B       | 0.50    | 0.30     | 0.20          |

Now, the vectors are:
- A = [0.5, 0.3, 0.2]
- B = [0.5, 0.3, 0.2]

**Cosine Similarity compares the *angle* between vectors**:

$$
\text{cosine similarity}(A, B) = \frac{A \cdot B}{\|A\| \cdot \|B\|}
$$

- If the vectors point in the **same direction** --> similarity = 1 (distance = 0)
- If they are **orthogonal (completely different)** ---> similarity = 0 (distance = 1)

Here:
- Countries A and B have identical proportions, so cosine similarity = 1
- They are perfectly similar in terms of *distribution across topics*, even though B has more total speeches

---

## 2. Setup & Data Loading

This setup (installing/loading) libraries and data loading & filtering steps are the **same as in Tutorial 1**. For detailed explanations, see **Tutorial 1, Sections 1-2**.

```r
# Uncomment and run the following lines if you haven't installed these packages yet
#install.packages("tidyverse")
#install.packages("data.table")
#install.packages("proxy")      # for cosine distance
#install.packages("pheatmap")   # for heatmaps
#install.packages("dendextend") # for dendrograms
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

## 3. Cosine Distance Analysis of Sentiment

### 3.1. Example: Serbia (RS) & Croatia (HR)

In this first step of our cosine distance analysis, we will explore how similar or different two selected countries are in terms of the sentiment expressed across policy domains. We begin with a **pairwise comparison** between **Serbia (RS)** and **Croatia (HR)**. These two parliaments are chosen because as neighboring countries with a shared yet often contrasting political and historical trajectory, they are an interesting first case study for our cosine distance analysis.

To do this, the code below constructs a **country-by-category sentiment matrix**, where each cell represents the average sentiment expressed in speeches for a given CAP category. With the help of this matrix, the **cosine distance** between RS and HR is computed. As a result, we get a single similarity score that represents how closely their sentiment profiles match.

This serves as a numerical view of these relationships, rounded to three decimal places for clarity.

In addition to the numeric score, we also visualize the results with a **heatmap**. The heatmap displays the average sentiment values of RS and HR side by side across all CAP categories, with **red indicating lower sentiment values and green indicating higher ones**. This makes it easy to spot where the two countries align more closely and where they differ.

```r
# ---- 1. Pivot data: rows = country, columns = CAP_category, values = mean sentiment ----
country_cap_sent <- filtered_all[, .(mean_sent = mean(sent_logit, na.rm = TRUE)),
                                 by = .(country, CAP_category)]

# Convert to wide format (matrix)
country_cap_sent_wide <- dcast(country_cap_sent, country ~ CAP_category,
                               value.var = "mean_sent", fill = 0)

# Convert to matrix with country names as row names
country_matrix <- as.matrix(country_cap_sent_wide[, -1])
rownames(country_matrix) <- country_cap_sent_wide$country

# ---- 2. Compute cosine distance between RS and HR ----
rs_vector <- country_matrix["RS", , drop = FALSE]
hr_vector <- country_matrix["HR", , drop = FALSE]

# Cosine distance = 1 - cosine similarity
cosine_sim <- sum(rs_vector * hr_vector) / (sqrt(sum(rs_vector^2)) * sqrt(sum(hr_vector^2)))
cosine_dist <- 1 - cosine_sim

# ---- 3. Print the result ----
cat(sprintf("Cosine distance between RS and HR: %.3f\n", cosine_dist))

# ---- 4. Prepare data for heatmap ----
pair_sent <- t(country_matrix[c("RS", "HR"), ])

# ---- 5. Plot heatmap ----
pheatmap(
 pair_sent,
 display_numbers = TRUE,
 number_format = "%.2f",
 color = colorRampPalette(c("red", "yellow", "green"))(100),
 breaks = seq(1.5, 2.5, length.out = 101),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = "Fig. 1, Sentiment Heatmap by CAP Category (RS vs HR)",
 fontsize = 10
)
```

*The pairwise cosine distance between **Serbia** and **Croatia** is very small at **0.003**. This indicates that their overall sentiment profiles across CAP categories are **highly similar**. Essentially, on most topics, parliamentary speeches in these two neighboring countries share a similar emotional tone.*

*Looking more closely at individual CAP categories on the heatmap, we can further identify a few differences:*
+ **International Affairs**: *RS has an average sentiment of **1.7**, while HR scores **2.2**, indicating that Croatia's speeches are more positive or optimistic in this domain compared to Serbia*.
+ **Law and Crime**: *RS scores **1.6**, slightly lower than HR's **1.9**, suggesting a small divergence in sentiment around legal and criminal policy discussions.*

*For all other CAP categories, the sentiment scores are nearly identical which again underlines the overall closeness of the two countries' sentiment profiles and emotional tones, captured by the cosine distance.*

*This pairwise comparison serves as a benchmark for interpreting larger-scale patterns when we extend the analysis to **all countries** of ParlaMint.*

---

### 3.2. Cosine distance analysis of sentiment - all countries

In the following step, we will extend the same procedure to **all countries in the dataset**.

The first step is again to compute the **full cosine distance matrix** which consists of the similarity values between every pair of countries based on their average sentiment across CAP categories. Each entry in this matrix represents the cosine distance between two countries' sentiment profiles. This provides a numerical measure of how closely their parliamentary rhetoric aligns. Additionally, the full cosine distance matrix can be **exported as a TSV file** for further analysis or sharing of the results.

To make these relationships easier to interpret, we visualize the matrix as a **heatmap**. The heatmaps provides a quick overview of which countries have similar sentiment patterns (green) and which differ more strongly (red).

Finally, we use **hierarchical clustering** to generate a **dendrogram**. This visualization method groups countries based on the similarity of their sentiment profiles, revealing clusters of parliaments that share comparable emotional tones. The dendrogram does not only summarize the pairwise relationships captured in the heatmap but also helps identify regional or political patterns that might not be immediately obvious otherwise.

```r
# ---- 1. Compute cosine distance matrix for all countries ----
# Using the proxy package for cosine distance
cosine_dist_matrix <- as.matrix(dist(country_matrix, method = "cosine"))

# ---- 2. Convert to readable data frame ----
cosine_df <- as.data.frame(cosine_dist_matrix)
cosine_df <- round(cosine_df, 5)

print(cosine_df)

# ---- 2.1. Export to TSV ----
write.table(cosine_df, "cosine_distance_matrix.tsv", sep = "\t", quote = FALSE)

# ---- 3. Heatmap of the full cosine distance matrix ----
pheatmap(
 cosine_dist_matrix,
 color = colorRampPalette(c("green", "yellow", "red"))(100),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = "Fig. 2, Cosine Distance Heatmap: All Countries",
 fontsize = 8
)

# ---- 4. Hierarchical clustering and dendrogram ----
hc <- hclust(as.dist(cosine_dist_matrix), method = "average")

# Plot dendrogram
plot(hc, main = "Fig. 3, Cosine Distance Dendrogram: CAP x Sentiment",
    xlab = "", sub = "", cex = 0.8)
```

*From Figure 2, the **heatmap of the full cosine distance matrix**, several patterns become apparent. First, we have a few countries that are **highly divergent** (Spain (ES), Galicia (ES-GA), Latvia (LV)), which means that they stand out as very different from most other countries, indicating that their parliamentary sentiment patterns across CAP categories are more unique compared to the majority.*

*Second, we can see some **specific outlier pairs**. Some countries differ strongly from each other, such as LV & ES-GA, LV & Turkey (TR) or RS & LV, highlighting notable differences in how these parliaments frame policy topics.*

*Lastly, we also can identify some **highly similar countries**. Belgium (BE) & Austria (AT), Sweden (SE) & Belgium (BE), SE & Czech Republic (CZ) and SE & Estonia (EE) all show very similar sentiment profiles.*

*Figure 3, the **dendrogram**, visualizes this too and offers a hierarchical view of country similarities: clusters forming at low linkage distances indicate **high similarity** (e.g. BE, SE, CZ, EE), while countries that branch off early or are linked with a differently coloured line, like ES-GA and LV, are **more distinct**, confirming their outlier status. It also reveals sub-clusters that may reflect regional, political or historical patterns that are not immediately recognizable in the heatmap.*

---

### 3.3. Find the nearest neighbors (3 most similar countries)

Following the heatmap and dendrogram analysis, we now turn to a **more concrete, country-specific view**. The code below uses cosine distance to find the **nearest neighbors** for each country - that is, the three countries whose sentiment profiles are most similar based on the cosine distance.

This approach is a way to see which parliaments are **most closely aligned** in their emotional tone across CAP categories. For each country, we mask the diagonal of the cosine distance matrix (to avoid self-comparison) and select the three smallest distances - the nearest neighbors.

```r
# ---- 1. Find the 3 most similar countries ----
find_nearest_neighbors <- function(dist_matrix, n = 3) {
 neighbors <- data.frame(country = rownames(dist_matrix))
 
 for (i in 1:nrow(dist_matrix)) {
   # Get distances for this country, excluding self
   distances <- dist_matrix[i, ]
   distances[i] <- Inf  # Exclude self-comparison
  
   # Find n smallest distances
   nearest_idx <- order(distances)[1:n]
   neighbors[i, paste0("NN", 1:n)] <- colnames(dist_matrix)[nearest_idx]
 }
 
 return(neighbors)
}

nearest_neighbors <- find_nearest_neighbors(cosine_dist_matrix, 3)

# ---- 2. Display the results ----
for (i in 1:nrow(nearest_neighbors)) {
 cat(sprintf("%s: %s, %s, %s\n",
             nearest_neighbors$country[i],
             nearest_neighbors$NN1[i],
             nearest_neighbors$NN2[i],
             nearest_neighbors$NN3[i]))
}
```

*This table of nearest neighbors shows, for each country, the **three parliaments whose sentiment profiles are most similar** across CAP categories. For example, **Austria (AT)** is most similar to **Belgium (BE), Estonia (EE) and Greece (GR)**. **Serbia (RS)** on the other hand aligns closely with **Poland (PL), Greece (GR) and Croatia (HR)**.*

Overall, the results reveal patterns of similarity and divergence in parliamentary rhetoric that can reflect **regional proximity, shared political traditions or comparable policy approaches**.

---

## 4. Cosine Distance Analysis of Topic Distributions (Word Count)

In this section, we move from **sentiment** to **topic attention**. Similar to **Tutorial 2, Section 3.4.** we will explore now how parliaments allocate their relative attention to different CAP categories. Instead of asking *how positively or negatively* countries discuss topics, we now ask *how similarly do they devote their time to different topics*. This method allows us to identify countries with **similar topical priorities**, outliers that focus on very different topics and clusters of countries with comparable parliamentary attention patterns.

### 4.1. Example: Serbia (RS) & Croatia (HR)

We start with a **pairwise comparison** between RS and HR, similar to what we did in **Section 3.1** for sentiment.

First, we will construct a country-by-CAP-category **word count matrix** for RS and HR. Then, we compute the **cosine distance** between their topic distributions and visualize our results using a **heatmap** to see where the countries align or differ.

```r
# ---- 1. Pivot data: rows = country, columns = CAP_category, values = total word count ----
pair_words <- filtered_all[country %in% c("RS", "HR"),
                           .(total_words = sum(word_count)),
                           by = .(country, CAP_category)]

pair_words_wide <- dcast(pair_words, country ~ CAP_category, value.var = "total_words", fill = 0)
pair_words_matrix <- as.matrix(pair_words_wide[, -1])
rownames(pair_words_matrix) <- pair_words_wide$country

# ---- 1.1 Normalize to proportions ----
pair_words_norm <- pair_words_matrix / rowSums(pair_words_matrix)

# ---- 2. Compute cosine distance between RS and HR ----
rs_vec <- pair_words_norm["RS", ]
hr_vec <- pair_words_norm["HR", ]

cosine_sim <- sum(rs_vec * hr_vec) / (sqrt(sum(rs_vec^2)) * sqrt(sum(hr_vec^2)))
cosine_dist_pair <- 1 - cosine_sim

cat(sprintf("Cosine distance (topic distribution) between RS and HR: %.3f\n", cosine_dist_pair))

# ---- 3. Heatmap ----
pheatmap(
 t(pair_words_norm),
 display_numbers = TRUE,
 number_format = "%.2f",
 color = colorRampPalette(c("lightyellow", "lightblue", "darkblue"))(100),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = "Fig. 4, CAP Topic Distribution Heatmap: RS vs HR",
 fontsize = 10
)
```

*The cosine distance between RS and HR based on topic distributions (word counts) is **0.028**. This is slightly higher than the distance we observed for sentiment but still indicates that their overall topic focus is fairly similar.*

*The heatmap further visualizes how much attention each parliament devotes to the different CAP topics. Some topics have very small shares - for example, **Foreign Trade** accounts for only 0.38% of HR's total speech (0.0038), indicating it is rarely discussed. The largest shares are **Government Operations** and **Macroeconomics**, consistent with what we observed in **Tutorial 2**. RS allocates roughly 17% of all speech to Government Operations, which is noticeably higher than HR and could suggest a stronger focus on administrative matters in the parliament. Another notable difference is **Law and Crime**, where RS dedicates 12% of speech compared to HR's 3.8%.*

*These differences highlight areas where parliamentary attention diverges and could be further explored in comparative studies.*

---

### 4.2. Cosine distance analysis of topic distribution - all countries

Next, we extend the analysis to **all countries** to examine **broader patterns of topical similarity**.

For that matter, we first build a **country-by-CAP-category word count matrix** for all countries where we normalize the proportions to compare the relative topic emphasis. Then, we compute the **cosine distance** for all country pairs. The output cosine distance matrix can be exported to **TSV** for further analysis. In the end, we visualize the results using a **heatmap** and a **dendrogram** to identify clusters and outliers.

```r
# ---- 1. Pivot data for all countries ----
country_cap_words <- filtered_all[, .(total_words = sum(word_count)),
                                  by = .(country, CAP_category)]

country_cap_words_wide <- dcast(country_cap_words, country ~ CAP_category,
                                value.var = "total_words", fill = 0)
country_words_matrix <- as.matrix(country_cap_words_wide[, -1])
rownames(country_words_matrix) <- country_cap_words_wide$country

# ---- 1.1 Normalize to proportions ----
country_words_norm <- country_words_matrix / rowSums(country_words_matrix)

# ---- 2. Compute cosine distance ----
cosine_words_matrix <- as.matrix(dist(country_words_norm, method = "cosine"))

# ---- 2.1 Convert to data frame ----
cosine_words_df <- as.data.frame(round(cosine_words_matrix, 5))

# ---- 2.2 Export to TSV ----
write.table(cosine_words_df, "cosine_distance_wordcounts.tsv", sep = "\t", quote = FALSE)
print(cosine_words_df)

# ---- 3. Heatmap ----
pheatmap(
 cosine_words_matrix,
 color = colorRampPalette(c("lightyellow", "lightblue", "darkblue"))(100),
 cluster_rows = FALSE,
 cluster_cols = FALSE,
 main = "Fig. 5, Cosine Distance Heatmap: CAP Topic Distributions (Word Count)",
 fontsize = 8
)

# ---- 4. Dendrogram ----
hc_words <- hclust(as.dist(cosine_words_matrix), method = "average")
plot(hc_words, main = "Fig. 6, Dendrogram: CAP Topic Distributions (Word Count)",
    xlab = "", sub = "", cex = 0.8)
```

*Figure 5, the heatmap, reveals some interesting patterns in topic similarity across parliaments. In general, **Bosnia (BA)** and **Great Britain (GB)** stand out as more distinct from the majority of countries. Bosnia distinctiveness may come from its post-conflict institutional structure with complex power-sharing arrangements. The UK's outlier status is consistent with CAP findings that majoritarian systems produce different attention dynamics than consensus democracies Certain country pairs are particularly striking as outliers, such as **BA & SE**, **BA & DK** and **BA & ES-GA**, highlighting areas where parliamentary focus differs greatly.*

*On the other hand, a number of countries are closely aligned: **Estonia (EE) and Austria (AT)**, **Hungary (HU) and AT, Croatia (HR) and Serbia (RS)** and **HR & Ukraine (UA)** all exhibit high similarity in their topic distributions. Notably, **HR & RS** remain highly aligned even in this broader comparison, supporting the patterns observed in the pairwise analysis in **Section 4.1**.*

*Figure 6, the dendrogram, further illustrates these relationships. The tightest clusters are among **RS & UA (& HR), AT & HU (& GR), CZ & EE** and **DK & SE**. These clusters reflect parliaments that share very similar patterns on how they allocate attention across CAP categories, while countries that branch off earlier represent distinctive approaches to parliamentary discourse.*

---

### 4.3. Find the nearest neighbor (three most similar countries based on topic distributions)

After visualizing the heatmap and dendrogram for **CAP topic distributions**, we can now take a **closer, country-specific look**. Similar to what we did for sentiment in **Section 3.3.**, we can use the **cosine distance matrix** to identify the **three countries whose topic distributions are most similar** to each country.

```r
# ---- 1. Find the 3 most similar countries based on topic distribution ----
nearest_neighbors_words <- find_nearest_neighbors(cosine_words_matrix, 3)

# ---- 2. Display the results ----
for (i in 1:nrow(nearest_neighbors_words)) {
 cat(sprintf("%s: %s, %s, %s\n",
             nearest_neighbors_words$country[i],
             nearest_neighbors_words$NN1[i],
             nearest_neighbors_words$NN2[i],
             nearest_neighbors_words$NN3[i]))
}
```

*The results show patterns of similarity across European parliaments in how they allocate attention to CAP topics. **Austria (AT)**, for instance, is most similar to **Hungary, Greece and Estonia** - two of which (**HU** and **EE**) were also Austria's nearest neighbors in **Section 3.3.** of this tutorial, when we analyzed sentiment profiles.*

*Overall, we can identify some **regional clusters**, like **Bosnia (BA)** which stands out with neighbors **RS, UA** and **LV**, indicating different positioning compared to Western European parliaments. Northern and Western European countries also show close alignment: **Denmark (DK)** with **Sweden (SE), Netherlands (NL)** and **Austria (AT)**. Some smaller countries, such as **Iceland (IS)**, align with multiple Central and Southern European countries (**Greece, Hungary, Czech Republic**), reflecting common topic priorities despite their geographic distance.*

---

## 5. Conclusion

In this tutorial, we extended the analysis from **country-specific descriptive statistics** to **cross-parliament comparisons**. With the help of **cosine distance**, we were able to:
- Measure **how similar or different countries are** in the topics they discuss and the way they express sentiment in parliament.
- Find **groups of countries that behave similarly** (nearest neighbors and clusters).
- Spot **outliers** - countries that stand out because they focus on different topics or express sentiment in a different way.

Overall, the combination of **numerical measures, heatmaps and dendrograms** provides a powerful toolkit for exploring **comparative parliamentary discourse**.

## What's Next?

In the next tutorial (**Tutorial 4**), we will shift our focus from static comparisons to **how sentiment and attention change over time**. We will explore weekly patterns in parliamentary debates, showing which topics become more positive or negative and how much attention different countries devote to key issues like 'Health'.

