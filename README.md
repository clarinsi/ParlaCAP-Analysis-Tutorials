# Parliamentary Speech Analysis with ParlaMint

This repository contains **multiple tutorials for analyzing parliamentary speeches** across multiple European countries.
The notebooks combine **processing of ParlaMint data, sentiment analysis, party comparisons, and cross-country analyses**, enabling students and researchers to study the **tone** and **content** of parliamentary debates systematically.

These notebooks can be downloaded to run in Python and R, but were adapted to run in Google Colab as well (suitable for people with little to no programming knowledge).

**Main author of the tutorials:** Isabell Furkert

This work is supported by the OSCARS project — and its ParlaCAP cascading grant project —, which has received funding from the European Commission's Horizon Europe Research and Innovation programme under grant agreement No. 101129751.

## Getting Started

- **Just want to explore the analyses?** Start with Tutorials 1-5. In Google Colab they load `ParlaCAP.data.parquet` (~17 GB, the full dataset) automatically from Hugging Face, so no setup is required. If you run the notebooks locally in Python or R, follow the download instructions in each notebook.
- **Want to fetch live or custom-filtered data?** Tutorial 0 is an optional, advanced tutorial showing how to query the ParlaCAP API and download your own data subsets.

## Contents

| Notebook | Description | Time | Open |
|---|---|---|---|
| Tutorial 0 | Downloading the data (via API) | 2 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_0_API_Access_and_Authentication.ipynb) |
| Tutorial 1 | First Steps with the ParlaCAP Dataset | 10 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_1_First_steps_with_ParlaMint_dataset.ipynb) |
| Tutorial 2 | Topic & Sentiment Distributions | 25 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_2_Calculating_Topic_and_Sentiment_Distributions.ipynb) |
| Tutorial 3 | Cross-Parliament Comparison | 30 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_3_Cross_Parliament_Comparison.ipynb) |
| Tutorial 4 | Sentiment & Frequency of Topics over Time | 30 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_4_Sentiment_x_Topics_over_Time.ipynb) |
| Tutorial 5 | Coalition vs. Opposition Dynamics | 35 min | [![Open In Colab](https://colab.research.google.com/assets/colab-badge.svg)](https://colab.research.google.com/github/Isabell-Furkert-129/ParlaCAP-Colab-Tutorials/blob/main/notebook_google_colab/Tutorial_5_Coalition_vs_Opposition_Dynamics.ipynb) |

## Workflow Overview

The analysis workflow is organized across **six tutorials**. Tutorial 0 is a standalone, optional data-download tutorial, while Tutorials 1-5 each build on the previous one.

### Tutorial 0 — Downloading the Data via API

Shows how to use the ParlaCAP API to query and download your own data subsets.

### Tutorial 1 — First Steps with the ParlaCAP Dataset

**Objective:**
This tutorial introduces the **ParlaCAP dataset**, built on the ParlaMint corpus of European parliamentary debates. It is designed for researchers, data scientists, and political science enthusiasts who want to analyze parliamentary proceedings across different European countries.

**Dataset Overview:**
- **ParlaMint** contains transcribed parliamentary speeches annotated with detailed metadata, including speaker role (e.g., Member of Parliament), party affiliation, session date, gender and speech length.
    + Extension: **ParlaCAP** categorizes each speech segment into a policy domain from the **Comparative Agendas Project (CAP)**, such as healthcare, education, or foreign affairs.
    + Extension: **ParlaSent** provides sentiment scores for each segment, enabling analysis of the emotional tone of debates.

### Tutorial 2 — Topic & Sentiment Distributions

**Objective:**
This tutorial performs **Exploratory Data Analysis (EDA)** on the ParlaCAP dataset, providing a descriptive overview of **policy topics (CAP categories)** and **sentiment** in parliamentary debates.

**Possible Research Questions:**
- Which CAP categories dominate parliamentary debates, and do distributions differ across countries?
- How does average sentiment vary by topic and country?
- Does party affiliation or coalition status influence the tone of debates?

**Analyses & Visualizations used in this Tutorial:**
- **Histograms and barplots** showing CAP category distributions across parliaments
- **Sentiment distributions** for individual topics within a single country and across multiple countries
- **Comparisons by party status** (Coalition vs. Opposition) to explore differences in tone between governing and opposing parties
- **Summary tables** of average sentiment by topic and country, exportable as TSV for further research
- **Scatterplots** linking topic prominence and sentiment, highlighting correlations between how much a topic is discussed and the tone used

This tutorial sets the foundation for cross-country comparisons and party-based analyses in subsequent tutorials.

### Tutorial 3 — Cross-Parliament Comparison

**Objective:**
This tutorial extends the analysis from **Tutorial 2** by moving from descriptive, country-specific observations to **quantitative comparison and clustering** of parliamentary discourse. It focuses on measuring **similarity and differences across parliaments** using computational methods from **NLP** and **Computational Linguistics**.

**Key Concept:**
- Each country is represented as a **high-dimensional vector** where:
  - Each dimension corresponds to a **CAP category**.
  - Values can represent **total word counts** (topic prevalence) or **average sentiment** (emotional tone).
- **Cosine distance** measures the similarity between these vectors, allowing for comparison independent of text volume.

**Research Questions:**
- Which countries exhibit the most similar sentiment patterns across CAP categories?
- How similar are countries in their topic distributions based on word counts?
- For a specific CAP category, which countries have the most similar sentiment patterns?

**Analysis Steps:**
- Introduction to **Cosine Distance**
- Compute cosine distance between countries based on **sentiment**:
  1. Example comparison: Serbia (RS) & Croatia (HR)
  2. All countries combined
  3. Identify the 3 nearest neighbors for each country
- Compute cosine distance between **topic distributions** (word counts):
  1. Example comparison: Serbia (RS) & Croatia (HR)
  2. All countries combined
  3. Identify the 3 nearest neighbors for each country

**Outcome:**
This tutorial provides a systematic, data-driven framework for **cross-parliament comparison**, revealing clusters of similar parliaments, identifying outliers, and quantifying differences in both topic focus and sentiment patterns.

### Tutorial 4 — Sentiment & Frequency of Topics over Time

**Objective:**
This tutorial explores **temporal dynamics** in parliamentary debates, building on previous analyses of topics and sentiment. It focuses on how **topic prevalence and sentiment change over time** within and across parliaments.

**Key Concepts:**
- **Time series analysis** allows tracking of trends over regular intervals (e.g., weekly).
- **Average sentiment per week** highlights shifts in tone for specific topics or overall discourse.
- **Weekly word counts** reveal the attention parliaments devote to different CAP categories over time.

**Research Questions:**
- Are there changes in sentiment across CAP categories over time in a specific parliament (e.g., Serbia)?
- Did sentiment change noticeably during impactful events, such as the COVID-19 outbreak?
- How does sentiment toward a specific topic (e.g., Health) differ across countries like Serbia (RS), Croatia (HR), and Hungary (HU)?
- How does parliamentary attention (measured via weekly word count) evolve over time for specific topics?

**Analysis Steps:**
- Select a timeframe for analysis
- Calculate and visualize sentiment over time for a selected country:
  1. Across all topics
  2. For one specific topic (e.g., Health)
- Analyze average sentiment scores by country and CAP category using heatmaps
- Compute and visualize **average weekly word counts** for selected countries and topics

**Outcome:**
This tutorial provides insights into **how sentiment and topic attention evolve over time**, enabling researchers to detect peaks, dips, and shifts in parliamentary focus that may correspond to political events, crises, or debates.

### Tutorial 5 — Coalition vs. Opposition Dynamics

**Objective:**
This tutorial combines previous analyses of **topic distributions and sentiment** to examine **party alignment**, focusing on differences between **coalition and opposition parties**. It extends the analysis from a single parliament to **multiple countries**, highlighting patterns that are robust across parliamentary systems.

**Key Concepts:**
- **Party status (Coalition vs. Opposition)** as a lens to analyze sentiment and topic emphasis
- **Cross-country comparison** using cosine distances, dendrograms, and nearest neighbors
- **Relative word count differences** to quantify which parties dominate debates in each CAP category

**Research Questions:**
- Do coalitions and oppositions across parliaments agree on which topics are important?
- Do coalitions and oppositions across parliaments express similar sentiment for the same topics?
- Which countries or topics show the strongest divergence in tone or focus between coalition and opposition parties?

**Analysis Steps:**
1. Compute **mean sentiment** by country and CAP category for coalition and opposition parties
   - Visualize with heatmaps
   - Explore cross-country patterns using **cosine distances** and **dendrograms**
2. Compute **topic shares** (proportion of words per CAP category) for coalition and opposition
   - Visualize with heatmaps
   - Analyze cross-country similarities and differences
3. Compute **relative word count differences**
   - Identify CAP categories or parliaments dominated by coalition or opposition speeches

**Outcome:**
This tutorial provides a systematic framework to analyze how **political alignment influences both the tone and focus of parliamentary debates**, allowing for cross-country comparisons and identification of both common patterns and exceptional cases.


### Source

Erjavec, Tomaž; et al. (2025). *Multilingual comparable corpora of parliamentary debates ParlaMint 5.0*. Slovenian language resource repository CLARIN.SI. ISSN 2820-4042. [hdl.handle.net/11356/2004](https://hdl.handle.net/11356/2004)
