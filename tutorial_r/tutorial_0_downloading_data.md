# Tutorial 0: Downloading Data from CROSSDA


This tutorial will guide you through downloading parliamentary speech data from **CROSSDA** (the Croatian Social Science Data Archive). We'll be working with the **ParlaCAP** dataset, which contains parliamentary speeches from multiple European countries.

---

## Step 1: Download the Data Manually

Before we use R, we need to download the data files from the CROSSDA repository. Follow these steps:

1. **Go to the repository**: Open your web browser and navigate to the ParlaCAP dataset:
   - [https://doi.org/10.23669/1ZTELP](https://doi.org/10.23669/1ZTELP)

2. **Search for the files**: In the repository, search for files containing `*speeches_no_text`

3. **Select ALL files**: 
   - Check the boxes next to all the 28 speech files
   - **Important**: Make sure to look at additional pages—not all files may be shown on the first page!

4. **Download**: Click the "Download" button

5. **Accept terms**: Read and accept the terms and conditions

6. **Save the file**: Save the downloaded zip file (`dataverse_files.zip`) in your R project folder (the same folder where this tutorial is located)

> **Tip**: If you're not sure where your R project folder is, you can check by running `getwd()` in R. This shows your current "working directory."

---

## Step 2: Extract the Files Using R

Now we'll use R to unzip the downloaded file and organize our data. This is where the magic happens!

### 2.1 Check that the file exists

First, let's make sure R can find the downloaded zip file:

```r
# Check if the zip file is in our working directory
file.exists("dataverse_files.zip")
```

If this returns `TRUE`, you're good to go! If it returns `FALSE`, make sure you saved the zip file in the correct folder.

### 2.2 Unzip the file

R has a built-in function called `unzip()` that can extract files from a zip archive:

```r
# Extract all files from the zip archive
unzip("dataverse_files.zip")
```

**What does this do?** The `unzip()` function opens the zip file and extracts all its contents into your current folder. You should now see a new folder called `speeches_no_text`.

### 2.3 Clean up (optional)

Now that we've extracted the files, we don't need the zip file anymore. Let's delete it to save space:

```r
# Remove the zip file since we no longer need it
file.remove("dataverse_files.zip")
```

**What does this do?** The `file.remove()` function deletes the specified file. It returns `TRUE` if successful.

### 2.4 Verify everything worked

Let's check that our new folder exists and see what's inside:

```r
# Check if the folder was created
dir.exists("speeches_no_text")

# List all files in the new folder
list.files("speeches_no_text")
```

You should see a list of `.tsv.zip` files, one for each country in the dataset (like `ParlaCAP-DK_speeches_no_text.tsv.zip` for Denmark, `ParlaCAP-GB_speeches_no_text.tsv.zip` for Great Britain, etc.).


## What's Next?

In **Tutorial 1**, we'll learn how to:
- Load the extracted data into R
- Explore the structure of the parliamentary speech data
- Perform basic data manipulation
