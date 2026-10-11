# Instagram Trend and Engagement Analytics Using Hadoop Ecosystem

## 📌 Project Overview

This project analyzes Instagram post data using the Hadoop ecosystem to identify patterns in engagement, content performance, posting activity, reach, and follower growth.

The project uses **HDFS, Apache Pig, Hadoop MapReduce, and Apache Hive** for data storage, preprocessing, and analytics. An interactive dashboard developed using **R Shiny** presents the results through key performance indicators, charts, and data exploration features.

## 🎯 Objectives

* Store Instagram post data using HDFS.
* Clean and transform data using Apache Pig.
* Perform distributed analytics using Hadoop MapReduce.
* Execute structured analytical queries using Apache Hive.
* Analyze engagement across media types, content categories, traffic sources, and hashtag counts.
* Examine posting-day and posting-hour patterns.
* Analyze average reach and followers gained by content category.
* Visualize analytical findings through an interactive R Shiny dashboard.

## 🛠️ Technology Stack

| Technology        | Purpose                             |
| ----------------- | ----------------------------------- |
| Java              | Runtime environment for Hadoop      |
| Hadoop 3.3.6      | Distributed data processing         |
| HDFS              | Dataset storage                     |
| Apache Pig 0.18.0 | Data cleaning and transformation    |
| Hadoop MapReduce  | Distributed analytical operations   |
| Apache Hive 4.0.1 | Structured querying and aggregation |
| R                 | Data analysis and visualization     |
| R Shiny           | Interactive dashboard               |
| Plotly            | Interactive charts                  |
| DT                | Interactive data tables             |


## 📊 Dataset

The project uses an Instagram Analytics Dataset containing **29,999 post records and 23 attributes**.


## 🔍 Analytical Operations

### Hadoop MapReduce

The project performs the following analyses:

1. Total engagement by media type.
2. Total engagement by content category.
3. Average engagement by traffic source.
4. Average engagement by hashtag count.

Total engagement is calculated as:

```text
Total Engagement = Likes + Comments + Shares + Saves
```

### Apache Hive

Hive queries are used to analyze:

* Engagement by content category.
* Engagement by posting day.
* Engagement by posting hour.
* Average reach by content category.
* Average followers gained by content category.

## 📈 Dashboard Features

The R Shiny dashboard contains four sections:

### 1. Overview

Displays key performance indicators:

* Total Posts
* Total Engagement
* Average Reach
* Average Followers Gained

### 2. Engagement Analysis

Explores engagement patterns across media types, content categories, traffic sources, and hashtag counts.

### 3. Audience & Content

Presents insights into average reach and follower growth across content categories.

### 4. Data Explorer

Allows users to inspect the cleaned Instagram post records.

## 📋 Key Results

The analysis of the dataset produced the following findings:

* **Image posts** recorded the highest total engagement among the analyzed media types.
* **Fashion** recorded the highest total engagement among content categories.
* **Music** had the highest average followers gained by category.
* **External traffic** recorded the highest average engagement among traffic sources.
* Engagement varied across different hashtag counts.

These findings describe the analyzed dataset and should not be interpreted as universal Instagram trends.


## 🚀 Running the Dashboard

1. Install R and RStudio.
2. Open the dashboard project folder in RStudio.
3. Install any missing packages:

```r
install.packages(c("shiny", "bslib", "plotly", "DT", "dplyr"))
```

4. Ensure the cleaned dataset exists at:

```text
data/cleaned_instagram_data.csv
```

5. Open `app.R` and click **Run App**, or execute:

```r
shiny::runApp()
```

The dashboard uses the cleaned CSV file, so Hadoop services are not required just to run the dashboard when the file and required R packages are available.

## 📁 Dashboard Structure

```text
instagram_dashboard/
├── app.R
├── data/
│   └── cleaned_instagram_data.csv
├── R/
│   ├── data.R
│   ├── overview.R
│   ├── engagement.R
│   ├── audience.R
│   ├── explorer.R
│   └── helpers.R
└── www/
    └── style.css
```


