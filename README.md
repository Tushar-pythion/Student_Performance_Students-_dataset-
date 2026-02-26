# Student_Performance_Students-_dataset-
---
title: "DMV CASE STUDY"
author: "TUSHAR BHITE"
date: "2026-02-26"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(corrplot)
```

```{r}
# Load dataset
students <- read.csv("StudentsPerformance.csv")

# Basic checks
head(students)
str(students)
dim(students)
```
```{r}
# Column names
colnames(students)

# Summary statistics
summary(students)
```
```{r}
# Check missing values
colSums(is.na(students))
```
```{r}
students$gender <- as.factor(students$gender)
students$race.ethnicity <- as.factor(students$race.ethnicity)
students$parental.level.of.education <- as.factor(students$parental.level.of.education)
students$lunch <- as.factor(students$lunch)
students$test.preparation.course <- as.factor(students$test.preparation.course)
```

```{r}
ggplot(students, aes(x = gender)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Gender Distribution",
       x = "Gender",
       y = "Count")
```
```{r}
ggplot(students, aes(x = math.score)) +
  geom_histogram(bins = 10, fill = "orange", color = "black") +
  labs(title = "Distribution of Math Scores",
       x = "Math Score",
       y = "Frequency")
```
```{r}
ggplot(students, aes(x = gender, y = math.score, fill = gender)) +
  geom_boxplot() +
  labs(title = "Math Score by Gender",
       x = "Gender",
       y = "Math Score")
```
```{r}
ggplot(students, aes(x = test.preparation.course, y = math.score,
                     fill = test.preparation.course)) +
  geom_boxplot() +
  labs(title = "Math Score vs Test Preparation Course",
       x = "Test Preparation",
       y = "Math Score")
```
```{r}
# Select numeric columns
score_data <- students %>%
  select(math.score, reading.score, writing.score)

# Correlation matrix
cor_matrix <- cor(score_data)

# Correlation plot
corrplot(cor_matrix, method = "number")
```
```{r}
students %>%
  group_by(gender) %>%
  summarise(
    Avg_Math = mean(math.score),
    Avg_Reading = mean(reading.score),
    Avg_Writing = mean(writing.score)
  )
```
```{r}
students %>%
  group_by(lunch) %>%
  summarise(
    Avg_Math = mean(math.score),
    Avg_Reading = mean(reading.score),
    Avg_Writing = mean(writing.score)
  )
```
```{r}
ggplot(students,
       aes(x = parental.level.of.education, y = math.score)) +
  geom_boxplot(fill = "lightgreen") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Math Score vs Parental Education Level",
       x = "Parental Education Level",
       y = "Math Score")
```
```{r}
# Create total score
students$total_score <- students$math.score +
  students$reading.score +
  students$writing.score

# Summary of total score
summary(students$total_score)
```
