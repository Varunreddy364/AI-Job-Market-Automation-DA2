dim(ai_impact_jobs_2010_2025)
str(ai_impact_jobs_2010_2025)
summary(ai_impact_jobs_2010_2025)
colSums(is.na(ai_impact_jobs_2010_2025))
sum(duplicated(ai_impact_jobs_2010_2025))

ai_impact_jobs_2010_2025$ai_keywords[is.na(ai_impact_jobs_2010_2025$ai_keywords)] <- "Not Specified"

ai_impact_jobs_2010_2025$ai_skills[is.na(ai_impact_jobs_2010_2025$ai_skills)] <- "Not Specified"

colSums(is.na(ai_impact_jobs_2010_2025))

library(readr)

write_csv(
  ai_impact_jobs_2010_2025,
  "C:/Users/varun/Desktop/cleaned_ai_impact_jobs_2010_2025.csv"
)
colSums(ai_impact_jobs_2010_2025 == "", na.rm = TRUE)

boxplot(ai_impact_jobs_2010_2025$salary_usd,
        main = "Salary Distribution")

hist(ai_impact_jobs_2010_2025$salary_usd,
     main = "Distribution of Annual Salaries",
     xlab = "Salary (USD)",
     ylab = "Frequency",
     col = "lightblue",
     border = "black")

industry_counts <- sort(table(ai_impact_jobs_2010_2025$industry), decreasing = TRUE)

barplot(industry_counts,
        main = "Number of Job Postings by Industry",
        xlab = "Industry",
        ylab = "Number of Job Postings",
        col = "skyblue",
        las = 2)

country_counts <- sort(table(ai_impact_jobs_2010_2025$country), decreasing = TRUE)

top10 <- head(country_counts, 10)

barplot(top10,
        horiz = TRUE,
        main = "Top 10 Countries by AI Job Postings",
        xlab = "Number of Job Postings",
        col = "lightgreen",
        border = "black",
        las = 1)

adoption_counts <- table(ai_impact_jobs_2010_2025$industry_ai_adoption_stage)

pie(adoption_counts,
    main = "Distribution of Industry AI Adoption Stages",
    col = rainbow(length(adoption_counts)))



boxplot(salary_usd ~ seniority_level,
        data = ai_impact_jobs_2010_2025,
        main = "Salary Distribution by Experience Level",
        xlab = "Experience Level",
        ylab = "Salary (USD)",
        col = c("lightblue", "lightgreen", "lightpink", "lightyellow"))


plot(ai_impact_jobs_2010_2025$ai_intensity_score,
     ai_impact_jobs_2010_2025$automation_risk_score,
     main = "AI Intensity Score vs Automation Risk Score",
     xlab = "AI Intensity Score",
     ylab = "Automation Risk Score",
     pch = 19,
     col = "blue")

install.packages("corrplot", dependencies = TRUE)

numeric_data <- ai_impact_jobs_2010_2025[, sapply(ai_impact_jobs_2010_2025, is.numeric)]

cor_matrix <- cor(numeric_data)

corrplot(cor_matrix,
         method = "color",
         type = "upper",
         tl.col = "black",
         tl.srt = 45)






