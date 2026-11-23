# GTA Reviews: R Data Analysis Mini Project

This project analyzes the review dataset supplied in `dataset gta.zip`. The CSV contains 46,105 records with platform, review ID, publication date, a 0–10 score, and free-text review.

## Questions

- How are scores distributed, and what share of reviews score 7–10?
- How do average scores and review volume compare across platforms?
- How did review volume change by publication day?
- Which words appear most often in the review text?
- Are there repeated review IDs or missing values?

## Run the analysis

R 4.6.2 is installed for the current Windows user. Open PowerShell in this project folder, then run:

```powershell
& "$env:LOCALAPPDATA\Programs\R\R-4.6.2\bin\Rscript.exe" scripts/analyze_reviews.R
```

The script uses base R only. It removes repeated review IDs (keeping the first row), excludes records without a valid score or date, and writes summary CSV files and PNG charts to `output/`.

## Files

- `data/reviews.csv` — source dataset extracted from the supplied ZIP
- `scripts/analyze_reviews.R` — reproducible analysis
- `output/` — generated tables and charts

Generated outputs include `kpis.csv`, `platform_summary.csv`, `score_distribution.csv`, `rating_bands.csv`, `daily_summary.csv`, `top_terms.csv`, and nine PNG charts covering scores, rating bands, platform comparisons, daily trends, common terms, and review length.

For a clear overview, open `output/analysis_report.html` in a web browser. It brings the key numbers, takeaways, comparison tables, and charts into one page. The chart notes explain what each view shows, and the term counts are described as word frequency rather than sentiment.

The score bands are descriptive: low = 0–4, mixed = 5–6, high = 7–10. The word-frequency section is a simple exploratory count, not sentiment analysis. Treat platform comparisons descriptively because the platform sample sizes are very different.
