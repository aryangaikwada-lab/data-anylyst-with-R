# GTA Review Analysis (base R only)
# Run from the project root: Rscript scripts/analyze_reviews.R

input_file <- "data/reviews.csv"
output_dir <- "output"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

reviews <- read.csv(input_file, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                    na.strings = c("", "NA"))
required <- c("platform", "review_id", "published_date", "score", "review_text")
missing_columns <- setdiff(required, names(reviews))
if (length(missing_columns) > 0) stop("Missing required columns: ", paste(missing_columns, collapse = ", "))
reviews$published_date <- as.Date(reviews$published_date)
reviews$score <- suppressWarnings(as.numeric(reviews$score))

# Keep the first record for repeated IDs and report how many were removed.
input_rows <- nrow(reviews)
duplicate_id_rows <- sum(duplicated(reviews$review_id))
reviews <- reviews[!duplicated(reviews$review_id), ]
after_dedup_rows <- nrow(reviews)
reviews <- reviews[!is.na(reviews$score) & !is.na(reviews$published_date), ]
invalid_rows_removed <- after_dedup_rows - nrow(reviews)
reviews$review_length <- nchar(reviews$review_text, type = "chars")
reviews$day <- format(reviews$published_date, "%Y-%m-%d")

# Overall indicators and data quality
kpis <- data.frame(
  metric = c("Rows in source", "Duplicate ID rows removed", "Invalid score/date rows removed",
             "Reviews analyzed", "Average score", "Median score",
             "High-score share (7-10, %)", "Average review length (characters)"),
  value = c(input_rows, duplicate_id_rows, invalid_rows_removed, nrow(reviews),
            mean(reviews$score), median(reviews$score),
            100 * mean(reviews$score >= 7), mean(reviews$review_length))
)
write.csv(kpis, file.path(output_dir, "kpis.csv"), row.names = FALSE)

platform_summary <- do.call(rbind, lapply(split(reviews, reviews$platform), function(d) {
  data.frame(platform = d$platform[1], review_count = nrow(d),
             average_score = mean(d$score), median_score = median(d$score),
             high_score_share_pct = 100 * mean(d$score >= 7),
             average_review_length = mean(d$review_length))
}))
platform_summary <- platform_summary[order(-platform_summary$review_count), ]
score_distribution <- as.data.frame(table(score = factor(reviews$score, levels = 0:10)))
names(score_distribution) <- c("score", "review_count")
score_distribution$score <- as.integer(as.character(score_distribution$score))
score_distribution$share_pct <- 100 * score_distribution$review_count / sum(score_distribution$review_count)

reviews$rating_band <- ifelse(reviews$score <= 4, "Low (0-4)",
                              ifelse(reviews$score <= 6, "Mixed (5-6)", "High (7-10)"))
rating_bands <- as.data.frame(table(rating_band = factor(reviews$rating_band,
                         levels = c("Low (0-4)", "Mixed (5-6)", "High (7-10)"))))
names(rating_bands) <- c("rating_band", "review_count")
rating_bands$share_pct <- 100 * rating_bands$review_count / sum(rating_bands$review_count)

daily_count <- aggregate(review_id ~ day, data = reviews, FUN = length)
names(daily_count)[2] <- "review_count"
daily_score <- aggregate(score ~ day, data = reviews, FUN = mean)
names(daily_score)[2] <- "average_score"
daily_summary <- merge(daily_count, daily_score, by = "day")
daily_summary <- daily_summary[order(daily_summary$day), ]
write.csv(platform_summary, file.path(output_dir, "platform_summary.csv"), row.names = FALSE)
write.csv(score_distribution, file.path(output_dir, "score_distribution.csv"), row.names = FALSE)
write.csv(rating_bands, file.path(output_dir, "rating_bands.csv"), row.names = FALSE)
write.csv(daily_summary, file.path(output_dir, "daily_summary.csv"), row.names = FALSE)

# Exploratory English/Spanish term counts, excluding common filler words.
stop_words <- c("the", "and", "for", "you", "that", "this", "with", "was", "are", "but",
                "have", "has", "had", "not", "from", "they", "them", "their", "its", "just",
                "very", "more", "your", "all", "can", "will", "would", "about", "what", "when",
                "where", "who", "how", "why", "too", "one", "two", "there", "here", "been",
                "into", "out", "get", "got", "like", "than", "then", "also", "only", "game",
                "gta", "gta vi", "gta6", "gta6", "rockstar", "review", "it", "is", "in", "of",
                "to", "a", "an", "on", "at", "as", "by", "be", "or", "if", "i", "me", "my",
                "we", "our", "he", "she", "his", "her", "do", "did", "so", "no", "yes",
                "que", "para", "pero", "con", "por", "una", "uno", "los", "las", "del",
                "como", "esto", "esta", "este", "sus", "sin", "más", "mas", "porque", "fue")
text <- tolower(paste(reviews$review_text, collapse = " "))
text <- gsub("http[s]?://[^[:space:]]+", " ", text, perl = TRUE)
tokens <- unlist(strsplit(gsub("[^[:alpha:]]+", " ", text, perl = TRUE), "[[:space:]]+"))
tokens <- tokens[nchar(tokens) >= 3 & !tokens %in% stop_words]
term_counts <- sort(table(tokens), decreasing = TRUE)
top_terms <- head(data.frame(term = names(term_counts), count = as.integer(term_counts)), 15)
top_terms$share_of_content_terms_pct <- round(100 * top_terms$count / sum(term_counts), 3)
write.csv(top_terms, file.path(output_dir, "top_terms.csv"), row.names = FALSE)

# Charts
png(file.path(output_dir, "score_distribution.png"), width = 1000, height = 650)
barplot(score_distribution$review_count, names.arg = score_distribution$score,
        col = "steelblue", xlab = "Score (0-10)", ylab = "Reviews",
        main = "GTA Review Score Distribution")
dev.off()

png(file.path(output_dir, "scores_by_platform.png"), width = 1000, height = 650)
barplot(platform_summary$average_score, names.arg = platform_summary$platform,
        col = "darkseagreen3", ylim = c(0, 10), ylab = "Average score (0-10)",
        main = "Average Review Score by Platform")
dev.off()

png(file.path(output_dir, "daily_review_volume.png"), width = 1100, height = 650)
plot(as.Date(daily_summary$day), daily_summary$review_count, type = "o", pch = 16,
     col = "firebrick", xlab = "Publication date", ylab = "Reviews",
     main = "Review Volume by Day")
dev.off()

png(file.path(output_dir, "rating_bands_by_platform.png"), width = 1000, height = 650)
band_by_platform <- prop.table(table(
  factor(reviews$platform, levels = platform_summary$platform),
  factor(reviews$rating_band, levels = c("Low (0-4)", "Mixed (5-6)", "High (7-10)"))
), margin = 1) * 100
barplot(t(band_by_platform), col = c("indianred2", "goldenrod1", "seagreen3"),
        ylim = c(0, 100), ylab = "Share of reviews (%)", xlab = "Platform",
        main = "Rating Bands by Platform", legend.text = TRUE,
        args.legend = list(x = "topright", bty = "n", cex = 0.9))
dev.off()

png(file.path(output_dir, "review_count_by_platform.png"), width = 1000, height = 650)
par(mar = c(8, 5, 4, 1))
barplot(platform_summary$review_count, names.arg = platform_summary$platform, las = 2,
        col = "mediumpurple3", ylab = "Reviews",
        main = "Review Count by Platform")
dev.off()

png(file.path(output_dir, "average_daily_score.png"), width = 1100, height = 650)
plot(as.Date(daily_summary$day), daily_summary$average_score, type = "o", pch = 16,
     col = "darkorange3", ylim = c(0, 10), xlab = "Publication date",
     ylab = "Average score (0-10)", main = "Average Score by Day")
dev.off()

png(file.path(output_dir, "score_spread_by_platform.png"), width = 1000, height = 650)
boxplot(score ~ platform, data = reviews, col = "skyblue2", ylim = c(0, 10),
        xlab = "Platform", ylab = "Score (0-10)",
        main = "Score Spread by Platform")
dev.off()

png(file.path(output_dir, "top_review_terms.png"), width = 1100, height = 800)
par(mar = c(5, 10, 4, 1))
barplot(rev(top_terms$count), names.arg = rev(top_terms$term), horiz = TRUE,
        las = 1, col = "darkcyan", xlab = "Occurrences",
        main = "15 Most Frequent Content Terms",
        sub = "Common English and Spanish filler words removed")
dev.off()

png(file.path(output_dir, "review_length_distribution.png"), width = 1000, height = 650)
hist(reviews$review_length, breaks = 40, col = "tan2", border = "white",
     xlab = "Characters per review", ylab = "Reviews",
     main = "Review Text Length Distribution")
dev.off()

# A single, readable dashboard that brings the main findings and charts together.
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}
html_table <- function(data) {
  header <- paste0("<thead><tr>", paste0("<th>", html_escape(names(data)), "</th>", collapse = ""), "</tr></thead>")
  body <- apply(data, 1, function(row) paste0("<tr>",
    paste0("<td>", html_escape(row), "</td>", collapse = ""), "</tr>"))
  paste0("<table>", header, "<tbody>", paste(body, collapse = ""), "</tbody></table>")
}
platform_names <- c("playstation-5" = "PlayStation 5", "xbox-series-x" = "Xbox Series X")
pretty_platform <- function(x) ifelse(x %in% names(platform_names), unname(platform_names[x]), x)
mode_row <- score_distribution[which.max(score_distribution$review_count), ]
peak_day <- daily_summary[which.max(daily_summary$review_count), ]
platform_score_order <- platform_summary[order(-platform_summary$average_score), ]
platform_count_share <- 100 * platform_summary$review_count[1] / sum(platform_summary$review_count)
platform_score_gap <- diff(range(platform_summary$average_score))
date_range <- paste(format(min(reviews$published_date), "%d %b %Y"),
                    format(max(reviews$published_date), "%d %b %Y"), sep = " to ")

platform_table <- data.frame(
  Platform = pretty_platform(platform_summary$platform),
  Reviews = format(platform_summary$review_count, big.mark = ","),
  `Average score` = sprintf("%.2f / 10", platform_summary$average_score),
  `Median score` = sprintf("%.0f / 10", platform_summary$median_score),
  `Scores 7-10` = sprintf("%.1f%%", platform_summary$high_score_share_pct),
  check.names = FALSE
)
band_table <- data.frame(
  `Score group` = rating_bands$rating_band,
  Reviews = format(rating_bands$review_count, big.mark = ","),
  `Share of reviews` = sprintf("%.1f%%", rating_bands$share_pct),
  check.names = FALSE
)
term_table <- data.frame(
  `Content word` = top_terms$term,
  Occurrences = format(top_terms$count, big.mark = ","),
  `Share of content words` = sprintf("%.3f%%", top_terms$share_of_content_terms_pct),
  check.names = FALSE
)
high_share <- 100 * mean(reviews$score >= 7)
low_share <- 100 * mean(reviews$score <= 4)
most_common_score_share <- 100 * mode_row$review_count / nrow(reviews)
winner_label <- pretty_platform(platform_score_order$platform[1])
relative_gap <- sprintf("%.2f points", platform_score_gap)

chart_cards <- c(
  '<article class="chart"><h3>Score distribution</h3><p>Number of reviews at each score from 0 to 10.</p><img src="score_distribution.png" alt="Bar chart of review counts for each score"></article>',
  '<article class="chart"><h3>Average score by platform</h3><p>Mean score for each platform; the scale runs from 0 to 10.</p><img src="scores_by_platform.png" alt="Average review score by platform"></article>',
  '<article class="chart"><h3>Rating mix by platform</h3><p>Each bar totals 100%, so compare the shares of low, mixed, and high scores.</p><img src="rating_bands_by_platform.png" alt="100 percent stacked rating bands by platform"></article>',
  '<article class="chart"><h3>Review volume by platform</h3><p>The platform sample sizes differ, so score comparisons are descriptive.</p><img src="review_count_by_platform.png" alt="Number of reviews by platform"></article>',
  '<article class="chart"><h3>Reviews by day</h3><p>Shows when reviews were published in the dataset.</p><img src="daily_review_volume.png" alt="Review counts by publication day"></article>',
  '<article class="chart"><h3>Average score by day</h3><p>Daily averages can move more when a day has fewer reviews.</p><img src="average_daily_score.png" alt="Daily average review score"></article>',
  '<article class="chart"><h3>Score spread by platform</h3><p>The box shows the middle half of scores; the line marks the median.</p><img src="score_spread_by_platform.png" alt="Boxplots of review scores by platform"></article>',
  '<article class="chart"><h3>Most frequent content words</h3><p>Common English and Spanish filler words were filtered. Counts are word occurrences, not review counts or sentiment.</p><img src="top_review_terms.png" alt="Top 15 content word counts"></article>',
  '<article class="chart"><h3>Review length</h3><p>Each review is measured by its number of characters.</p><img src="review_length_distribution.png" alt="Histogram of review text length"></article>'
)

html_report <- c(
  '<!doctype html><html lang="en"><head><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  '<title>GTA Reviews | Analysis Dashboard</title><style>',
  'body{margin:0;background:#f4f6fb;color:#182230;font:16px/1.55 "Segoe UI",Arial,sans-serif}',
  '.wrap{max-width:1180px;margin:auto;padding:32px 22px 54px}.hero{background:#172554;color:white;padding:30px 34px;border-radius:18px}',
  '.eyebrow{color:#bfdbfe;text-transform:uppercase;letter-spacing:.12em;font-size:12px;font-weight:700}.hero h1{margin:5px 0;font-size:34px}.hero p{margin:8px 0 0;color:#dbeafe}',
  '.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(170px,1fr));gap:14px;margin:20px 0}.card,.panel,.chart{background:white;border:1px solid #e2e8f0;border-radius:14px;padding:18px;box-shadow:0 3px 12px #0f172a0a}',
  '.card strong{display:block;font-size:27px;color:#1d4ed8}.card span{color:#526174;font-size:13px}.panel{margin:18px 0}.panel h2,.section-title{margin:0 0 10px;color:#172554}',
  '.insights{padding-left:20px;margin-bottom:0}.insights li{margin:7px 0}.grid{display:grid;grid-template-columns:repeat(auto-fit,minmax(min(100%,480px),1fr));gap:16px}',
  '.chart{padding:18px}.chart h3{margin:0;color:#1e3a8a}.chart p{margin:4px 0 12px;color:#526174;font-size:14px}.chart img{display:block;width:100%;height:auto;border-radius:8px}',
  'table{border-collapse:collapse;width:100%;margin:10px 0 2px;font-size:14px}th,td{text-align:left;padding:10px 12px;border-bottom:1px solid #e2e8f0}th{background:#eff6ff;color:#1e3a8a}tr:last-child td{border-bottom:0}',
  '.note{color:#526174;font-size:14px}.files a{color:#1d4ed8;margin-right:14px}.footer{color:#64748b;font-size:13px;margin-top:22px}@media(max-width:600px){.wrap{padding:16px 12px}.hero{padding:22px}.hero h1{font-size:27px}}',
  '</style></head><body><main class="wrap">',
  '<header class="hero"><div class="eyebrow">Data analysis mini project</div><h1>GTA Review Dashboard</h1>',
  paste0('<p>', nrow(reviews), ' unique reviews · ', html_escape(date_range), ' · scores from 0 to 10</p></header>'),
  '<section class="cards">',
  paste0('<div class="card"><strong>', sprintf("%.2f", mean(reviews$score)), '</strong><span>Average score out of 10</span></div>'),
  paste0('<div class="card"><strong>', sprintf("%.1f%%", high_share), '</strong><span>High scores (7–10)</span></div>'),
  paste0('<div class="card"><strong>', sprintf("%.1f%%", low_share), '</strong><span>Low scores (0–4)</span></div>'),
  paste0('<div class="card"><strong>', format(nrow(reviews), big.mark = ","), '</strong><span>Unique reviews analyzed</span></div>'),
  '</section>',
  '<section class="panel"><h2>What stands out</h2><ul class="insights">',
  paste0('<li>The most common score is <b>', mode_row$score, '/10</b> (', format(mode_row$review_count, big.mark = ","), ' reviews; ', sprintf("%.1f%%", most_common_score_share), ').</li>'),
  paste0('<li>', sprintf("%.1f%%", high_share), ' of reviews score 7–10, while ', sprintf("%.1f%%", low_share), ' score 0–4.</li>'),
  paste0('<li>', html_escape(pretty_platform(platform_score_order$platform[1])), ' has the higher platform average at ', sprintf("%.2f", platform_score_order$average_score[1]), '/10; platform averages are ', relative_gap, ' apart.</li>'),
  paste0('<li>', html_escape(peak_day$day), ' has the most reviews (', format(peak_day$review_count, big.mark = ","), '). The largest platform supplies ', sprintf("%.1f%%", platform_count_share), ' of all analyzed reviews.</li>'),
  '</ul></section>',
  '<section class="panel"><h2>Platform comparison</h2><p class="note">Scores summarize the reviews in this file. Platform review counts are uneven, so these figures do not establish why ratings differ.</p>',
  html_table(platform_table), '</section>',
  '<div class="grid">', paste(chart_cards, collapse = "\n"), '</div>',
  '<section class="panel"><h2>Score groups</h2><p class="note">For this project, low = 0–4, mixed = 5–6, and high = 7–10. These are simple reporting bands.</p>',
  html_table(band_table), '</section>',
  '<section class="panel"><h2>Most common content words</h2><p class="note">The percentage is each word’s share of all remaining token occurrences after common filler words were filtered. This is a frequency count, not sentiment analysis.</p>',
  html_table(term_table), '</section>',
  paste0('<section class="panel"><h2>Data and cleaning</h2><p class="note">The source had ', format(input_rows, big.mark = ","), ' rows. ', format(duplicate_id_rows, big.mark = ","), ' repeated review-ID rows were removed, keeping the first record for each ID. ', invalid_rows_removed, ' rows with invalid score or date were excluded. Review length is measured in characters.</p>'),
  '<p class="files"><b>CSV tables:</b> <a href="kpis.csv">KPIs</a><a href="platform_summary.csv">Platform summary</a><a href="score_distribution.csv">Scores</a><a href="daily_summary.csv">Daily summary</a><a href="rating_bands.csv">Score groups</a><a href="top_terms.csv">Content words</a></p></section>',
  '<footer class="footer">Generated by the base R script in this project. No additional R packages required.</footer>',
  '</main></body></html>'
)
writeLines(html_report, file.path(output_dir, "analysis_report.html"), useBytes = TRUE)

cat("Rows in source:", input_rows, "\nDuplicate ID rows removed:", duplicate_id_rows,
    "\nReviews analyzed:", nrow(reviews), "\nAverage score:", round(mean(reviews$score), 2),
    "\nOutputs saved to", output_dir, "\n")
