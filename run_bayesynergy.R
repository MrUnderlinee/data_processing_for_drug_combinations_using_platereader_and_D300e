# --- run_bayesynergy.R ---

args <- commandArgs(trailingOnly=TRUE)
INPUT_FILE   <- args[1]  
ANCHOR_DRUG  <- args[2]  
PARTNER_DRUG <- args[3]  
PDF_FILE_OUTPUT  <- args[4]  

# Load required libraries
suppressPackageStartupMessages(library(bayesynergy))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(scales)) # <-- NEW: Required for 'squish'

df <- read.csv(INPUT_FILE)

# =====================================================================
# 1. GENERATE THE SEABORN-STYLE HEATMAP IN R
# =====================================================================
agg_df <- aggregate(Viability_pct ~ Conc1 + Conc2, data = df, FUN = mean)

heatmap_plot <- ggplot(agg_df, aes(x = factor(Conc2), y = factor(Conc1), fill = Viability_pct)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = sprintf("%.1f", Viability_pct)), 
            color = ifelse(agg_df$Viability_pct < 40, "white", "black"), size = 5) +
  
  # --- NEW: Lock limits to 0-100 and squish out-of-bounds values! ---
  scale_fill_viridis_c(option = "plasma", name = "Viability [%]", 
                       limits = c(0, 100), oob = squish) +
  
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = paste("Viability Heatmap:", ANCHOR_DRUG, "vs", PARTNER_DRUG),
       x = paste(PARTNER_DRUG, "[µM]"),
       y = paste(ANCHOR_DRUG, "[µM]")) +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 20))

# =====================================================================
# 2. RUN BAYESYNERGY MODEL
# =====================================================================
y <- df$Viability_pct / 100
x <- as.matrix(df[, c("Conc1", "Conc2")])

cat(paste("\n[R] Running Bayesynergy for:", ANCHOR_DRUG, "+", PARTNER_DRUG, "...\n"))
fit <- bayesynergy(
  y = y, 
  x = x,
  drug_names = c(ANCHOR_DRUG, PARTNER_DRUG),
  method = "sampling"
)

# =====================================================================
# 3. EXPORT PDF WITH CUSTOM NAME
# =====================================================================
if (PDF_FILE_OUTPUT == "DEFAULT" || is.na(PDF_FILE_OUTPUT) || PDF_FILE_OUTPUT == "") {
  file_base <- paste0("Report_", ANCHOR_DRUG, "_vs_", PARTNER_DRUG)
} else {
  file_base <- paste0(PDF_FILE_OUTPUT, "_Report_", ANCHOR_DRUG, "_vs_", PARTNER_DRUG)
}

output_pdf <- paste0(file_base, ".pdf")

pdf(output_pdf, width=12, height=9)
print(heatmap_plot)  
plot(fit)            
dev.off()

cat(paste("[R] PDF saved as:", output_pdf, "\n"))