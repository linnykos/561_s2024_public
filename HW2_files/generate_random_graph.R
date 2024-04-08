set.seed(0)
res <- generate_random_graph(n = 20,
                                 clique_fraction = 0.5,
                                 density_low = 0.3)
adj_mat <- res$adj_mat

# Convert the matrix to a tibble (a type of data frame) for easier manipulation
colnames(adj_mat) <- paste0("node:", 1:nrow(adj_mat))
mat_df <- as_tibble(adj_mat)

# Add row numbers as a new column, since gather() will melt all existing columns
mat_df <- mat_df %>% mutate(Row = row_number())

# Use gather() to convert from wide to long format
mat_long <- mat_df %>%
  gather(key = "Column", value = "Value", -Row) %>%
  mutate(Column = as.numeric(gsub("node:", "", Column))) %>%
  rename(X = Row, Y = Column)

# Plot using ggplot
ggplot(mat_long, aes(x = X, y = Y, fill = factor(Value))) +
  geom_tile(color = "white") + # Use color to distinguish tiles
  scale_fill_manual(values=c("0"="palegoldenrod", "1"="red")) + # Set colors
  labs(fill = "Value") + # Label for the legend
  xlab("X axis") + ylab("Y axis") + # Axis labels
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Improve axis text readability

###############

adj_mat <- res$adj_mat[res$rev_order, res$rev_order]
# n <- nrow(adj_mat)
# rownames(adj_mat) <- paste0("node:",1:n)
# colnames(adj_mat) <- paste0("node:",1:n)

# Convert the matrix to a tibble (a type of data frame) for easier manipulation
colnames(adj_mat) <- paste0("node:", 1:nrow(adj_mat))
mat_df <- as_tibble(adj_mat)

# Add row numbers as a new column, since gather() will melt all existing columns
mat_df <- mat_df %>% mutate(Row = row_number())

# Use gather() to convert from wide to long format
mat_long <- mat_df %>%
  gather(key = "Column", value = "Value", -Row) %>%
  mutate(Column = as.numeric(gsub("node:", "", Column))) %>%
  rename(X = Row, Y = Column)

# Plot using ggplot
ggplot(mat_long, aes(x = X, y = Y, fill = factor(Value))) +
  geom_tile(color = "white") + # Use color to distinguish tiles
  scale_fill_manual(values=c("0"="palegoldenrod", "1"="red")) + # Set colors
  labs(fill = "Value") + # Label for the legend
  xlab("X axis") + ylab("Y axis") + # Axis labels
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) # Improve axis text readability


#################

# Dummy data
x <- LETTERS[1:20]
y <- paste0("var", seq(1,20))
data <- expand.grid(X=x, Y=y)
data$Z <- runif(400, 0, 5)

# Heatmap 
ggplot(data, aes(X, Y, fill= Z)) + 
  geom_tile()