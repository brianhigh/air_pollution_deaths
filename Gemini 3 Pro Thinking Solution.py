import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import os

# To get these installed in RStudio (via reticulate), run in RStudio Terminal:
#   python -m ensurepip --upgrade
#   python -m pip install pandas
#   python -m pip install matplotlib

# Note: Gemini 3 Pro Thinking generated this python script as part of its 
# "thinking" process before generating the R script. I only edited the path
# to the data file, the filename of the PNG image, and the "Annotations" 
# section by reducing the font size and removing the "%" sign from the 
# annotations, to minimize overlap of the text. The rest was left as-is.

# Create figures directory
os.makedirs('figures', exist_ok=True)

# Load data
df = pd.read_csv('data/IHME GBD Compare - Air Pollution Deaths.csv')

# Filter
df_filtered = df[df['Measure'] == 'Percent of total deaths'].copy()
df_filtered['Percent'] = df_filtered['Value'] * 100

# Filter >= 0.1
df_filtered = df_filtered[df_filtered['Percent'] >= 0.1]

# Sort by Total Percent per Cause
total_per_cause = df_filtered.groupby('Cause of death or injury')['Percent'].sum().sort_values(ascending=True) # Ascending for horizontal bar plot (bottom to top)
# For top to bottom in plot, we usually plot 0 to N, so if we want longest at top, we might need to sort accordingly.
# Matplotlib plots from bottom up usually. So index 0 is bottom.
# If we want longest at top, the sort order should be ascending? Let's check.
# If I have [Small, Medium, Large], Small is at bottom. Large is at top.
# User wants longest (Large) at top. So Ascending sort of the index is correct.

ordered_causes = total_per_cause.index.tolist()
df_filtered['Cause of death or injury'] = pd.Categorical(df_filtered['Cause of death or injury'], categories=ordered_causes, ordered=True)

# Colors
# Pastel palette
# "Ambient particulate matter pollution", "Household air pollution from solid fuels"
risk_factors = df_filtered['Risk factor'].unique()
colors = ['#aec7e8', '#ffbb78', '#98df8a', '#ff9896'] # Pastel-ish colors
# Map risk factors to colors
color_map = {rf: c for rf, c in zip(risk_factors, colors)}

# Plotting
fig, ax = plt.subplots(figsize=(10, 6))

causes = ordered_causes
bottoms = np.zeros(len(causes))

for rf in risk_factors:
    # Extract data for this risk factor, ensuring alignment with 'causes'
    subset = df_filtered[df_filtered['Risk factor'] == rf]
    # Reindex to ensure we have all causes, fill missing with 0
    subset = subset.set_index('Cause of death or injury').reindex(causes).fillna({'Percent': 0}).reset_index()
    
    values = subset['Percent'].values
    
    bars = ax.barh(causes, values, left=bottoms, label=rf, color=color_map[rf], edgecolor='white')
    
    # Annotations
    for bar, val in zip(bars, values):
        if val > 0: # Only annotate if there is a bar
            width = bar.get_width()
            x_pos = bar.get_x() + width / 2
            y_pos = bar.get_y() + bar.get_height() / 2
            ax.text(x_pos, y_pos, f'{val:.1f}', ha='center', va='center', fontsize=7, color='black')

    bottoms += values

ax.set_xlabel('Percent of Total Deaths')
ax.set_ylabel('Cause of Death')
ax.set_title('Global Deaths from Particulate Matter Air Pollution (2023)')
ax.legend(title='Risk Factor')

# Caption
plt.figtext(0.99, 0.01, 'Source: IHME, Global Burden of Disease (2023)', horizontalalignment='right', fontsize=8)

plt.tight_layout()
plt.savefig('figures/global_deaths_particulate_matter_2023_python.png')
print("Plot saved.")

