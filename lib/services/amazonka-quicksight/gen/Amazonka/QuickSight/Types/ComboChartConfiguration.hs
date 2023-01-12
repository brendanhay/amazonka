{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.Types.ComboChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ComboChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.BarsArrangement
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ComboChartFieldWells
import Amazonka.QuickSight.Types.ComboChartSortConfiguration
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.ReferenceLine
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @ComboChartVisual@.
--
-- /See:/ 'newComboChartConfiguration' smart constructor.
data ComboChartConfiguration = ComboChartConfiguration'
  { -- | The options that determine if visual data labels are displayed.
    --
    -- The data label options for a bar in a combo chart.
    barDataLabels :: Prelude.Maybe DataLabelOptions,
    -- | Determines the bar arrangement in a combo chart. The following are valid
    -- values in this structure:
    --
    -- -   @CLUSTERED@: For clustered bar combo charts.
    --
    -- -   @STACKED@: For stacked bar combo charts.
    --
    -- -   @STACKED_PERCENT@: Do not use. If you use this value, the operation
    --     returns a validation error.
    barsArrangement :: Prelude.Maybe BarsArrangement,
    -- | The category axis of a combo chart.
    categoryAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of a combo chart category (group\/color) field well.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of a combo chart\'s color field well.
    colorLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe ComboChartFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The options that determine if visual data labels are displayed.
    --
    -- The data label options for a line in a combo chart.
    lineDataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The label display options (grid line, range, scale, and axis step) of a
    -- combo chart\'s primary y-axis (bar) field well.
    primaryYAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of a combo chart\'s primary y-axis (bar) field well.
    primaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The reference line setup of the visual.
    referenceLines :: Prelude.Maybe [ReferenceLine],
    -- | The label display options (grid line, range, scale, axis step) of a
    -- combo chart\'s secondary y-axis (line) field well.
    secondaryYAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of a combo chart\'s secondary y-axis(line) field well.
    secondaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The sort configuration of a @ComboChartVisual@.
    sortConfiguration :: Prelude.Maybe ComboChartSortConfiguration,
    -- | The legend display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComboChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'barDataLabels', 'comboChartConfiguration_barDataLabels' - The options that determine if visual data labels are displayed.
--
-- The data label options for a bar in a combo chart.
--
-- 'barsArrangement', 'comboChartConfiguration_barsArrangement' - Determines the bar arrangement in a combo chart. The following are valid
-- values in this structure:
--
-- -   @CLUSTERED@: For clustered bar combo charts.
--
-- -   @STACKED@: For stacked bar combo charts.
--
-- -   @STACKED_PERCENT@: Do not use. If you use this value, the operation
--     returns a validation error.
--
-- 'categoryAxis', 'comboChartConfiguration_categoryAxis' - The category axis of a combo chart.
--
-- 'categoryLabelOptions', 'comboChartConfiguration_categoryLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart category (group\/color) field well.
--
-- 'colorLabelOptions', 'comboChartConfiguration_colorLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s color field well.
--
-- 'fieldWells', 'comboChartConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'comboChartConfiguration_legend' - The legend display setup of the visual.
--
-- 'lineDataLabels', 'comboChartConfiguration_lineDataLabels' - The options that determine if visual data labels are displayed.
--
-- The data label options for a line in a combo chart.
--
-- 'primaryYAxisDisplayOptions', 'comboChartConfiguration_primaryYAxisDisplayOptions' - The label display options (grid line, range, scale, and axis step) of a
-- combo chart\'s primary y-axis (bar) field well.
--
-- 'primaryYAxisLabelOptions', 'comboChartConfiguration_primaryYAxisLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s primary y-axis (bar) field well.
--
-- 'referenceLines', 'comboChartConfiguration_referenceLines' - The reference line setup of the visual.
--
-- 'secondaryYAxisDisplayOptions', 'comboChartConfiguration_secondaryYAxisDisplayOptions' - The label display options (grid line, range, scale, axis step) of a
-- combo chart\'s secondary y-axis (line) field well.
--
-- 'secondaryYAxisLabelOptions', 'comboChartConfiguration_secondaryYAxisLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s secondary y-axis(line) field well.
--
-- 'sortConfiguration', 'comboChartConfiguration_sortConfiguration' - The sort configuration of a @ComboChartVisual@.
--
-- 'tooltip', 'comboChartConfiguration_tooltip' - The legend display setup of the visual.
--
-- 'visualPalette', 'comboChartConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
newComboChartConfiguration ::
  ComboChartConfiguration
newComboChartConfiguration =
  ComboChartConfiguration'
    { barDataLabels =
        Prelude.Nothing,
      barsArrangement = Prelude.Nothing,
      categoryAxis = Prelude.Nothing,
      categoryLabelOptions = Prelude.Nothing,
      colorLabelOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      lineDataLabels = Prelude.Nothing,
      primaryYAxisDisplayOptions = Prelude.Nothing,
      primaryYAxisLabelOptions = Prelude.Nothing,
      referenceLines = Prelude.Nothing,
      secondaryYAxisDisplayOptions = Prelude.Nothing,
      secondaryYAxisLabelOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | The options that determine if visual data labels are displayed.
--
-- The data label options for a bar in a combo chart.
comboChartConfiguration_barDataLabels :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe DataLabelOptions)
comboChartConfiguration_barDataLabels = Lens.lens (\ComboChartConfiguration' {barDataLabels} -> barDataLabels) (\s@ComboChartConfiguration' {} a -> s {barDataLabels = a} :: ComboChartConfiguration)

-- | Determines the bar arrangement in a combo chart. The following are valid
-- values in this structure:
--
-- -   @CLUSTERED@: For clustered bar combo charts.
--
-- -   @STACKED@: For stacked bar combo charts.
--
-- -   @STACKED_PERCENT@: Do not use. If you use this value, the operation
--     returns a validation error.
comboChartConfiguration_barsArrangement :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe BarsArrangement)
comboChartConfiguration_barsArrangement = Lens.lens (\ComboChartConfiguration' {barsArrangement} -> barsArrangement) (\s@ComboChartConfiguration' {} a -> s {barsArrangement = a} :: ComboChartConfiguration)

-- | The category axis of a combo chart.
comboChartConfiguration_categoryAxis :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe AxisDisplayOptions)
comboChartConfiguration_categoryAxis = Lens.lens (\ComboChartConfiguration' {categoryAxis} -> categoryAxis) (\s@ComboChartConfiguration' {} a -> s {categoryAxis = a} :: ComboChartConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart category (group\/color) field well.
comboChartConfiguration_categoryLabelOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
comboChartConfiguration_categoryLabelOptions = Lens.lens (\ComboChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@ComboChartConfiguration' {} a -> s {categoryLabelOptions = a} :: ComboChartConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s color field well.
comboChartConfiguration_colorLabelOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
comboChartConfiguration_colorLabelOptions = Lens.lens (\ComboChartConfiguration' {colorLabelOptions} -> colorLabelOptions) (\s@ComboChartConfiguration' {} a -> s {colorLabelOptions = a} :: ComboChartConfiguration)

-- | The field wells of the visual.
comboChartConfiguration_fieldWells :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ComboChartFieldWells)
comboChartConfiguration_fieldWells = Lens.lens (\ComboChartConfiguration' {fieldWells} -> fieldWells) (\s@ComboChartConfiguration' {} a -> s {fieldWells = a} :: ComboChartConfiguration)

-- | The legend display setup of the visual.
comboChartConfiguration_legend :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe LegendOptions)
comboChartConfiguration_legend = Lens.lens (\ComboChartConfiguration' {legend} -> legend) (\s@ComboChartConfiguration' {} a -> s {legend = a} :: ComboChartConfiguration)

-- | The options that determine if visual data labels are displayed.
--
-- The data label options for a line in a combo chart.
comboChartConfiguration_lineDataLabels :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe DataLabelOptions)
comboChartConfiguration_lineDataLabels = Lens.lens (\ComboChartConfiguration' {lineDataLabels} -> lineDataLabels) (\s@ComboChartConfiguration' {} a -> s {lineDataLabels = a} :: ComboChartConfiguration)

-- | The label display options (grid line, range, scale, and axis step) of a
-- combo chart\'s primary y-axis (bar) field well.
comboChartConfiguration_primaryYAxisDisplayOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe AxisDisplayOptions)
comboChartConfiguration_primaryYAxisDisplayOptions = Lens.lens (\ComboChartConfiguration' {primaryYAxisDisplayOptions} -> primaryYAxisDisplayOptions) (\s@ComboChartConfiguration' {} a -> s {primaryYAxisDisplayOptions = a} :: ComboChartConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s primary y-axis (bar) field well.
comboChartConfiguration_primaryYAxisLabelOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
comboChartConfiguration_primaryYAxisLabelOptions = Lens.lens (\ComboChartConfiguration' {primaryYAxisLabelOptions} -> primaryYAxisLabelOptions) (\s@ComboChartConfiguration' {} a -> s {primaryYAxisLabelOptions = a} :: ComboChartConfiguration)

-- | The reference line setup of the visual.
comboChartConfiguration_referenceLines :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe [ReferenceLine])
comboChartConfiguration_referenceLines = Lens.lens (\ComboChartConfiguration' {referenceLines} -> referenceLines) (\s@ComboChartConfiguration' {} a -> s {referenceLines = a} :: ComboChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The label display options (grid line, range, scale, axis step) of a
-- combo chart\'s secondary y-axis (line) field well.
comboChartConfiguration_secondaryYAxisDisplayOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe AxisDisplayOptions)
comboChartConfiguration_secondaryYAxisDisplayOptions = Lens.lens (\ComboChartConfiguration' {secondaryYAxisDisplayOptions} -> secondaryYAxisDisplayOptions) (\s@ComboChartConfiguration' {} a -> s {secondaryYAxisDisplayOptions = a} :: ComboChartConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of a combo chart\'s secondary y-axis(line) field well.
comboChartConfiguration_secondaryYAxisLabelOptions :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
comboChartConfiguration_secondaryYAxisLabelOptions = Lens.lens (\ComboChartConfiguration' {secondaryYAxisLabelOptions} -> secondaryYAxisLabelOptions) (\s@ComboChartConfiguration' {} a -> s {secondaryYAxisLabelOptions = a} :: ComboChartConfiguration)

-- | The sort configuration of a @ComboChartVisual@.
comboChartConfiguration_sortConfiguration :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe ComboChartSortConfiguration)
comboChartConfiguration_sortConfiguration = Lens.lens (\ComboChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@ComboChartConfiguration' {} a -> s {sortConfiguration = a} :: ComboChartConfiguration)

-- | The legend display setup of the visual.
comboChartConfiguration_tooltip :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe TooltipOptions)
comboChartConfiguration_tooltip = Lens.lens (\ComboChartConfiguration' {tooltip} -> tooltip) (\s@ComboChartConfiguration' {} a -> s {tooltip = a} :: ComboChartConfiguration)

-- | The palette (chart color) display setup of the visual.
comboChartConfiguration_visualPalette :: Lens.Lens' ComboChartConfiguration (Prelude.Maybe VisualPalette)
comboChartConfiguration_visualPalette = Lens.lens (\ComboChartConfiguration' {visualPalette} -> visualPalette) (\s@ComboChartConfiguration' {} a -> s {visualPalette = a} :: ComboChartConfiguration)

instance Data.FromJSON ComboChartConfiguration where
  parseJSON =
    Data.withObject
      "ComboChartConfiguration"
      ( \x ->
          ComboChartConfiguration'
            Prelude.<$> (x Data..:? "BarDataLabels")
            Prelude.<*> (x Data..:? "BarsArrangement")
            Prelude.<*> (x Data..:? "CategoryAxis")
            Prelude.<*> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "ColorLabelOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "LineDataLabels")
            Prelude.<*> (x Data..:? "PrimaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "PrimaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "ReferenceLines" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SecondaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "SecondaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable ComboChartConfiguration where
  hashWithSalt _salt ComboChartConfiguration' {..} =
    _salt `Prelude.hashWithSalt` barDataLabels
      `Prelude.hashWithSalt` barsArrangement
      `Prelude.hashWithSalt` categoryAxis
      `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` colorLabelOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` lineDataLabels
      `Prelude.hashWithSalt` primaryYAxisDisplayOptions
      `Prelude.hashWithSalt` primaryYAxisLabelOptions
      `Prelude.hashWithSalt` referenceLines
      `Prelude.hashWithSalt` secondaryYAxisDisplayOptions
      `Prelude.hashWithSalt` secondaryYAxisLabelOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData ComboChartConfiguration where
  rnf ComboChartConfiguration' {..} =
    Prelude.rnf barDataLabels
      `Prelude.seq` Prelude.rnf barsArrangement
      `Prelude.seq` Prelude.rnf categoryAxis
      `Prelude.seq` Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf colorLabelOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf lineDataLabels
      `Prelude.seq` Prelude.rnf primaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf primaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf referenceLines
      `Prelude.seq` Prelude.rnf secondaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf secondaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON ComboChartConfiguration where
  toJSON ComboChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BarDataLabels" Data..=) Prelude.<$> barDataLabels,
            ("BarsArrangement" Data..=)
              Prelude.<$> barsArrangement,
            ("CategoryAxis" Data..=) Prelude.<$> categoryAxis,
            ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("ColorLabelOptions" Data..=)
              Prelude.<$> colorLabelOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("LineDataLabels" Data..=)
              Prelude.<$> lineDataLabels,
            ("PrimaryYAxisDisplayOptions" Data..=)
              Prelude.<$> primaryYAxisDisplayOptions,
            ("PrimaryYAxisLabelOptions" Data..=)
              Prelude.<$> primaryYAxisLabelOptions,
            ("ReferenceLines" Data..=)
              Prelude.<$> referenceLines,
            ("SecondaryYAxisDisplayOptions" Data..=)
              Prelude.<$> secondaryYAxisDisplayOptions,
            ("SecondaryYAxisLabelOptions" Data..=)
              Prelude.<$> secondaryYAxisLabelOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
