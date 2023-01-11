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
-- Module      : Amazonka.QuickSight.Types.BarChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BarChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.BarChartFieldWells
import Amazonka.QuickSight.Types.BarChartOrientation
import Amazonka.QuickSight.Types.BarChartSortConfiguration
import Amazonka.QuickSight.Types.BarsArrangement
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ContributionAnalysisDefault
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.ReferenceLine
import Amazonka.QuickSight.Types.SmallMultiplesOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @BarChartVisual@.
--
-- /See:/ 'newBarChartConfiguration' smart constructor.
data BarChartConfiguration = BarChartConfiguration'
  { -- | Determines the arrangement of the bars. The orientation and arrangement
    -- of bars determine the type of bar that is used in the visual.
    barsArrangement :: Prelude.Maybe BarsArrangement,
    -- | The label display options (grid line, range, scale, axis step) for bar
    -- chart category.
    categoryAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility and sort icon
    -- visibility) for a bar chart.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The label options (label text, label visibility and sort icon
    -- visibility) for a color that is used in a bar chart.
    colorLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The contribution analysis (anomaly configuration) setup of the visual.
    contributionAnalysisDefaults :: Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault),
    -- | The options that determine if visual data labels are displayed.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe BarChartFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The orientation of the bars in a bar chart visual. There are two valid
    -- values in this structure:
    --
    -- -   @HORIZONTAL@: Used for charts that have horizontal bars. Visuals
    --     that use this value are horizontal bar charts, horizontal stacked
    --     bar charts, and horizontal stacked 100% bar charts.
    --
    -- -   @VERTICAL@: Used for charts that have vertical bars. Visuals that
    --     use this value are vertical bar charts, vertical stacked bar charts,
    --     and vertical stacked 100% bar charts.
    orientation :: Prelude.Maybe BarChartOrientation,
    -- | The reference line setup of the visual.
    referenceLines :: Prelude.Maybe [ReferenceLine],
    -- | The small multiples setup for the visual.
    smallMultiplesOptions :: Prelude.Maybe SmallMultiplesOptions,
    -- | The sort configuration of a @BarChartVisual@.
    sortConfiguration :: Prelude.Maybe BarChartSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The label display options (grid line, range, scale, axis step) for a bar
    -- chart value.
    valueAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility and sort icon
    -- visibility) for a bar chart value.
    valueLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BarChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'barsArrangement', 'barChartConfiguration_barsArrangement' - Determines the arrangement of the bars. The orientation and arrangement
-- of bars determine the type of bar that is used in the visual.
--
-- 'categoryAxis', 'barChartConfiguration_categoryAxis' - The label display options (grid line, range, scale, axis step) for bar
-- chart category.
--
-- 'categoryLabelOptions', 'barChartConfiguration_categoryLabelOptions' - The label options (label text, label visibility and sort icon
-- visibility) for a bar chart.
--
-- 'colorLabelOptions', 'barChartConfiguration_colorLabelOptions' - The label options (label text, label visibility and sort icon
-- visibility) for a color that is used in a bar chart.
--
-- 'contributionAnalysisDefaults', 'barChartConfiguration_contributionAnalysisDefaults' - The contribution analysis (anomaly configuration) setup of the visual.
--
-- 'dataLabels', 'barChartConfiguration_dataLabels' - The options that determine if visual data labels are displayed.
--
-- 'fieldWells', 'barChartConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'barChartConfiguration_legend' - The legend display setup of the visual.
--
-- 'orientation', 'barChartConfiguration_orientation' - The orientation of the bars in a bar chart visual. There are two valid
-- values in this structure:
--
-- -   @HORIZONTAL@: Used for charts that have horizontal bars. Visuals
--     that use this value are horizontal bar charts, horizontal stacked
--     bar charts, and horizontal stacked 100% bar charts.
--
-- -   @VERTICAL@: Used for charts that have vertical bars. Visuals that
--     use this value are vertical bar charts, vertical stacked bar charts,
--     and vertical stacked 100% bar charts.
--
-- 'referenceLines', 'barChartConfiguration_referenceLines' - The reference line setup of the visual.
--
-- 'smallMultiplesOptions', 'barChartConfiguration_smallMultiplesOptions' - The small multiples setup for the visual.
--
-- 'sortConfiguration', 'barChartConfiguration_sortConfiguration' - The sort configuration of a @BarChartVisual@.
--
-- 'tooltip', 'barChartConfiguration_tooltip' - The tooltip display setup of the visual.
--
-- 'valueAxis', 'barChartConfiguration_valueAxis' - The label display options (grid line, range, scale, axis step) for a bar
-- chart value.
--
-- 'valueLabelOptions', 'barChartConfiguration_valueLabelOptions' - The label options (label text, label visibility and sort icon
-- visibility) for a bar chart value.
--
-- 'visualPalette', 'barChartConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
newBarChartConfiguration ::
  BarChartConfiguration
newBarChartConfiguration =
  BarChartConfiguration'
    { barsArrangement =
        Prelude.Nothing,
      categoryAxis = Prelude.Nothing,
      categoryLabelOptions = Prelude.Nothing,
      colorLabelOptions = Prelude.Nothing,
      contributionAnalysisDefaults = Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      orientation = Prelude.Nothing,
      referenceLines = Prelude.Nothing,
      smallMultiplesOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      valueAxis = Prelude.Nothing,
      valueLabelOptions = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | Determines the arrangement of the bars. The orientation and arrangement
-- of bars determine the type of bar that is used in the visual.
barChartConfiguration_barsArrangement :: Lens.Lens' BarChartConfiguration (Prelude.Maybe BarsArrangement)
barChartConfiguration_barsArrangement = Lens.lens (\BarChartConfiguration' {barsArrangement} -> barsArrangement) (\s@BarChartConfiguration' {} a -> s {barsArrangement = a} :: BarChartConfiguration)

-- | The label display options (grid line, range, scale, axis step) for bar
-- chart category.
barChartConfiguration_categoryAxis :: Lens.Lens' BarChartConfiguration (Prelude.Maybe AxisDisplayOptions)
barChartConfiguration_categoryAxis = Lens.lens (\BarChartConfiguration' {categoryAxis} -> categoryAxis) (\s@BarChartConfiguration' {} a -> s {categoryAxis = a} :: BarChartConfiguration)

-- | The label options (label text, label visibility and sort icon
-- visibility) for a bar chart.
barChartConfiguration_categoryLabelOptions :: Lens.Lens' BarChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
barChartConfiguration_categoryLabelOptions = Lens.lens (\BarChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@BarChartConfiguration' {} a -> s {categoryLabelOptions = a} :: BarChartConfiguration)

-- | The label options (label text, label visibility and sort icon
-- visibility) for a color that is used in a bar chart.
barChartConfiguration_colorLabelOptions :: Lens.Lens' BarChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
barChartConfiguration_colorLabelOptions = Lens.lens (\BarChartConfiguration' {colorLabelOptions} -> colorLabelOptions) (\s@BarChartConfiguration' {} a -> s {colorLabelOptions = a} :: BarChartConfiguration)

-- | The contribution analysis (anomaly configuration) setup of the visual.
barChartConfiguration_contributionAnalysisDefaults :: Lens.Lens' BarChartConfiguration (Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault))
barChartConfiguration_contributionAnalysisDefaults = Lens.lens (\BarChartConfiguration' {contributionAnalysisDefaults} -> contributionAnalysisDefaults) (\s@BarChartConfiguration' {} a -> s {contributionAnalysisDefaults = a} :: BarChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The options that determine if visual data labels are displayed.
barChartConfiguration_dataLabels :: Lens.Lens' BarChartConfiguration (Prelude.Maybe DataLabelOptions)
barChartConfiguration_dataLabels = Lens.lens (\BarChartConfiguration' {dataLabels} -> dataLabels) (\s@BarChartConfiguration' {} a -> s {dataLabels = a} :: BarChartConfiguration)

-- | The field wells of the visual.
barChartConfiguration_fieldWells :: Lens.Lens' BarChartConfiguration (Prelude.Maybe BarChartFieldWells)
barChartConfiguration_fieldWells = Lens.lens (\BarChartConfiguration' {fieldWells} -> fieldWells) (\s@BarChartConfiguration' {} a -> s {fieldWells = a} :: BarChartConfiguration)

-- | The legend display setup of the visual.
barChartConfiguration_legend :: Lens.Lens' BarChartConfiguration (Prelude.Maybe LegendOptions)
barChartConfiguration_legend = Lens.lens (\BarChartConfiguration' {legend} -> legend) (\s@BarChartConfiguration' {} a -> s {legend = a} :: BarChartConfiguration)

-- | The orientation of the bars in a bar chart visual. There are two valid
-- values in this structure:
--
-- -   @HORIZONTAL@: Used for charts that have horizontal bars. Visuals
--     that use this value are horizontal bar charts, horizontal stacked
--     bar charts, and horizontal stacked 100% bar charts.
--
-- -   @VERTICAL@: Used for charts that have vertical bars. Visuals that
--     use this value are vertical bar charts, vertical stacked bar charts,
--     and vertical stacked 100% bar charts.
barChartConfiguration_orientation :: Lens.Lens' BarChartConfiguration (Prelude.Maybe BarChartOrientation)
barChartConfiguration_orientation = Lens.lens (\BarChartConfiguration' {orientation} -> orientation) (\s@BarChartConfiguration' {} a -> s {orientation = a} :: BarChartConfiguration)

-- | The reference line setup of the visual.
barChartConfiguration_referenceLines :: Lens.Lens' BarChartConfiguration (Prelude.Maybe [ReferenceLine])
barChartConfiguration_referenceLines = Lens.lens (\BarChartConfiguration' {referenceLines} -> referenceLines) (\s@BarChartConfiguration' {} a -> s {referenceLines = a} :: BarChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The small multiples setup for the visual.
barChartConfiguration_smallMultiplesOptions :: Lens.Lens' BarChartConfiguration (Prelude.Maybe SmallMultiplesOptions)
barChartConfiguration_smallMultiplesOptions = Lens.lens (\BarChartConfiguration' {smallMultiplesOptions} -> smallMultiplesOptions) (\s@BarChartConfiguration' {} a -> s {smallMultiplesOptions = a} :: BarChartConfiguration)

-- | The sort configuration of a @BarChartVisual@.
barChartConfiguration_sortConfiguration :: Lens.Lens' BarChartConfiguration (Prelude.Maybe BarChartSortConfiguration)
barChartConfiguration_sortConfiguration = Lens.lens (\BarChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@BarChartConfiguration' {} a -> s {sortConfiguration = a} :: BarChartConfiguration)

-- | The tooltip display setup of the visual.
barChartConfiguration_tooltip :: Lens.Lens' BarChartConfiguration (Prelude.Maybe TooltipOptions)
barChartConfiguration_tooltip = Lens.lens (\BarChartConfiguration' {tooltip} -> tooltip) (\s@BarChartConfiguration' {} a -> s {tooltip = a} :: BarChartConfiguration)

-- | The label display options (grid line, range, scale, axis step) for a bar
-- chart value.
barChartConfiguration_valueAxis :: Lens.Lens' BarChartConfiguration (Prelude.Maybe AxisDisplayOptions)
barChartConfiguration_valueAxis = Lens.lens (\BarChartConfiguration' {valueAxis} -> valueAxis) (\s@BarChartConfiguration' {} a -> s {valueAxis = a} :: BarChartConfiguration)

-- | The label options (label text, label visibility and sort icon
-- visibility) for a bar chart value.
barChartConfiguration_valueLabelOptions :: Lens.Lens' BarChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
barChartConfiguration_valueLabelOptions = Lens.lens (\BarChartConfiguration' {valueLabelOptions} -> valueLabelOptions) (\s@BarChartConfiguration' {} a -> s {valueLabelOptions = a} :: BarChartConfiguration)

-- | The palette (chart color) display setup of the visual.
barChartConfiguration_visualPalette :: Lens.Lens' BarChartConfiguration (Prelude.Maybe VisualPalette)
barChartConfiguration_visualPalette = Lens.lens (\BarChartConfiguration' {visualPalette} -> visualPalette) (\s@BarChartConfiguration' {} a -> s {visualPalette = a} :: BarChartConfiguration)

instance Data.FromJSON BarChartConfiguration where
  parseJSON =
    Data.withObject
      "BarChartConfiguration"
      ( \x ->
          BarChartConfiguration'
            Prelude.<$> (x Data..:? "BarsArrangement")
            Prelude.<*> (x Data..:? "CategoryAxis")
            Prelude.<*> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "ColorLabelOptions")
            Prelude.<*> (x Data..:? "ContributionAnalysisDefaults")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "Orientation")
            Prelude.<*> (x Data..:? "ReferenceLines" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiplesOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "ValueAxis")
            Prelude.<*> (x Data..:? "ValueLabelOptions")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable BarChartConfiguration where
  hashWithSalt _salt BarChartConfiguration' {..} =
    _salt `Prelude.hashWithSalt` barsArrangement
      `Prelude.hashWithSalt` categoryAxis
      `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` colorLabelOptions
      `Prelude.hashWithSalt` contributionAnalysisDefaults
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` referenceLines
      `Prelude.hashWithSalt` smallMultiplesOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` valueAxis
      `Prelude.hashWithSalt` valueLabelOptions
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData BarChartConfiguration where
  rnf BarChartConfiguration' {..} =
    Prelude.rnf barsArrangement
      `Prelude.seq` Prelude.rnf categoryAxis
      `Prelude.seq` Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf colorLabelOptions
      `Prelude.seq` Prelude.rnf contributionAnalysisDefaults
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf referenceLines
      `Prelude.seq` Prelude.rnf smallMultiplesOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf valueAxis
      `Prelude.seq` Prelude.rnf valueLabelOptions
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON BarChartConfiguration where
  toJSON BarChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BarsArrangement" Data..=)
              Prelude.<$> barsArrangement,
            ("CategoryAxis" Data..=) Prelude.<$> categoryAxis,
            ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("ColorLabelOptions" Data..=)
              Prelude.<$> colorLabelOptions,
            ("ContributionAnalysisDefaults" Data..=)
              Prelude.<$> contributionAnalysisDefaults,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("Orientation" Data..=) Prelude.<$> orientation,
            ("ReferenceLines" Data..=)
              Prelude.<$> referenceLines,
            ("SmallMultiplesOptions" Data..=)
              Prelude.<$> smallMultiplesOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("ValueAxis" Data..=) Prelude.<$> valueAxis,
            ("ValueLabelOptions" Data..=)
              Prelude.<$> valueLabelOptions,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
