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
-- Module      : Amazonka.QuickSight.Types.PieChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.PieChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ContributionAnalysisDefault
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.DonutOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.PieChartFieldWells
import Amazonka.QuickSight.Types.PieChartSortConfiguration
import Amazonka.QuickSight.Types.SmallMultiplesOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a pie chart.
--
-- /See:/ 'newPieChartConfiguration' smart constructor.
data PieChartConfiguration = PieChartConfiguration'
  { -- | The label options of the group\/color that is displayed in a pie chart.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The contribution analysis (anomaly configuration) setup of the visual.
    contributionAnalysisDefaults :: Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault),
    -- | The options that determine if visual data labels are displayed.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The options that determine the shape of the chart. This option
    -- determines whether the chart is a pie chart or a donut chart.
    donutOptions :: Prelude.Maybe DonutOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe PieChartFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The small multiples setup for the visual.
    smallMultiplesOptions :: Prelude.Maybe SmallMultiplesOptions,
    -- | The sort configuration of a pie chart.
    sortConfiguration :: Prelude.Maybe PieChartSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The label options for the value that is displayed in a pie chart.
    valueLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PieChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryLabelOptions', 'pieChartConfiguration_categoryLabelOptions' - The label options of the group\/color that is displayed in a pie chart.
--
-- 'contributionAnalysisDefaults', 'pieChartConfiguration_contributionAnalysisDefaults' - The contribution analysis (anomaly configuration) setup of the visual.
--
-- 'dataLabels', 'pieChartConfiguration_dataLabels' - The options that determine if visual data labels are displayed.
--
-- 'donutOptions', 'pieChartConfiguration_donutOptions' - The options that determine the shape of the chart. This option
-- determines whether the chart is a pie chart or a donut chart.
--
-- 'fieldWells', 'pieChartConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'pieChartConfiguration_legend' - The legend display setup of the visual.
--
-- 'smallMultiplesOptions', 'pieChartConfiguration_smallMultiplesOptions' - The small multiples setup for the visual.
--
-- 'sortConfiguration', 'pieChartConfiguration_sortConfiguration' - The sort configuration of a pie chart.
--
-- 'tooltip', 'pieChartConfiguration_tooltip' - The tooltip display setup of the visual.
--
-- 'valueLabelOptions', 'pieChartConfiguration_valueLabelOptions' - The label options for the value that is displayed in a pie chart.
--
-- 'visualPalette', 'pieChartConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
newPieChartConfiguration ::
  PieChartConfiguration
newPieChartConfiguration =
  PieChartConfiguration'
    { categoryLabelOptions =
        Prelude.Nothing,
      contributionAnalysisDefaults = Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      donutOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      smallMultiplesOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      valueLabelOptions = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | The label options of the group\/color that is displayed in a pie chart.
pieChartConfiguration_categoryLabelOptions :: Lens.Lens' PieChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
pieChartConfiguration_categoryLabelOptions = Lens.lens (\PieChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@PieChartConfiguration' {} a -> s {categoryLabelOptions = a} :: PieChartConfiguration)

-- | The contribution analysis (anomaly configuration) setup of the visual.
pieChartConfiguration_contributionAnalysisDefaults :: Lens.Lens' PieChartConfiguration (Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault))
pieChartConfiguration_contributionAnalysisDefaults = Lens.lens (\PieChartConfiguration' {contributionAnalysisDefaults} -> contributionAnalysisDefaults) (\s@PieChartConfiguration' {} a -> s {contributionAnalysisDefaults = a} :: PieChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The options that determine if visual data labels are displayed.
pieChartConfiguration_dataLabels :: Lens.Lens' PieChartConfiguration (Prelude.Maybe DataLabelOptions)
pieChartConfiguration_dataLabels = Lens.lens (\PieChartConfiguration' {dataLabels} -> dataLabels) (\s@PieChartConfiguration' {} a -> s {dataLabels = a} :: PieChartConfiguration)

-- | The options that determine the shape of the chart. This option
-- determines whether the chart is a pie chart or a donut chart.
pieChartConfiguration_donutOptions :: Lens.Lens' PieChartConfiguration (Prelude.Maybe DonutOptions)
pieChartConfiguration_donutOptions = Lens.lens (\PieChartConfiguration' {donutOptions} -> donutOptions) (\s@PieChartConfiguration' {} a -> s {donutOptions = a} :: PieChartConfiguration)

-- | The field wells of the visual.
pieChartConfiguration_fieldWells :: Lens.Lens' PieChartConfiguration (Prelude.Maybe PieChartFieldWells)
pieChartConfiguration_fieldWells = Lens.lens (\PieChartConfiguration' {fieldWells} -> fieldWells) (\s@PieChartConfiguration' {} a -> s {fieldWells = a} :: PieChartConfiguration)

-- | The legend display setup of the visual.
pieChartConfiguration_legend :: Lens.Lens' PieChartConfiguration (Prelude.Maybe LegendOptions)
pieChartConfiguration_legend = Lens.lens (\PieChartConfiguration' {legend} -> legend) (\s@PieChartConfiguration' {} a -> s {legend = a} :: PieChartConfiguration)

-- | The small multiples setup for the visual.
pieChartConfiguration_smallMultiplesOptions :: Lens.Lens' PieChartConfiguration (Prelude.Maybe SmallMultiplesOptions)
pieChartConfiguration_smallMultiplesOptions = Lens.lens (\PieChartConfiguration' {smallMultiplesOptions} -> smallMultiplesOptions) (\s@PieChartConfiguration' {} a -> s {smallMultiplesOptions = a} :: PieChartConfiguration)

-- | The sort configuration of a pie chart.
pieChartConfiguration_sortConfiguration :: Lens.Lens' PieChartConfiguration (Prelude.Maybe PieChartSortConfiguration)
pieChartConfiguration_sortConfiguration = Lens.lens (\PieChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@PieChartConfiguration' {} a -> s {sortConfiguration = a} :: PieChartConfiguration)

-- | The tooltip display setup of the visual.
pieChartConfiguration_tooltip :: Lens.Lens' PieChartConfiguration (Prelude.Maybe TooltipOptions)
pieChartConfiguration_tooltip = Lens.lens (\PieChartConfiguration' {tooltip} -> tooltip) (\s@PieChartConfiguration' {} a -> s {tooltip = a} :: PieChartConfiguration)

-- | The label options for the value that is displayed in a pie chart.
pieChartConfiguration_valueLabelOptions :: Lens.Lens' PieChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
pieChartConfiguration_valueLabelOptions = Lens.lens (\PieChartConfiguration' {valueLabelOptions} -> valueLabelOptions) (\s@PieChartConfiguration' {} a -> s {valueLabelOptions = a} :: PieChartConfiguration)

-- | The palette (chart color) display setup of the visual.
pieChartConfiguration_visualPalette :: Lens.Lens' PieChartConfiguration (Prelude.Maybe VisualPalette)
pieChartConfiguration_visualPalette = Lens.lens (\PieChartConfiguration' {visualPalette} -> visualPalette) (\s@PieChartConfiguration' {} a -> s {visualPalette = a} :: PieChartConfiguration)

instance Data.FromJSON PieChartConfiguration where
  parseJSON =
    Data.withObject
      "PieChartConfiguration"
      ( \x ->
          PieChartConfiguration'
            Prelude.<$> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "ContributionAnalysisDefaults")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "DonutOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "SmallMultiplesOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "ValueLabelOptions")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable PieChartConfiguration where
  hashWithSalt _salt PieChartConfiguration' {..} =
    _salt `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` contributionAnalysisDefaults
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` donutOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` smallMultiplesOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` valueLabelOptions
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData PieChartConfiguration where
  rnf PieChartConfiguration' {..} =
    Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf contributionAnalysisDefaults
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf donutOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf smallMultiplesOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf valueLabelOptions
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON PieChartConfiguration where
  toJSON PieChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("ContributionAnalysisDefaults" Data..=)
              Prelude.<$> contributionAnalysisDefaults,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("DonutOptions" Data..=) Prelude.<$> donutOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("SmallMultiplesOptions" Data..=)
              Prelude.<$> smallMultiplesOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("ValueLabelOptions" Data..=)
              Prelude.<$> valueLabelOptions,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
