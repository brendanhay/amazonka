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
-- Module      : Amazonka.QuickSight.Types.LineChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ContributionAnalysisDefault
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.ForecastConfiguration
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.LineChartDefaultSeriesSettings
import Amazonka.QuickSight.Types.LineChartFieldWells
import Amazonka.QuickSight.Types.LineChartSortConfiguration
import Amazonka.QuickSight.Types.LineChartType
import Amazonka.QuickSight.Types.LineSeriesAxisDisplayOptions
import Amazonka.QuickSight.Types.ReferenceLine
import Amazonka.QuickSight.Types.SeriesItem
import Amazonka.QuickSight.Types.SmallMultiplesOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a line chart.
--
-- /See:/ 'newLineChartConfiguration' smart constructor.
data LineChartConfiguration = LineChartConfiguration'
  { -- | The default configuration of a line chart\'s contribution analysis.
    contributionAnalysisDefaults :: Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault),
    -- | The data label configuration of a line chart.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The options that determine the default presentation of all line series
    -- in @LineChartVisual@.
    defaultSeriesSettings :: Prelude.Maybe LineChartDefaultSeriesSettings,
    -- | The field well configuration of a line chart.
    fieldWells :: Prelude.Maybe LineChartFieldWells,
    -- | The forecast configuration of a line chart.
    forecastConfigurations :: Prelude.Maybe [ForecastConfiguration],
    -- | The legend configuration of a line chart.
    legend :: Prelude.Maybe LegendOptions,
    -- | The series axis configuration of a line chart.
    primaryYAxisDisplayOptions :: Prelude.Maybe LineSeriesAxisDisplayOptions,
    -- | The options that determine the presentation of the y-axis label.
    primaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The reference lines configuration of a line chart.
    referenceLines :: Prelude.Maybe [ReferenceLine],
    -- | The series axis configuration of a line chart.
    secondaryYAxisDisplayOptions :: Prelude.Maybe LineSeriesAxisDisplayOptions,
    -- | The options that determine the presentation of the secondary y-axis
    -- label.
    secondaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The series item configuration of a line chart.
    series :: Prelude.Maybe [SeriesItem],
    -- | The small multiples setup for the visual.
    smallMultiplesOptions :: Prelude.Maybe SmallMultiplesOptions,
    -- | The sort configuration of a line chart.
    sortConfiguration :: Prelude.Maybe LineChartSortConfiguration,
    -- | The tooltip configuration of a line chart.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | Determines the type of the line chart.
    type' :: Prelude.Maybe LineChartType,
    -- | The visual palette configuration of a line chart.
    visualPalette :: Prelude.Maybe VisualPalette,
    -- | The options that determine the presentation of the x-axis.
    xAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The options that determine the presentation of the x-axis label.
    xAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contributionAnalysisDefaults', 'lineChartConfiguration_contributionAnalysisDefaults' - The default configuration of a line chart\'s contribution analysis.
--
-- 'dataLabels', 'lineChartConfiguration_dataLabels' - The data label configuration of a line chart.
--
-- 'defaultSeriesSettings', 'lineChartConfiguration_defaultSeriesSettings' - The options that determine the default presentation of all line series
-- in @LineChartVisual@.
--
-- 'fieldWells', 'lineChartConfiguration_fieldWells' - The field well configuration of a line chart.
--
-- 'forecastConfigurations', 'lineChartConfiguration_forecastConfigurations' - The forecast configuration of a line chart.
--
-- 'legend', 'lineChartConfiguration_legend' - The legend configuration of a line chart.
--
-- 'primaryYAxisDisplayOptions', 'lineChartConfiguration_primaryYAxisDisplayOptions' - The series axis configuration of a line chart.
--
-- 'primaryYAxisLabelOptions', 'lineChartConfiguration_primaryYAxisLabelOptions' - The options that determine the presentation of the y-axis label.
--
-- 'referenceLines', 'lineChartConfiguration_referenceLines' - The reference lines configuration of a line chart.
--
-- 'secondaryYAxisDisplayOptions', 'lineChartConfiguration_secondaryYAxisDisplayOptions' - The series axis configuration of a line chart.
--
-- 'secondaryYAxisLabelOptions', 'lineChartConfiguration_secondaryYAxisLabelOptions' - The options that determine the presentation of the secondary y-axis
-- label.
--
-- 'series', 'lineChartConfiguration_series' - The series item configuration of a line chart.
--
-- 'smallMultiplesOptions', 'lineChartConfiguration_smallMultiplesOptions' - The small multiples setup for the visual.
--
-- 'sortConfiguration', 'lineChartConfiguration_sortConfiguration' - The sort configuration of a line chart.
--
-- 'tooltip', 'lineChartConfiguration_tooltip' - The tooltip configuration of a line chart.
--
-- 'type'', 'lineChartConfiguration_type' - Determines the type of the line chart.
--
-- 'visualPalette', 'lineChartConfiguration_visualPalette' - The visual palette configuration of a line chart.
--
-- 'xAxisDisplayOptions', 'lineChartConfiguration_xAxisDisplayOptions' - The options that determine the presentation of the x-axis.
--
-- 'xAxisLabelOptions', 'lineChartConfiguration_xAxisLabelOptions' - The options that determine the presentation of the x-axis label.
newLineChartConfiguration ::
  LineChartConfiguration
newLineChartConfiguration =
  LineChartConfiguration'
    { contributionAnalysisDefaults =
        Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      defaultSeriesSettings = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      forecastConfigurations = Prelude.Nothing,
      legend = Prelude.Nothing,
      primaryYAxisDisplayOptions = Prelude.Nothing,
      primaryYAxisLabelOptions = Prelude.Nothing,
      referenceLines = Prelude.Nothing,
      secondaryYAxisDisplayOptions = Prelude.Nothing,
      secondaryYAxisLabelOptions = Prelude.Nothing,
      series = Prelude.Nothing,
      smallMultiplesOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      type' = Prelude.Nothing,
      visualPalette = Prelude.Nothing,
      xAxisDisplayOptions = Prelude.Nothing,
      xAxisLabelOptions = Prelude.Nothing
    }

-- | The default configuration of a line chart\'s contribution analysis.
lineChartConfiguration_contributionAnalysisDefaults :: Lens.Lens' LineChartConfiguration (Prelude.Maybe (Prelude.NonEmpty ContributionAnalysisDefault))
lineChartConfiguration_contributionAnalysisDefaults = Lens.lens (\LineChartConfiguration' {contributionAnalysisDefaults} -> contributionAnalysisDefaults) (\s@LineChartConfiguration' {} a -> s {contributionAnalysisDefaults = a} :: LineChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The data label configuration of a line chart.
lineChartConfiguration_dataLabels :: Lens.Lens' LineChartConfiguration (Prelude.Maybe DataLabelOptions)
lineChartConfiguration_dataLabels = Lens.lens (\LineChartConfiguration' {dataLabels} -> dataLabels) (\s@LineChartConfiguration' {} a -> s {dataLabels = a} :: LineChartConfiguration)

-- | The options that determine the default presentation of all line series
-- in @LineChartVisual@.
lineChartConfiguration_defaultSeriesSettings :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineChartDefaultSeriesSettings)
lineChartConfiguration_defaultSeriesSettings = Lens.lens (\LineChartConfiguration' {defaultSeriesSettings} -> defaultSeriesSettings) (\s@LineChartConfiguration' {} a -> s {defaultSeriesSettings = a} :: LineChartConfiguration)

-- | The field well configuration of a line chart.
lineChartConfiguration_fieldWells :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineChartFieldWells)
lineChartConfiguration_fieldWells = Lens.lens (\LineChartConfiguration' {fieldWells} -> fieldWells) (\s@LineChartConfiguration' {} a -> s {fieldWells = a} :: LineChartConfiguration)

-- | The forecast configuration of a line chart.
lineChartConfiguration_forecastConfigurations :: Lens.Lens' LineChartConfiguration (Prelude.Maybe [ForecastConfiguration])
lineChartConfiguration_forecastConfigurations = Lens.lens (\LineChartConfiguration' {forecastConfigurations} -> forecastConfigurations) (\s@LineChartConfiguration' {} a -> s {forecastConfigurations = a} :: LineChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The legend configuration of a line chart.
lineChartConfiguration_legend :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LegendOptions)
lineChartConfiguration_legend = Lens.lens (\LineChartConfiguration' {legend} -> legend) (\s@LineChartConfiguration' {} a -> s {legend = a} :: LineChartConfiguration)

-- | The series axis configuration of a line chart.
lineChartConfiguration_primaryYAxisDisplayOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineSeriesAxisDisplayOptions)
lineChartConfiguration_primaryYAxisDisplayOptions = Lens.lens (\LineChartConfiguration' {primaryYAxisDisplayOptions} -> primaryYAxisDisplayOptions) (\s@LineChartConfiguration' {} a -> s {primaryYAxisDisplayOptions = a} :: LineChartConfiguration)

-- | The options that determine the presentation of the y-axis label.
lineChartConfiguration_primaryYAxisLabelOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
lineChartConfiguration_primaryYAxisLabelOptions = Lens.lens (\LineChartConfiguration' {primaryYAxisLabelOptions} -> primaryYAxisLabelOptions) (\s@LineChartConfiguration' {} a -> s {primaryYAxisLabelOptions = a} :: LineChartConfiguration)

-- | The reference lines configuration of a line chart.
lineChartConfiguration_referenceLines :: Lens.Lens' LineChartConfiguration (Prelude.Maybe [ReferenceLine])
lineChartConfiguration_referenceLines = Lens.lens (\LineChartConfiguration' {referenceLines} -> referenceLines) (\s@LineChartConfiguration' {} a -> s {referenceLines = a} :: LineChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The series axis configuration of a line chart.
lineChartConfiguration_secondaryYAxisDisplayOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineSeriesAxisDisplayOptions)
lineChartConfiguration_secondaryYAxisDisplayOptions = Lens.lens (\LineChartConfiguration' {secondaryYAxisDisplayOptions} -> secondaryYAxisDisplayOptions) (\s@LineChartConfiguration' {} a -> s {secondaryYAxisDisplayOptions = a} :: LineChartConfiguration)

-- | The options that determine the presentation of the secondary y-axis
-- label.
lineChartConfiguration_secondaryYAxisLabelOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
lineChartConfiguration_secondaryYAxisLabelOptions = Lens.lens (\LineChartConfiguration' {secondaryYAxisLabelOptions} -> secondaryYAxisLabelOptions) (\s@LineChartConfiguration' {} a -> s {secondaryYAxisLabelOptions = a} :: LineChartConfiguration)

-- | The series item configuration of a line chart.
lineChartConfiguration_series :: Lens.Lens' LineChartConfiguration (Prelude.Maybe [SeriesItem])
lineChartConfiguration_series = Lens.lens (\LineChartConfiguration' {series} -> series) (\s@LineChartConfiguration' {} a -> s {series = a} :: LineChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The small multiples setup for the visual.
lineChartConfiguration_smallMultiplesOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe SmallMultiplesOptions)
lineChartConfiguration_smallMultiplesOptions = Lens.lens (\LineChartConfiguration' {smallMultiplesOptions} -> smallMultiplesOptions) (\s@LineChartConfiguration' {} a -> s {smallMultiplesOptions = a} :: LineChartConfiguration)

-- | The sort configuration of a line chart.
lineChartConfiguration_sortConfiguration :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineChartSortConfiguration)
lineChartConfiguration_sortConfiguration = Lens.lens (\LineChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@LineChartConfiguration' {} a -> s {sortConfiguration = a} :: LineChartConfiguration)

-- | The tooltip configuration of a line chart.
lineChartConfiguration_tooltip :: Lens.Lens' LineChartConfiguration (Prelude.Maybe TooltipOptions)
lineChartConfiguration_tooltip = Lens.lens (\LineChartConfiguration' {tooltip} -> tooltip) (\s@LineChartConfiguration' {} a -> s {tooltip = a} :: LineChartConfiguration)

-- | Determines the type of the line chart.
lineChartConfiguration_type :: Lens.Lens' LineChartConfiguration (Prelude.Maybe LineChartType)
lineChartConfiguration_type = Lens.lens (\LineChartConfiguration' {type'} -> type') (\s@LineChartConfiguration' {} a -> s {type' = a} :: LineChartConfiguration)

-- | The visual palette configuration of a line chart.
lineChartConfiguration_visualPalette :: Lens.Lens' LineChartConfiguration (Prelude.Maybe VisualPalette)
lineChartConfiguration_visualPalette = Lens.lens (\LineChartConfiguration' {visualPalette} -> visualPalette) (\s@LineChartConfiguration' {} a -> s {visualPalette = a} :: LineChartConfiguration)

-- | The options that determine the presentation of the x-axis.
lineChartConfiguration_xAxisDisplayOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe AxisDisplayOptions)
lineChartConfiguration_xAxisDisplayOptions = Lens.lens (\LineChartConfiguration' {xAxisDisplayOptions} -> xAxisDisplayOptions) (\s@LineChartConfiguration' {} a -> s {xAxisDisplayOptions = a} :: LineChartConfiguration)

-- | The options that determine the presentation of the x-axis label.
lineChartConfiguration_xAxisLabelOptions :: Lens.Lens' LineChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
lineChartConfiguration_xAxisLabelOptions = Lens.lens (\LineChartConfiguration' {xAxisLabelOptions} -> xAxisLabelOptions) (\s@LineChartConfiguration' {} a -> s {xAxisLabelOptions = a} :: LineChartConfiguration)

instance Data.FromJSON LineChartConfiguration where
  parseJSON =
    Data.withObject
      "LineChartConfiguration"
      ( \x ->
          LineChartConfiguration'
            Prelude.<$> (x Data..:? "ContributionAnalysisDefaults")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "DefaultSeriesSettings")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> ( x Data..:? "ForecastConfigurations"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "PrimaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "PrimaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "ReferenceLines" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SecondaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "SecondaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "Series" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SmallMultiplesOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "VisualPalette")
            Prelude.<*> (x Data..:? "XAxisDisplayOptions")
            Prelude.<*> (x Data..:? "XAxisLabelOptions")
      )

instance Prelude.Hashable LineChartConfiguration where
  hashWithSalt _salt LineChartConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` contributionAnalysisDefaults
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` defaultSeriesSettings
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` forecastConfigurations
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` primaryYAxisDisplayOptions
      `Prelude.hashWithSalt` primaryYAxisLabelOptions
      `Prelude.hashWithSalt` referenceLines
      `Prelude.hashWithSalt` secondaryYAxisDisplayOptions
      `Prelude.hashWithSalt` secondaryYAxisLabelOptions
      `Prelude.hashWithSalt` series
      `Prelude.hashWithSalt` smallMultiplesOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` visualPalette
      `Prelude.hashWithSalt` xAxisDisplayOptions
      `Prelude.hashWithSalt` xAxisLabelOptions

instance Prelude.NFData LineChartConfiguration where
  rnf LineChartConfiguration' {..} =
    Prelude.rnf contributionAnalysisDefaults
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf defaultSeriesSettings
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf forecastConfigurations
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf primaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf primaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf referenceLines
      `Prelude.seq` Prelude.rnf secondaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf secondaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf series
      `Prelude.seq` Prelude.rnf smallMultiplesOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf visualPalette
      `Prelude.seq` Prelude.rnf xAxisDisplayOptions
      `Prelude.seq` Prelude.rnf xAxisLabelOptions

instance Data.ToJSON LineChartConfiguration where
  toJSON LineChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContributionAnalysisDefaults" Data..=)
              Prelude.<$> contributionAnalysisDefaults,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("DefaultSeriesSettings" Data..=)
              Prelude.<$> defaultSeriesSettings,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("ForecastConfigurations" Data..=)
              Prelude.<$> forecastConfigurations,
            ("Legend" Data..=) Prelude.<$> legend,
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
            ("Series" Data..=) Prelude.<$> series,
            ("SmallMultiplesOptions" Data..=)
              Prelude.<$> smallMultiplesOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("Type" Data..=) Prelude.<$> type',
            ("VisualPalette" Data..=) Prelude.<$> visualPalette,
            ("XAxisDisplayOptions" Data..=)
              Prelude.<$> xAxisDisplayOptions,
            ("XAxisLabelOptions" Data..=)
              Prelude.<$> xAxisLabelOptions
          ]
      )
