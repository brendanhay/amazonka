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
-- Module      : Amazonka.QuickSight.Types.WaterfallChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.WaterfallChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.VisualPalette
import Amazonka.QuickSight.Types.WaterfallChartFieldWells
import Amazonka.QuickSight.Types.WaterfallChartOptions
import Amazonka.QuickSight.Types.WaterfallChartSortConfiguration

-- | The configuration for a waterfall visual.
--
-- /See:/ 'newWaterfallChartConfiguration' smart constructor.
data WaterfallChartConfiguration = WaterfallChartConfiguration'
  { -- | The options that determine the presentation of the category axis.
    categoryAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The options that determine the presentation of the category axis label.
    categoryAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The data label configuration of a waterfall visual.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field well configuration of a waterfall visual.
    fieldWells :: Prelude.Maybe WaterfallChartFieldWells,
    -- | The legend configuration of a waterfall visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The options that determine the presentation of the y-axis.
    primaryYAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The options that determine the presentation of the y-axis label.
    primaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The sort configuration of a waterfall visual.
    sortConfiguration :: Prelude.Maybe WaterfallChartSortConfiguration,
    -- | The visual palette configuration of a waterfall visual.
    visualPalette :: Prelude.Maybe VisualPalette,
    -- | The options that determine the presentation of a waterfall visual.
    waterfallChartOptions :: Prelude.Maybe WaterfallChartOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WaterfallChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryAxisDisplayOptions', 'waterfallChartConfiguration_categoryAxisDisplayOptions' - The options that determine the presentation of the category axis.
--
-- 'categoryAxisLabelOptions', 'waterfallChartConfiguration_categoryAxisLabelOptions' - The options that determine the presentation of the category axis label.
--
-- 'dataLabels', 'waterfallChartConfiguration_dataLabels' - The data label configuration of a waterfall visual.
--
-- 'fieldWells', 'waterfallChartConfiguration_fieldWells' - The field well configuration of a waterfall visual.
--
-- 'legend', 'waterfallChartConfiguration_legend' - The legend configuration of a waterfall visual.
--
-- 'primaryYAxisDisplayOptions', 'waterfallChartConfiguration_primaryYAxisDisplayOptions' - The options that determine the presentation of the y-axis.
--
-- 'primaryYAxisLabelOptions', 'waterfallChartConfiguration_primaryYAxisLabelOptions' - The options that determine the presentation of the y-axis label.
--
-- 'sortConfiguration', 'waterfallChartConfiguration_sortConfiguration' - The sort configuration of a waterfall visual.
--
-- 'visualPalette', 'waterfallChartConfiguration_visualPalette' - The visual palette configuration of a waterfall visual.
--
-- 'waterfallChartOptions', 'waterfallChartConfiguration_waterfallChartOptions' - The options that determine the presentation of a waterfall visual.
newWaterfallChartConfiguration ::
  WaterfallChartConfiguration
newWaterfallChartConfiguration =
  WaterfallChartConfiguration'
    { categoryAxisDisplayOptions =
        Prelude.Nothing,
      categoryAxisLabelOptions = Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      primaryYAxisDisplayOptions = Prelude.Nothing,
      primaryYAxisLabelOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      visualPalette = Prelude.Nothing,
      waterfallChartOptions = Prelude.Nothing
    }

-- | The options that determine the presentation of the category axis.
waterfallChartConfiguration_categoryAxisDisplayOptions :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe AxisDisplayOptions)
waterfallChartConfiguration_categoryAxisDisplayOptions = Lens.lens (\WaterfallChartConfiguration' {categoryAxisDisplayOptions} -> categoryAxisDisplayOptions) (\s@WaterfallChartConfiguration' {} a -> s {categoryAxisDisplayOptions = a} :: WaterfallChartConfiguration)

-- | The options that determine the presentation of the category axis label.
waterfallChartConfiguration_categoryAxisLabelOptions :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
waterfallChartConfiguration_categoryAxisLabelOptions = Lens.lens (\WaterfallChartConfiguration' {categoryAxisLabelOptions} -> categoryAxisLabelOptions) (\s@WaterfallChartConfiguration' {} a -> s {categoryAxisLabelOptions = a} :: WaterfallChartConfiguration)

-- | The data label configuration of a waterfall visual.
waterfallChartConfiguration_dataLabels :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe DataLabelOptions)
waterfallChartConfiguration_dataLabels = Lens.lens (\WaterfallChartConfiguration' {dataLabels} -> dataLabels) (\s@WaterfallChartConfiguration' {} a -> s {dataLabels = a} :: WaterfallChartConfiguration)

-- | The field well configuration of a waterfall visual.
waterfallChartConfiguration_fieldWells :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe WaterfallChartFieldWells)
waterfallChartConfiguration_fieldWells = Lens.lens (\WaterfallChartConfiguration' {fieldWells} -> fieldWells) (\s@WaterfallChartConfiguration' {} a -> s {fieldWells = a} :: WaterfallChartConfiguration)

-- | The legend configuration of a waterfall visual.
waterfallChartConfiguration_legend :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe LegendOptions)
waterfallChartConfiguration_legend = Lens.lens (\WaterfallChartConfiguration' {legend} -> legend) (\s@WaterfallChartConfiguration' {} a -> s {legend = a} :: WaterfallChartConfiguration)

-- | The options that determine the presentation of the y-axis.
waterfallChartConfiguration_primaryYAxisDisplayOptions :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe AxisDisplayOptions)
waterfallChartConfiguration_primaryYAxisDisplayOptions = Lens.lens (\WaterfallChartConfiguration' {primaryYAxisDisplayOptions} -> primaryYAxisDisplayOptions) (\s@WaterfallChartConfiguration' {} a -> s {primaryYAxisDisplayOptions = a} :: WaterfallChartConfiguration)

-- | The options that determine the presentation of the y-axis label.
waterfallChartConfiguration_primaryYAxisLabelOptions :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
waterfallChartConfiguration_primaryYAxisLabelOptions = Lens.lens (\WaterfallChartConfiguration' {primaryYAxisLabelOptions} -> primaryYAxisLabelOptions) (\s@WaterfallChartConfiguration' {} a -> s {primaryYAxisLabelOptions = a} :: WaterfallChartConfiguration)

-- | The sort configuration of a waterfall visual.
waterfallChartConfiguration_sortConfiguration :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe WaterfallChartSortConfiguration)
waterfallChartConfiguration_sortConfiguration = Lens.lens (\WaterfallChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@WaterfallChartConfiguration' {} a -> s {sortConfiguration = a} :: WaterfallChartConfiguration)

-- | The visual palette configuration of a waterfall visual.
waterfallChartConfiguration_visualPalette :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe VisualPalette)
waterfallChartConfiguration_visualPalette = Lens.lens (\WaterfallChartConfiguration' {visualPalette} -> visualPalette) (\s@WaterfallChartConfiguration' {} a -> s {visualPalette = a} :: WaterfallChartConfiguration)

-- | The options that determine the presentation of a waterfall visual.
waterfallChartConfiguration_waterfallChartOptions :: Lens.Lens' WaterfallChartConfiguration (Prelude.Maybe WaterfallChartOptions)
waterfallChartConfiguration_waterfallChartOptions = Lens.lens (\WaterfallChartConfiguration' {waterfallChartOptions} -> waterfallChartOptions) (\s@WaterfallChartConfiguration' {} a -> s {waterfallChartOptions = a} :: WaterfallChartConfiguration)

instance Data.FromJSON WaterfallChartConfiguration where
  parseJSON =
    Data.withObject
      "WaterfallChartConfiguration"
      ( \x ->
          WaterfallChartConfiguration'
            Prelude.<$> (x Data..:? "CategoryAxisDisplayOptions")
            Prelude.<*> (x Data..:? "CategoryAxisLabelOptions")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "PrimaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "PrimaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "VisualPalette")
            Prelude.<*> (x Data..:? "WaterfallChartOptions")
      )

instance Prelude.Hashable WaterfallChartConfiguration where
  hashWithSalt _salt WaterfallChartConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryAxisDisplayOptions
      `Prelude.hashWithSalt` categoryAxisLabelOptions
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` primaryYAxisDisplayOptions
      `Prelude.hashWithSalt` primaryYAxisLabelOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` visualPalette
      `Prelude.hashWithSalt` waterfallChartOptions

instance Prelude.NFData WaterfallChartConfiguration where
  rnf WaterfallChartConfiguration' {..} =
    Prelude.rnf categoryAxisDisplayOptions
      `Prelude.seq` Prelude.rnf categoryAxisLabelOptions
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf primaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf primaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf visualPalette
      `Prelude.seq` Prelude.rnf waterfallChartOptions

instance Data.ToJSON WaterfallChartConfiguration where
  toJSON WaterfallChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryAxisDisplayOptions" Data..=)
              Prelude.<$> categoryAxisDisplayOptions,
            ("CategoryAxisLabelOptions" Data..=)
              Prelude.<$> categoryAxisLabelOptions,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("PrimaryYAxisDisplayOptions" Data..=)
              Prelude.<$> primaryYAxisDisplayOptions,
            ("PrimaryYAxisLabelOptions" Data..=)
              Prelude.<$> primaryYAxisLabelOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette,
            ("WaterfallChartOptions" Data..=)
              Prelude.<$> waterfallChartOptions
          ]
      )
