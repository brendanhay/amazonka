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
-- Module      : Amazonka.QuickSight.Types.HeatMapConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HeatMapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ColorScale
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.HeatMapFieldWells
import Amazonka.QuickSight.Types.HeatMapSortConfiguration
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.TooltipOptions

-- | The configuration of a heat map.
--
-- /See:/ 'newHeatMapConfiguration' smart constructor.
data HeatMapConfiguration = HeatMapConfiguration'
  { -- | The color options (gradient color, point of divergence) in a heat map.
    colorScale :: Prelude.Maybe ColorScale,
    -- | The label options of the column that is displayed in a heat map.
    columnLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The options that determine if visual data labels are displayed.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe HeatMapFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The label options of the row that is displayed in a @heat map@.
    rowLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The sort configuration of a heat map.
    sortConfiguration :: Prelude.Maybe HeatMapSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HeatMapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colorScale', 'heatMapConfiguration_colorScale' - The color options (gradient color, point of divergence) in a heat map.
--
-- 'columnLabelOptions', 'heatMapConfiguration_columnLabelOptions' - The label options of the column that is displayed in a heat map.
--
-- 'dataLabels', 'heatMapConfiguration_dataLabels' - The options that determine if visual data labels are displayed.
--
-- 'fieldWells', 'heatMapConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'heatMapConfiguration_legend' - The legend display setup of the visual.
--
-- 'rowLabelOptions', 'heatMapConfiguration_rowLabelOptions' - The label options of the row that is displayed in a @heat map@.
--
-- 'sortConfiguration', 'heatMapConfiguration_sortConfiguration' - The sort configuration of a heat map.
--
-- 'tooltip', 'heatMapConfiguration_tooltip' - The tooltip display setup of the visual.
newHeatMapConfiguration ::
  HeatMapConfiguration
newHeatMapConfiguration =
  HeatMapConfiguration'
    { colorScale = Prelude.Nothing,
      columnLabelOptions = Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      rowLabelOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing
    }

-- | The color options (gradient color, point of divergence) in a heat map.
heatMapConfiguration_colorScale :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe ColorScale)
heatMapConfiguration_colorScale = Lens.lens (\HeatMapConfiguration' {colorScale} -> colorScale) (\s@HeatMapConfiguration' {} a -> s {colorScale = a} :: HeatMapConfiguration)

-- | The label options of the column that is displayed in a heat map.
heatMapConfiguration_columnLabelOptions :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe ChartAxisLabelOptions)
heatMapConfiguration_columnLabelOptions = Lens.lens (\HeatMapConfiguration' {columnLabelOptions} -> columnLabelOptions) (\s@HeatMapConfiguration' {} a -> s {columnLabelOptions = a} :: HeatMapConfiguration)

-- | The options that determine if visual data labels are displayed.
heatMapConfiguration_dataLabels :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe DataLabelOptions)
heatMapConfiguration_dataLabels = Lens.lens (\HeatMapConfiguration' {dataLabels} -> dataLabels) (\s@HeatMapConfiguration' {} a -> s {dataLabels = a} :: HeatMapConfiguration)

-- | The field wells of the visual.
heatMapConfiguration_fieldWells :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe HeatMapFieldWells)
heatMapConfiguration_fieldWells = Lens.lens (\HeatMapConfiguration' {fieldWells} -> fieldWells) (\s@HeatMapConfiguration' {} a -> s {fieldWells = a} :: HeatMapConfiguration)

-- | The legend display setup of the visual.
heatMapConfiguration_legend :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe LegendOptions)
heatMapConfiguration_legend = Lens.lens (\HeatMapConfiguration' {legend} -> legend) (\s@HeatMapConfiguration' {} a -> s {legend = a} :: HeatMapConfiguration)

-- | The label options of the row that is displayed in a @heat map@.
heatMapConfiguration_rowLabelOptions :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe ChartAxisLabelOptions)
heatMapConfiguration_rowLabelOptions = Lens.lens (\HeatMapConfiguration' {rowLabelOptions} -> rowLabelOptions) (\s@HeatMapConfiguration' {} a -> s {rowLabelOptions = a} :: HeatMapConfiguration)

-- | The sort configuration of a heat map.
heatMapConfiguration_sortConfiguration :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe HeatMapSortConfiguration)
heatMapConfiguration_sortConfiguration = Lens.lens (\HeatMapConfiguration' {sortConfiguration} -> sortConfiguration) (\s@HeatMapConfiguration' {} a -> s {sortConfiguration = a} :: HeatMapConfiguration)

-- | The tooltip display setup of the visual.
heatMapConfiguration_tooltip :: Lens.Lens' HeatMapConfiguration (Prelude.Maybe TooltipOptions)
heatMapConfiguration_tooltip = Lens.lens (\HeatMapConfiguration' {tooltip} -> tooltip) (\s@HeatMapConfiguration' {} a -> s {tooltip = a} :: HeatMapConfiguration)

instance Data.FromJSON HeatMapConfiguration where
  parseJSON =
    Data.withObject
      "HeatMapConfiguration"
      ( \x ->
          HeatMapConfiguration'
            Prelude.<$> (x Data..:? "ColorScale")
            Prelude.<*> (x Data..:? "ColumnLabelOptions")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "RowLabelOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
      )

instance Prelude.Hashable HeatMapConfiguration where
  hashWithSalt _salt HeatMapConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` colorScale
      `Prelude.hashWithSalt` columnLabelOptions
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` rowLabelOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip

instance Prelude.NFData HeatMapConfiguration where
  rnf HeatMapConfiguration' {..} =
    Prelude.rnf colorScale
      `Prelude.seq` Prelude.rnf columnLabelOptions
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf rowLabelOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip

instance Data.ToJSON HeatMapConfiguration where
  toJSON HeatMapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColorScale" Data..=) Prelude.<$> colorScale,
            ("ColumnLabelOptions" Data..=)
              Prelude.<$> columnLabelOptions,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("RowLabelOptions" Data..=)
              Prelude.<$> rowLabelOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip
          ]
      )
