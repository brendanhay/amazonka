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
-- Module      : Amazonka.QuickSight.Types.ScatterPlotConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ScatterPlotConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.ScatterPlotFieldWells
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a scatter plot.
--
-- /See:/ 'newScatterPlotConfiguration' smart constructor.
data ScatterPlotConfiguration = ScatterPlotConfiguration'
  { -- | The options that determine if visual data labels are displayed.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe ScatterPlotFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The legend display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette,
    -- | The label display options (grid line, range, scale, and axis step) of
    -- the scatter plot\'s x-axis.
    xAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of the scatter plot\'s x-axis.
    xAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The label display options (grid line, range, scale, and axis step) of
    -- the scatter plot\'s y-axis.
    yAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility, and sort icon
    -- visibility) of the scatter plot\'s y-axis.
    yAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScatterPlotConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLabels', 'scatterPlotConfiguration_dataLabels' - The options that determine if visual data labels are displayed.
--
-- 'fieldWells', 'scatterPlotConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'scatterPlotConfiguration_legend' - The legend display setup of the visual.
--
-- 'tooltip', 'scatterPlotConfiguration_tooltip' - The legend display setup of the visual.
--
-- 'visualPalette', 'scatterPlotConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
--
-- 'xAxisDisplayOptions', 'scatterPlotConfiguration_xAxisDisplayOptions' - The label display options (grid line, range, scale, and axis step) of
-- the scatter plot\'s x-axis.
--
-- 'xAxisLabelOptions', 'scatterPlotConfiguration_xAxisLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of the scatter plot\'s x-axis.
--
-- 'yAxisDisplayOptions', 'scatterPlotConfiguration_yAxisDisplayOptions' - The label display options (grid line, range, scale, and axis step) of
-- the scatter plot\'s y-axis.
--
-- 'yAxisLabelOptions', 'scatterPlotConfiguration_yAxisLabelOptions' - The label options (label text, label visibility, and sort icon
-- visibility) of the scatter plot\'s y-axis.
newScatterPlotConfiguration ::
  ScatterPlotConfiguration
newScatterPlotConfiguration =
  ScatterPlotConfiguration'
    { dataLabels =
        Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      visualPalette = Prelude.Nothing,
      xAxisDisplayOptions = Prelude.Nothing,
      xAxisLabelOptions = Prelude.Nothing,
      yAxisDisplayOptions = Prelude.Nothing,
      yAxisLabelOptions = Prelude.Nothing
    }

-- | The options that determine if visual data labels are displayed.
scatterPlotConfiguration_dataLabels :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe DataLabelOptions)
scatterPlotConfiguration_dataLabels = Lens.lens (\ScatterPlotConfiguration' {dataLabels} -> dataLabels) (\s@ScatterPlotConfiguration' {} a -> s {dataLabels = a} :: ScatterPlotConfiguration)

-- | The field wells of the visual.
scatterPlotConfiguration_fieldWells :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe ScatterPlotFieldWells)
scatterPlotConfiguration_fieldWells = Lens.lens (\ScatterPlotConfiguration' {fieldWells} -> fieldWells) (\s@ScatterPlotConfiguration' {} a -> s {fieldWells = a} :: ScatterPlotConfiguration)

-- | The legend display setup of the visual.
scatterPlotConfiguration_legend :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe LegendOptions)
scatterPlotConfiguration_legend = Lens.lens (\ScatterPlotConfiguration' {legend} -> legend) (\s@ScatterPlotConfiguration' {} a -> s {legend = a} :: ScatterPlotConfiguration)

-- | The legend display setup of the visual.
scatterPlotConfiguration_tooltip :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe TooltipOptions)
scatterPlotConfiguration_tooltip = Lens.lens (\ScatterPlotConfiguration' {tooltip} -> tooltip) (\s@ScatterPlotConfiguration' {} a -> s {tooltip = a} :: ScatterPlotConfiguration)

-- | The palette (chart color) display setup of the visual.
scatterPlotConfiguration_visualPalette :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe VisualPalette)
scatterPlotConfiguration_visualPalette = Lens.lens (\ScatterPlotConfiguration' {visualPalette} -> visualPalette) (\s@ScatterPlotConfiguration' {} a -> s {visualPalette = a} :: ScatterPlotConfiguration)

-- | The label display options (grid line, range, scale, and axis step) of
-- the scatter plot\'s x-axis.
scatterPlotConfiguration_xAxisDisplayOptions :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe AxisDisplayOptions)
scatterPlotConfiguration_xAxisDisplayOptions = Lens.lens (\ScatterPlotConfiguration' {xAxisDisplayOptions} -> xAxisDisplayOptions) (\s@ScatterPlotConfiguration' {} a -> s {xAxisDisplayOptions = a} :: ScatterPlotConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of the scatter plot\'s x-axis.
scatterPlotConfiguration_xAxisLabelOptions :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe ChartAxisLabelOptions)
scatterPlotConfiguration_xAxisLabelOptions = Lens.lens (\ScatterPlotConfiguration' {xAxisLabelOptions} -> xAxisLabelOptions) (\s@ScatterPlotConfiguration' {} a -> s {xAxisLabelOptions = a} :: ScatterPlotConfiguration)

-- | The label display options (grid line, range, scale, and axis step) of
-- the scatter plot\'s y-axis.
scatterPlotConfiguration_yAxisDisplayOptions :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe AxisDisplayOptions)
scatterPlotConfiguration_yAxisDisplayOptions = Lens.lens (\ScatterPlotConfiguration' {yAxisDisplayOptions} -> yAxisDisplayOptions) (\s@ScatterPlotConfiguration' {} a -> s {yAxisDisplayOptions = a} :: ScatterPlotConfiguration)

-- | The label options (label text, label visibility, and sort icon
-- visibility) of the scatter plot\'s y-axis.
scatterPlotConfiguration_yAxisLabelOptions :: Lens.Lens' ScatterPlotConfiguration (Prelude.Maybe ChartAxisLabelOptions)
scatterPlotConfiguration_yAxisLabelOptions = Lens.lens (\ScatterPlotConfiguration' {yAxisLabelOptions} -> yAxisLabelOptions) (\s@ScatterPlotConfiguration' {} a -> s {yAxisLabelOptions = a} :: ScatterPlotConfiguration)

instance Data.FromJSON ScatterPlotConfiguration where
  parseJSON =
    Data.withObject
      "ScatterPlotConfiguration"
      ( \x ->
          ScatterPlotConfiguration'
            Prelude.<$> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "VisualPalette")
            Prelude.<*> (x Data..:? "XAxisDisplayOptions")
            Prelude.<*> (x Data..:? "XAxisLabelOptions")
            Prelude.<*> (x Data..:? "YAxisDisplayOptions")
            Prelude.<*> (x Data..:? "YAxisLabelOptions")
      )

instance Prelude.Hashable ScatterPlotConfiguration where
  hashWithSalt _salt ScatterPlotConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` visualPalette
      `Prelude.hashWithSalt` xAxisDisplayOptions
      `Prelude.hashWithSalt` xAxisLabelOptions
      `Prelude.hashWithSalt` yAxisDisplayOptions
      `Prelude.hashWithSalt` yAxisLabelOptions

instance Prelude.NFData ScatterPlotConfiguration where
  rnf ScatterPlotConfiguration' {..} =
    Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf visualPalette
      `Prelude.seq` Prelude.rnf xAxisDisplayOptions
      `Prelude.seq` Prelude.rnf xAxisLabelOptions
      `Prelude.seq` Prelude.rnf yAxisDisplayOptions
      `Prelude.seq` Prelude.rnf yAxisLabelOptions

instance Data.ToJSON ScatterPlotConfiguration where
  toJSON ScatterPlotConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette,
            ("XAxisDisplayOptions" Data..=)
              Prelude.<$> xAxisDisplayOptions,
            ("XAxisLabelOptions" Data..=)
              Prelude.<$> xAxisLabelOptions,
            ("YAxisDisplayOptions" Data..=)
              Prelude.<$> yAxisDisplayOptions,
            ("YAxisLabelOptions" Data..=)
              Prelude.<$> yAxisLabelOptions
          ]
      )
