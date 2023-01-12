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
-- Module      : Amazonka.QuickSight.Types.HistogramConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.HistogramConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.HistogramBinOptions
import Amazonka.QuickSight.Types.HistogramFieldWells
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration for a @HistogramVisual@.
--
-- /See:/ 'newHistogramConfiguration' smart constructor.
data HistogramConfiguration = HistogramConfiguration'
  { -- | The options that determine the presentation of histogram bins.
    binOptions :: Prelude.Maybe HistogramBinOptions,
    -- | The data label configuration of a histogram.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field well configuration of a histogram.
    fieldWells :: Prelude.Maybe HistogramFieldWells,
    -- | The tooltip configuration of a histogram.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The visual palette configuration of a histogram.
    visualPalette :: Prelude.Maybe VisualPalette,
    -- | The options that determine the presentation of the x-axis.
    xAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The options that determine the presentation of the x-axis label.
    xAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The options that determine the presentation of the y-axis.
    yAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistogramConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binOptions', 'histogramConfiguration_binOptions' - The options that determine the presentation of histogram bins.
--
-- 'dataLabels', 'histogramConfiguration_dataLabels' - The data label configuration of a histogram.
--
-- 'fieldWells', 'histogramConfiguration_fieldWells' - The field well configuration of a histogram.
--
-- 'tooltip', 'histogramConfiguration_tooltip' - The tooltip configuration of a histogram.
--
-- 'visualPalette', 'histogramConfiguration_visualPalette' - The visual palette configuration of a histogram.
--
-- 'xAxisDisplayOptions', 'histogramConfiguration_xAxisDisplayOptions' - The options that determine the presentation of the x-axis.
--
-- 'xAxisLabelOptions', 'histogramConfiguration_xAxisLabelOptions' - The options that determine the presentation of the x-axis label.
--
-- 'yAxisDisplayOptions', 'histogramConfiguration_yAxisDisplayOptions' - The options that determine the presentation of the y-axis.
newHistogramConfiguration ::
  HistogramConfiguration
newHistogramConfiguration =
  HistogramConfiguration'
    { binOptions =
        Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      visualPalette = Prelude.Nothing,
      xAxisDisplayOptions = Prelude.Nothing,
      xAxisLabelOptions = Prelude.Nothing,
      yAxisDisplayOptions = Prelude.Nothing
    }

-- | The options that determine the presentation of histogram bins.
histogramConfiguration_binOptions :: Lens.Lens' HistogramConfiguration (Prelude.Maybe HistogramBinOptions)
histogramConfiguration_binOptions = Lens.lens (\HistogramConfiguration' {binOptions} -> binOptions) (\s@HistogramConfiguration' {} a -> s {binOptions = a} :: HistogramConfiguration)

-- | The data label configuration of a histogram.
histogramConfiguration_dataLabels :: Lens.Lens' HistogramConfiguration (Prelude.Maybe DataLabelOptions)
histogramConfiguration_dataLabels = Lens.lens (\HistogramConfiguration' {dataLabels} -> dataLabels) (\s@HistogramConfiguration' {} a -> s {dataLabels = a} :: HistogramConfiguration)

-- | The field well configuration of a histogram.
histogramConfiguration_fieldWells :: Lens.Lens' HistogramConfiguration (Prelude.Maybe HistogramFieldWells)
histogramConfiguration_fieldWells = Lens.lens (\HistogramConfiguration' {fieldWells} -> fieldWells) (\s@HistogramConfiguration' {} a -> s {fieldWells = a} :: HistogramConfiguration)

-- | The tooltip configuration of a histogram.
histogramConfiguration_tooltip :: Lens.Lens' HistogramConfiguration (Prelude.Maybe TooltipOptions)
histogramConfiguration_tooltip = Lens.lens (\HistogramConfiguration' {tooltip} -> tooltip) (\s@HistogramConfiguration' {} a -> s {tooltip = a} :: HistogramConfiguration)

-- | The visual palette configuration of a histogram.
histogramConfiguration_visualPalette :: Lens.Lens' HistogramConfiguration (Prelude.Maybe VisualPalette)
histogramConfiguration_visualPalette = Lens.lens (\HistogramConfiguration' {visualPalette} -> visualPalette) (\s@HistogramConfiguration' {} a -> s {visualPalette = a} :: HistogramConfiguration)

-- | The options that determine the presentation of the x-axis.
histogramConfiguration_xAxisDisplayOptions :: Lens.Lens' HistogramConfiguration (Prelude.Maybe AxisDisplayOptions)
histogramConfiguration_xAxisDisplayOptions = Lens.lens (\HistogramConfiguration' {xAxisDisplayOptions} -> xAxisDisplayOptions) (\s@HistogramConfiguration' {} a -> s {xAxisDisplayOptions = a} :: HistogramConfiguration)

-- | The options that determine the presentation of the x-axis label.
histogramConfiguration_xAxisLabelOptions :: Lens.Lens' HistogramConfiguration (Prelude.Maybe ChartAxisLabelOptions)
histogramConfiguration_xAxisLabelOptions = Lens.lens (\HistogramConfiguration' {xAxisLabelOptions} -> xAxisLabelOptions) (\s@HistogramConfiguration' {} a -> s {xAxisLabelOptions = a} :: HistogramConfiguration)

-- | The options that determine the presentation of the y-axis.
histogramConfiguration_yAxisDisplayOptions :: Lens.Lens' HistogramConfiguration (Prelude.Maybe AxisDisplayOptions)
histogramConfiguration_yAxisDisplayOptions = Lens.lens (\HistogramConfiguration' {yAxisDisplayOptions} -> yAxisDisplayOptions) (\s@HistogramConfiguration' {} a -> s {yAxisDisplayOptions = a} :: HistogramConfiguration)

instance Data.FromJSON HistogramConfiguration where
  parseJSON =
    Data.withObject
      "HistogramConfiguration"
      ( \x ->
          HistogramConfiguration'
            Prelude.<$> (x Data..:? "BinOptions")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "VisualPalette")
            Prelude.<*> (x Data..:? "XAxisDisplayOptions")
            Prelude.<*> (x Data..:? "XAxisLabelOptions")
            Prelude.<*> (x Data..:? "YAxisDisplayOptions")
      )

instance Prelude.Hashable HistogramConfiguration where
  hashWithSalt _salt HistogramConfiguration' {..} =
    _salt `Prelude.hashWithSalt` binOptions
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` visualPalette
      `Prelude.hashWithSalt` xAxisDisplayOptions
      `Prelude.hashWithSalt` xAxisLabelOptions
      `Prelude.hashWithSalt` yAxisDisplayOptions

instance Prelude.NFData HistogramConfiguration where
  rnf HistogramConfiguration' {..} =
    Prelude.rnf binOptions
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf visualPalette
      `Prelude.seq` Prelude.rnf xAxisDisplayOptions
      `Prelude.seq` Prelude.rnf xAxisLabelOptions
      `Prelude.seq` Prelude.rnf yAxisDisplayOptions

instance Data.ToJSON HistogramConfiguration where
  toJSON HistogramConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BinOptions" Data..=) Prelude.<$> binOptions,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette,
            ("XAxisDisplayOptions" Data..=)
              Prelude.<$> xAxisDisplayOptions,
            ("XAxisLabelOptions" Data..=)
              Prelude.<$> xAxisLabelOptions,
            ("YAxisDisplayOptions" Data..=)
              Prelude.<$> yAxisDisplayOptions
          ]
      )
