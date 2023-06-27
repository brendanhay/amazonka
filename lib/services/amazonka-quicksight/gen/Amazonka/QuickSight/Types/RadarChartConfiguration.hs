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
-- Module      : Amazonka.QuickSight.Types.RadarChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.RadarChartAxesRangeScale
import Amazonka.QuickSight.Types.RadarChartFieldWells
import Amazonka.QuickSight.Types.RadarChartSeriesSettings
import Amazonka.QuickSight.Types.RadarChartShape
import Amazonka.QuickSight.Types.RadarChartSortConfiguration
import Amazonka.QuickSight.Types.Visibility
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @RadarChartVisual@.
--
-- /See:/ 'newRadarChartConfiguration' smart constructor.
data RadarChartConfiguration = RadarChartConfiguration'
  { -- | Determines the visibility of the colors of alternatign bands in a radar
    -- chart.
    alternateBandColorsVisibility :: Prelude.Maybe Visibility,
    -- | The color of the even-numbered alternate bands of a radar chart.
    alternateBandEvenColor :: Prelude.Maybe Prelude.Text,
    -- | The color of the odd-numbered alternate bands of a radar chart.
    alternateBandOddColor :: Prelude.Maybe Prelude.Text,
    -- | The axis behavior options of a radar chart.
    axesRangeScale :: Prelude.Maybe RadarChartAxesRangeScale,
    -- | The base sreies settings of a radar chart.
    baseSeriesSettings :: Prelude.Maybe RadarChartSeriesSettings,
    -- | The category axis of a radar chart.
    categoryAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The category label options of a radar chart.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The color axis of a radar chart.
    colorAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The color label options of a radar chart.
    colorLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The field well configuration of a @RadarChartVisual@.
    fieldWells :: Prelude.Maybe RadarChartFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The shape of the radar chart.
    shape :: Prelude.Maybe RadarChartShape,
    -- | The sort configuration of a @RadarChartVisual@.
    sortConfiguration :: Prelude.Maybe RadarChartSortConfiguration,
    -- | The start angle of a radar chart\'s axis.
    startAngle :: Prelude.Maybe Prelude.Double,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alternateBandColorsVisibility', 'radarChartConfiguration_alternateBandColorsVisibility' - Determines the visibility of the colors of alternatign bands in a radar
-- chart.
--
-- 'alternateBandEvenColor', 'radarChartConfiguration_alternateBandEvenColor' - The color of the even-numbered alternate bands of a radar chart.
--
-- 'alternateBandOddColor', 'radarChartConfiguration_alternateBandOddColor' - The color of the odd-numbered alternate bands of a radar chart.
--
-- 'axesRangeScale', 'radarChartConfiguration_axesRangeScale' - The axis behavior options of a radar chart.
--
-- 'baseSeriesSettings', 'radarChartConfiguration_baseSeriesSettings' - The base sreies settings of a radar chart.
--
-- 'categoryAxis', 'radarChartConfiguration_categoryAxis' - The category axis of a radar chart.
--
-- 'categoryLabelOptions', 'radarChartConfiguration_categoryLabelOptions' - The category label options of a radar chart.
--
-- 'colorAxis', 'radarChartConfiguration_colorAxis' - The color axis of a radar chart.
--
-- 'colorLabelOptions', 'radarChartConfiguration_colorLabelOptions' - The color label options of a radar chart.
--
-- 'fieldWells', 'radarChartConfiguration_fieldWells' - The field well configuration of a @RadarChartVisual@.
--
-- 'legend', 'radarChartConfiguration_legend' - The legend display setup of the visual.
--
-- 'shape', 'radarChartConfiguration_shape' - The shape of the radar chart.
--
-- 'sortConfiguration', 'radarChartConfiguration_sortConfiguration' - The sort configuration of a @RadarChartVisual@.
--
-- 'startAngle', 'radarChartConfiguration_startAngle' - The start angle of a radar chart\'s axis.
--
-- 'visualPalette', 'radarChartConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
newRadarChartConfiguration ::
  RadarChartConfiguration
newRadarChartConfiguration =
  RadarChartConfiguration'
    { alternateBandColorsVisibility =
        Prelude.Nothing,
      alternateBandEvenColor = Prelude.Nothing,
      alternateBandOddColor = Prelude.Nothing,
      axesRangeScale = Prelude.Nothing,
      baseSeriesSettings = Prelude.Nothing,
      categoryAxis = Prelude.Nothing,
      categoryLabelOptions = Prelude.Nothing,
      colorAxis = Prelude.Nothing,
      colorLabelOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      shape = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      startAngle = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | Determines the visibility of the colors of alternatign bands in a radar
-- chart.
radarChartConfiguration_alternateBandColorsVisibility :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe Visibility)
radarChartConfiguration_alternateBandColorsVisibility = Lens.lens (\RadarChartConfiguration' {alternateBandColorsVisibility} -> alternateBandColorsVisibility) (\s@RadarChartConfiguration' {} a -> s {alternateBandColorsVisibility = a} :: RadarChartConfiguration)

-- | The color of the even-numbered alternate bands of a radar chart.
radarChartConfiguration_alternateBandEvenColor :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe Prelude.Text)
radarChartConfiguration_alternateBandEvenColor = Lens.lens (\RadarChartConfiguration' {alternateBandEvenColor} -> alternateBandEvenColor) (\s@RadarChartConfiguration' {} a -> s {alternateBandEvenColor = a} :: RadarChartConfiguration)

-- | The color of the odd-numbered alternate bands of a radar chart.
radarChartConfiguration_alternateBandOddColor :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe Prelude.Text)
radarChartConfiguration_alternateBandOddColor = Lens.lens (\RadarChartConfiguration' {alternateBandOddColor} -> alternateBandOddColor) (\s@RadarChartConfiguration' {} a -> s {alternateBandOddColor = a} :: RadarChartConfiguration)

-- | The axis behavior options of a radar chart.
radarChartConfiguration_axesRangeScale :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe RadarChartAxesRangeScale)
radarChartConfiguration_axesRangeScale = Lens.lens (\RadarChartConfiguration' {axesRangeScale} -> axesRangeScale) (\s@RadarChartConfiguration' {} a -> s {axesRangeScale = a} :: RadarChartConfiguration)

-- | The base sreies settings of a radar chart.
radarChartConfiguration_baseSeriesSettings :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe RadarChartSeriesSettings)
radarChartConfiguration_baseSeriesSettings = Lens.lens (\RadarChartConfiguration' {baseSeriesSettings} -> baseSeriesSettings) (\s@RadarChartConfiguration' {} a -> s {baseSeriesSettings = a} :: RadarChartConfiguration)

-- | The category axis of a radar chart.
radarChartConfiguration_categoryAxis :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe AxisDisplayOptions)
radarChartConfiguration_categoryAxis = Lens.lens (\RadarChartConfiguration' {categoryAxis} -> categoryAxis) (\s@RadarChartConfiguration' {} a -> s {categoryAxis = a} :: RadarChartConfiguration)

-- | The category label options of a radar chart.
radarChartConfiguration_categoryLabelOptions :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
radarChartConfiguration_categoryLabelOptions = Lens.lens (\RadarChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@RadarChartConfiguration' {} a -> s {categoryLabelOptions = a} :: RadarChartConfiguration)

-- | The color axis of a radar chart.
radarChartConfiguration_colorAxis :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe AxisDisplayOptions)
radarChartConfiguration_colorAxis = Lens.lens (\RadarChartConfiguration' {colorAxis} -> colorAxis) (\s@RadarChartConfiguration' {} a -> s {colorAxis = a} :: RadarChartConfiguration)

-- | The color label options of a radar chart.
radarChartConfiguration_colorLabelOptions :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
radarChartConfiguration_colorLabelOptions = Lens.lens (\RadarChartConfiguration' {colorLabelOptions} -> colorLabelOptions) (\s@RadarChartConfiguration' {} a -> s {colorLabelOptions = a} :: RadarChartConfiguration)

-- | The field well configuration of a @RadarChartVisual@.
radarChartConfiguration_fieldWells :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe RadarChartFieldWells)
radarChartConfiguration_fieldWells = Lens.lens (\RadarChartConfiguration' {fieldWells} -> fieldWells) (\s@RadarChartConfiguration' {} a -> s {fieldWells = a} :: RadarChartConfiguration)

-- | The legend display setup of the visual.
radarChartConfiguration_legend :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe LegendOptions)
radarChartConfiguration_legend = Lens.lens (\RadarChartConfiguration' {legend} -> legend) (\s@RadarChartConfiguration' {} a -> s {legend = a} :: RadarChartConfiguration)

-- | The shape of the radar chart.
radarChartConfiguration_shape :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe RadarChartShape)
radarChartConfiguration_shape = Lens.lens (\RadarChartConfiguration' {shape} -> shape) (\s@RadarChartConfiguration' {} a -> s {shape = a} :: RadarChartConfiguration)

-- | The sort configuration of a @RadarChartVisual@.
radarChartConfiguration_sortConfiguration :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe RadarChartSortConfiguration)
radarChartConfiguration_sortConfiguration = Lens.lens (\RadarChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@RadarChartConfiguration' {} a -> s {sortConfiguration = a} :: RadarChartConfiguration)

-- | The start angle of a radar chart\'s axis.
radarChartConfiguration_startAngle :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe Prelude.Double)
radarChartConfiguration_startAngle = Lens.lens (\RadarChartConfiguration' {startAngle} -> startAngle) (\s@RadarChartConfiguration' {} a -> s {startAngle = a} :: RadarChartConfiguration)

-- | The palette (chart color) display setup of the visual.
radarChartConfiguration_visualPalette :: Lens.Lens' RadarChartConfiguration (Prelude.Maybe VisualPalette)
radarChartConfiguration_visualPalette = Lens.lens (\RadarChartConfiguration' {visualPalette} -> visualPalette) (\s@RadarChartConfiguration' {} a -> s {visualPalette = a} :: RadarChartConfiguration)

instance Data.FromJSON RadarChartConfiguration where
  parseJSON =
    Data.withObject
      "RadarChartConfiguration"
      ( \x ->
          RadarChartConfiguration'
            Prelude.<$> (x Data..:? "AlternateBandColorsVisibility")
            Prelude.<*> (x Data..:? "AlternateBandEvenColor")
            Prelude.<*> (x Data..:? "AlternateBandOddColor")
            Prelude.<*> (x Data..:? "AxesRangeScale")
            Prelude.<*> (x Data..:? "BaseSeriesSettings")
            Prelude.<*> (x Data..:? "CategoryAxis")
            Prelude.<*> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "ColorAxis")
            Prelude.<*> (x Data..:? "ColorLabelOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "Shape")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "StartAngle")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable RadarChartConfiguration where
  hashWithSalt _salt RadarChartConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` alternateBandColorsVisibility
      `Prelude.hashWithSalt` alternateBandEvenColor
      `Prelude.hashWithSalt` alternateBandOddColor
      `Prelude.hashWithSalt` axesRangeScale
      `Prelude.hashWithSalt` baseSeriesSettings
      `Prelude.hashWithSalt` categoryAxis
      `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` colorAxis
      `Prelude.hashWithSalt` colorLabelOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` shape
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` startAngle
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData RadarChartConfiguration where
  rnf RadarChartConfiguration' {..} =
    Prelude.rnf alternateBandColorsVisibility
      `Prelude.seq` Prelude.rnf alternateBandEvenColor
      `Prelude.seq` Prelude.rnf alternateBandOddColor
      `Prelude.seq` Prelude.rnf axesRangeScale
      `Prelude.seq` Prelude.rnf baseSeriesSettings
      `Prelude.seq` Prelude.rnf categoryAxis
      `Prelude.seq` Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf colorAxis
      `Prelude.seq` Prelude.rnf colorLabelOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf shape
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf startAngle
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON RadarChartConfiguration where
  toJSON RadarChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlternateBandColorsVisibility" Data..=)
              Prelude.<$> alternateBandColorsVisibility,
            ("AlternateBandEvenColor" Data..=)
              Prelude.<$> alternateBandEvenColor,
            ("AlternateBandOddColor" Data..=)
              Prelude.<$> alternateBandOddColor,
            ("AxesRangeScale" Data..=)
              Prelude.<$> axesRangeScale,
            ("BaseSeriesSettings" Data..=)
              Prelude.<$> baseSeriesSettings,
            ("CategoryAxis" Data..=) Prelude.<$> categoryAxis,
            ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("ColorAxis" Data..=) Prelude.<$> colorAxis,
            ("ColorLabelOptions" Data..=)
              Prelude.<$> colorLabelOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("Shape" Data..=) Prelude.<$> shape,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("StartAngle" Data..=) Prelude.<$> startAngle,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
