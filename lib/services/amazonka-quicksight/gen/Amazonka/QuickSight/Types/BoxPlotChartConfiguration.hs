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
-- Module      : Amazonka.QuickSight.Types.BoxPlotChartConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.BoxPlotChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisDisplayOptions
import Amazonka.QuickSight.Types.BoxPlotFieldWells
import Amazonka.QuickSight.Types.BoxPlotOptions
import Amazonka.QuickSight.Types.BoxPlotSortConfiguration
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.ReferenceLine
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @BoxPlotVisual@.
--
-- /See:/ 'newBoxPlotChartConfiguration' smart constructor.
data BoxPlotChartConfiguration = BoxPlotChartConfiguration'
  { -- | The box plot chart options for a box plot visual
    boxPlotOptions :: Prelude.Maybe BoxPlotOptions,
    -- | The label display options (grid line, range, scale, axis step) of a box
    -- plot category.
    categoryAxis :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility and sort Icon
    -- visibility) of a box plot category.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe BoxPlotFieldWells,
    legend :: Prelude.Maybe LegendOptions,
    -- | The label display options (grid line, range, scale, axis step) of a box
    -- plot category.
    primaryYAxisDisplayOptions :: Prelude.Maybe AxisDisplayOptions,
    -- | The label options (label text, label visibility and sort icon
    -- visibility) of a box plot value.
    primaryYAxisLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The reference line setup of the visual.
    referenceLines :: Prelude.Maybe [ReferenceLine],
    -- | The sort configuration of a @BoxPlotVisual@.
    sortConfiguration :: Prelude.Maybe BoxPlotSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The palette (chart color) display setup of the visual.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BoxPlotChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'boxPlotOptions', 'boxPlotChartConfiguration_boxPlotOptions' - The box plot chart options for a box plot visual
--
-- 'categoryAxis', 'boxPlotChartConfiguration_categoryAxis' - The label display options (grid line, range, scale, axis step) of a box
-- plot category.
--
-- 'categoryLabelOptions', 'boxPlotChartConfiguration_categoryLabelOptions' - The label options (label text, label visibility and sort Icon
-- visibility) of a box plot category.
--
-- 'fieldWells', 'boxPlotChartConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'boxPlotChartConfiguration_legend' - Undocumented member.
--
-- 'primaryYAxisDisplayOptions', 'boxPlotChartConfiguration_primaryYAxisDisplayOptions' - The label display options (grid line, range, scale, axis step) of a box
-- plot category.
--
-- 'primaryYAxisLabelOptions', 'boxPlotChartConfiguration_primaryYAxisLabelOptions' - The label options (label text, label visibility and sort icon
-- visibility) of a box plot value.
--
-- 'referenceLines', 'boxPlotChartConfiguration_referenceLines' - The reference line setup of the visual.
--
-- 'sortConfiguration', 'boxPlotChartConfiguration_sortConfiguration' - The sort configuration of a @BoxPlotVisual@.
--
-- 'tooltip', 'boxPlotChartConfiguration_tooltip' - The tooltip display setup of the visual.
--
-- 'visualPalette', 'boxPlotChartConfiguration_visualPalette' - The palette (chart color) display setup of the visual.
newBoxPlotChartConfiguration ::
  BoxPlotChartConfiguration
newBoxPlotChartConfiguration =
  BoxPlotChartConfiguration'
    { boxPlotOptions =
        Prelude.Nothing,
      categoryAxis = Prelude.Nothing,
      categoryLabelOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      legend = Prelude.Nothing,
      primaryYAxisDisplayOptions = Prelude.Nothing,
      primaryYAxisLabelOptions = Prelude.Nothing,
      referenceLines = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | The box plot chart options for a box plot visual
boxPlotChartConfiguration_boxPlotOptions :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe BoxPlotOptions)
boxPlotChartConfiguration_boxPlotOptions = Lens.lens (\BoxPlotChartConfiguration' {boxPlotOptions} -> boxPlotOptions) (\s@BoxPlotChartConfiguration' {} a -> s {boxPlotOptions = a} :: BoxPlotChartConfiguration)

-- | The label display options (grid line, range, scale, axis step) of a box
-- plot category.
boxPlotChartConfiguration_categoryAxis :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe AxisDisplayOptions)
boxPlotChartConfiguration_categoryAxis = Lens.lens (\BoxPlotChartConfiguration' {categoryAxis} -> categoryAxis) (\s@BoxPlotChartConfiguration' {} a -> s {categoryAxis = a} :: BoxPlotChartConfiguration)

-- | The label options (label text, label visibility and sort Icon
-- visibility) of a box plot category.
boxPlotChartConfiguration_categoryLabelOptions :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
boxPlotChartConfiguration_categoryLabelOptions = Lens.lens (\BoxPlotChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@BoxPlotChartConfiguration' {} a -> s {categoryLabelOptions = a} :: BoxPlotChartConfiguration)

-- | The field wells of the visual.
boxPlotChartConfiguration_fieldWells :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe BoxPlotFieldWells)
boxPlotChartConfiguration_fieldWells = Lens.lens (\BoxPlotChartConfiguration' {fieldWells} -> fieldWells) (\s@BoxPlotChartConfiguration' {} a -> s {fieldWells = a} :: BoxPlotChartConfiguration)

-- | Undocumented member.
boxPlotChartConfiguration_legend :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe LegendOptions)
boxPlotChartConfiguration_legend = Lens.lens (\BoxPlotChartConfiguration' {legend} -> legend) (\s@BoxPlotChartConfiguration' {} a -> s {legend = a} :: BoxPlotChartConfiguration)

-- | The label display options (grid line, range, scale, axis step) of a box
-- plot category.
boxPlotChartConfiguration_primaryYAxisDisplayOptions :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe AxisDisplayOptions)
boxPlotChartConfiguration_primaryYAxisDisplayOptions = Lens.lens (\BoxPlotChartConfiguration' {primaryYAxisDisplayOptions} -> primaryYAxisDisplayOptions) (\s@BoxPlotChartConfiguration' {} a -> s {primaryYAxisDisplayOptions = a} :: BoxPlotChartConfiguration)

-- | The label options (label text, label visibility and sort icon
-- visibility) of a box plot value.
boxPlotChartConfiguration_primaryYAxisLabelOptions :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
boxPlotChartConfiguration_primaryYAxisLabelOptions = Lens.lens (\BoxPlotChartConfiguration' {primaryYAxisLabelOptions} -> primaryYAxisLabelOptions) (\s@BoxPlotChartConfiguration' {} a -> s {primaryYAxisLabelOptions = a} :: BoxPlotChartConfiguration)

-- | The reference line setup of the visual.
boxPlotChartConfiguration_referenceLines :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe [ReferenceLine])
boxPlotChartConfiguration_referenceLines = Lens.lens (\BoxPlotChartConfiguration' {referenceLines} -> referenceLines) (\s@BoxPlotChartConfiguration' {} a -> s {referenceLines = a} :: BoxPlotChartConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The sort configuration of a @BoxPlotVisual@.
boxPlotChartConfiguration_sortConfiguration :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe BoxPlotSortConfiguration)
boxPlotChartConfiguration_sortConfiguration = Lens.lens (\BoxPlotChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@BoxPlotChartConfiguration' {} a -> s {sortConfiguration = a} :: BoxPlotChartConfiguration)

-- | The tooltip display setup of the visual.
boxPlotChartConfiguration_tooltip :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe TooltipOptions)
boxPlotChartConfiguration_tooltip = Lens.lens (\BoxPlotChartConfiguration' {tooltip} -> tooltip) (\s@BoxPlotChartConfiguration' {} a -> s {tooltip = a} :: BoxPlotChartConfiguration)

-- | The palette (chart color) display setup of the visual.
boxPlotChartConfiguration_visualPalette :: Lens.Lens' BoxPlotChartConfiguration (Prelude.Maybe VisualPalette)
boxPlotChartConfiguration_visualPalette = Lens.lens (\BoxPlotChartConfiguration' {visualPalette} -> visualPalette) (\s@BoxPlotChartConfiguration' {} a -> s {visualPalette = a} :: BoxPlotChartConfiguration)

instance Data.FromJSON BoxPlotChartConfiguration where
  parseJSON =
    Data.withObject
      "BoxPlotChartConfiguration"
      ( \x ->
          BoxPlotChartConfiguration'
            Prelude.<$> (x Data..:? "BoxPlotOptions")
            Prelude.<*> (x Data..:? "CategoryAxis")
            Prelude.<*> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "PrimaryYAxisDisplayOptions")
            Prelude.<*> (x Data..:? "PrimaryYAxisLabelOptions")
            Prelude.<*> (x Data..:? "ReferenceLines" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable BoxPlotChartConfiguration where
  hashWithSalt _salt BoxPlotChartConfiguration' {..} =
    _salt `Prelude.hashWithSalt` boxPlotOptions
      `Prelude.hashWithSalt` categoryAxis
      `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` primaryYAxisDisplayOptions
      `Prelude.hashWithSalt` primaryYAxisLabelOptions
      `Prelude.hashWithSalt` referenceLines
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData BoxPlotChartConfiguration where
  rnf BoxPlotChartConfiguration' {..} =
    Prelude.rnf boxPlotOptions
      `Prelude.seq` Prelude.rnf categoryAxis
      `Prelude.seq` Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf primaryYAxisDisplayOptions
      `Prelude.seq` Prelude.rnf primaryYAxisLabelOptions
      `Prelude.seq` Prelude.rnf referenceLines
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON BoxPlotChartConfiguration where
  toJSON BoxPlotChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BoxPlotOptions" Data..=)
              Prelude.<$> boxPlotOptions,
            ("CategoryAxis" Data..=) Prelude.<$> categoryAxis,
            ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("PrimaryYAxisDisplayOptions" Data..=)
              Prelude.<$> primaryYAxisDisplayOptions,
            ("PrimaryYAxisLabelOptions" Data..=)
              Prelude.<$> primaryYAxisLabelOptions,
            ("ReferenceLines" Data..=)
              Prelude.<$> referenceLines,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
