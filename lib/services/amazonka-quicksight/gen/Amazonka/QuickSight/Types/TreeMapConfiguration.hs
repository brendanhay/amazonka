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
-- Module      : Amazonka.QuickSight.Types.TreeMapConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TreeMapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.ColorScale
import Amazonka.QuickSight.Types.DataLabelOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.TreeMapFieldWells
import Amazonka.QuickSight.Types.TreeMapSortConfiguration

-- | The configuration of a tree map.
--
-- /See:/ 'newTreeMapConfiguration' smart constructor.
data TreeMapConfiguration = TreeMapConfiguration'
  { -- | The label options (label text, label visibility) for the colors
    -- displayed in a tree map.
    colorLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The color options (gradient color, point of divergence) of a tree map.
    colorScale :: Prelude.Maybe ColorScale,
    -- | The options that determine if visual data labels are displayed.
    dataLabels :: Prelude.Maybe DataLabelOptions,
    -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe TreeMapFieldWells,
    -- | The label options (label text, label visibility) of the groups that are
    -- displayed in a tree map.
    groupLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The label options (label text, label visibility) of the sizes that are
    -- displayed in a tree map.
    sizeLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The sort configuration of a tree map.
    sortConfiguration :: Prelude.Maybe TreeMapSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TreeMapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colorLabelOptions', 'treeMapConfiguration_colorLabelOptions' - The label options (label text, label visibility) for the colors
-- displayed in a tree map.
--
-- 'colorScale', 'treeMapConfiguration_colorScale' - The color options (gradient color, point of divergence) of a tree map.
--
-- 'dataLabels', 'treeMapConfiguration_dataLabels' - The options that determine if visual data labels are displayed.
--
-- 'fieldWells', 'treeMapConfiguration_fieldWells' - The field wells of the visual.
--
-- 'groupLabelOptions', 'treeMapConfiguration_groupLabelOptions' - The label options (label text, label visibility) of the groups that are
-- displayed in a tree map.
--
-- 'legend', 'treeMapConfiguration_legend' - The legend display setup of the visual.
--
-- 'sizeLabelOptions', 'treeMapConfiguration_sizeLabelOptions' - The label options (label text, label visibility) of the sizes that are
-- displayed in a tree map.
--
-- 'sortConfiguration', 'treeMapConfiguration_sortConfiguration' - The sort configuration of a tree map.
--
-- 'tooltip', 'treeMapConfiguration_tooltip' - The tooltip display setup of the visual.
newTreeMapConfiguration ::
  TreeMapConfiguration
newTreeMapConfiguration =
  TreeMapConfiguration'
    { colorLabelOptions =
        Prelude.Nothing,
      colorScale = Prelude.Nothing,
      dataLabels = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      groupLabelOptions = Prelude.Nothing,
      legend = Prelude.Nothing,
      sizeLabelOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing
    }

-- | The label options (label text, label visibility) for the colors
-- displayed in a tree map.
treeMapConfiguration_colorLabelOptions :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe ChartAxisLabelOptions)
treeMapConfiguration_colorLabelOptions = Lens.lens (\TreeMapConfiguration' {colorLabelOptions} -> colorLabelOptions) (\s@TreeMapConfiguration' {} a -> s {colorLabelOptions = a} :: TreeMapConfiguration)

-- | The color options (gradient color, point of divergence) of a tree map.
treeMapConfiguration_colorScale :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe ColorScale)
treeMapConfiguration_colorScale = Lens.lens (\TreeMapConfiguration' {colorScale} -> colorScale) (\s@TreeMapConfiguration' {} a -> s {colorScale = a} :: TreeMapConfiguration)

-- | The options that determine if visual data labels are displayed.
treeMapConfiguration_dataLabels :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe DataLabelOptions)
treeMapConfiguration_dataLabels = Lens.lens (\TreeMapConfiguration' {dataLabels} -> dataLabels) (\s@TreeMapConfiguration' {} a -> s {dataLabels = a} :: TreeMapConfiguration)

-- | The field wells of the visual.
treeMapConfiguration_fieldWells :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe TreeMapFieldWells)
treeMapConfiguration_fieldWells = Lens.lens (\TreeMapConfiguration' {fieldWells} -> fieldWells) (\s@TreeMapConfiguration' {} a -> s {fieldWells = a} :: TreeMapConfiguration)

-- | The label options (label text, label visibility) of the groups that are
-- displayed in a tree map.
treeMapConfiguration_groupLabelOptions :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe ChartAxisLabelOptions)
treeMapConfiguration_groupLabelOptions = Lens.lens (\TreeMapConfiguration' {groupLabelOptions} -> groupLabelOptions) (\s@TreeMapConfiguration' {} a -> s {groupLabelOptions = a} :: TreeMapConfiguration)

-- | The legend display setup of the visual.
treeMapConfiguration_legend :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe LegendOptions)
treeMapConfiguration_legend = Lens.lens (\TreeMapConfiguration' {legend} -> legend) (\s@TreeMapConfiguration' {} a -> s {legend = a} :: TreeMapConfiguration)

-- | The label options (label text, label visibility) of the sizes that are
-- displayed in a tree map.
treeMapConfiguration_sizeLabelOptions :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe ChartAxisLabelOptions)
treeMapConfiguration_sizeLabelOptions = Lens.lens (\TreeMapConfiguration' {sizeLabelOptions} -> sizeLabelOptions) (\s@TreeMapConfiguration' {} a -> s {sizeLabelOptions = a} :: TreeMapConfiguration)

-- | The sort configuration of a tree map.
treeMapConfiguration_sortConfiguration :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe TreeMapSortConfiguration)
treeMapConfiguration_sortConfiguration = Lens.lens (\TreeMapConfiguration' {sortConfiguration} -> sortConfiguration) (\s@TreeMapConfiguration' {} a -> s {sortConfiguration = a} :: TreeMapConfiguration)

-- | The tooltip display setup of the visual.
treeMapConfiguration_tooltip :: Lens.Lens' TreeMapConfiguration (Prelude.Maybe TooltipOptions)
treeMapConfiguration_tooltip = Lens.lens (\TreeMapConfiguration' {tooltip} -> tooltip) (\s@TreeMapConfiguration' {} a -> s {tooltip = a} :: TreeMapConfiguration)

instance Data.FromJSON TreeMapConfiguration where
  parseJSON =
    Data.withObject
      "TreeMapConfiguration"
      ( \x ->
          TreeMapConfiguration'
            Prelude.<$> (x Data..:? "ColorLabelOptions")
            Prelude.<*> (x Data..:? "ColorScale")
            Prelude.<*> (x Data..:? "DataLabels")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "GroupLabelOptions")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "SizeLabelOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
      )

instance Prelude.Hashable TreeMapConfiguration where
  hashWithSalt _salt TreeMapConfiguration' {..} =
    _salt `Prelude.hashWithSalt` colorLabelOptions
      `Prelude.hashWithSalt` colorScale
      `Prelude.hashWithSalt` dataLabels
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` groupLabelOptions
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` sizeLabelOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip

instance Prelude.NFData TreeMapConfiguration where
  rnf TreeMapConfiguration' {..} =
    Prelude.rnf colorLabelOptions
      `Prelude.seq` Prelude.rnf colorScale
      `Prelude.seq` Prelude.rnf dataLabels
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf groupLabelOptions
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf sizeLabelOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip

instance Data.ToJSON TreeMapConfiguration where
  toJSON TreeMapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ColorLabelOptions" Data..=)
              Prelude.<$> colorLabelOptions,
            ("ColorScale" Data..=) Prelude.<$> colorScale,
            ("DataLabels" Data..=) Prelude.<$> dataLabels,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("GroupLabelOptions" Data..=)
              Prelude.<$> groupLabelOptions,
            ("Legend" Data..=) Prelude.<$> legend,
            ("SizeLabelOptions" Data..=)
              Prelude.<$> sizeLabelOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip
          ]
      )
