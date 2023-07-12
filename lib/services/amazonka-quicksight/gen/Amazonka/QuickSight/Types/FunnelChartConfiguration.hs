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
-- Module      : Amazonka.QuickSight.Types.FunnelChartConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FunnelChartConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ChartAxisLabelOptions
import Amazonka.QuickSight.Types.FunnelChartDataLabelOptions
import Amazonka.QuickSight.Types.FunnelChartFieldWells
import Amazonka.QuickSight.Types.FunnelChartSortConfiguration
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @FunnelChartVisual@.
--
-- /See:/ 'newFunnelChartConfiguration' smart constructor.
data FunnelChartConfiguration = FunnelChartConfiguration'
  { -- | The label options of the categories that are displayed in a
    -- @FunnelChartVisual@.
    categoryLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The options that determine the presentation of the data labels.
    dataLabelOptions :: Prelude.Maybe FunnelChartDataLabelOptions,
    -- | The field well configuration of a @FunnelChartVisual@.
    fieldWells :: Prelude.Maybe FunnelChartFieldWells,
    -- | The sort configuration of a @FunnelChartVisual@.
    sortConfiguration :: Prelude.Maybe FunnelChartSortConfiguration,
    -- | The tooltip configuration of a @FunnelChartVisual@.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The label options for the values that are displayed in a
    -- @FunnelChartVisual@.
    valueLabelOptions :: Prelude.Maybe ChartAxisLabelOptions,
    -- | The visual palette configuration of a @FunnelChartVisual@.
    visualPalette :: Prelude.Maybe VisualPalette
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunnelChartConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoryLabelOptions', 'funnelChartConfiguration_categoryLabelOptions' - The label options of the categories that are displayed in a
-- @FunnelChartVisual@.
--
-- 'dataLabelOptions', 'funnelChartConfiguration_dataLabelOptions' - The options that determine the presentation of the data labels.
--
-- 'fieldWells', 'funnelChartConfiguration_fieldWells' - The field well configuration of a @FunnelChartVisual@.
--
-- 'sortConfiguration', 'funnelChartConfiguration_sortConfiguration' - The sort configuration of a @FunnelChartVisual@.
--
-- 'tooltip', 'funnelChartConfiguration_tooltip' - The tooltip configuration of a @FunnelChartVisual@.
--
-- 'valueLabelOptions', 'funnelChartConfiguration_valueLabelOptions' - The label options for the values that are displayed in a
-- @FunnelChartVisual@.
--
-- 'visualPalette', 'funnelChartConfiguration_visualPalette' - The visual palette configuration of a @FunnelChartVisual@.
newFunnelChartConfiguration ::
  FunnelChartConfiguration
newFunnelChartConfiguration =
  FunnelChartConfiguration'
    { categoryLabelOptions =
        Prelude.Nothing,
      dataLabelOptions = Prelude.Nothing,
      fieldWells = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      valueLabelOptions = Prelude.Nothing,
      visualPalette = Prelude.Nothing
    }

-- | The label options of the categories that are displayed in a
-- @FunnelChartVisual@.
funnelChartConfiguration_categoryLabelOptions :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
funnelChartConfiguration_categoryLabelOptions = Lens.lens (\FunnelChartConfiguration' {categoryLabelOptions} -> categoryLabelOptions) (\s@FunnelChartConfiguration' {} a -> s {categoryLabelOptions = a} :: FunnelChartConfiguration)

-- | The options that determine the presentation of the data labels.
funnelChartConfiguration_dataLabelOptions :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe FunnelChartDataLabelOptions)
funnelChartConfiguration_dataLabelOptions = Lens.lens (\FunnelChartConfiguration' {dataLabelOptions} -> dataLabelOptions) (\s@FunnelChartConfiguration' {} a -> s {dataLabelOptions = a} :: FunnelChartConfiguration)

-- | The field well configuration of a @FunnelChartVisual@.
funnelChartConfiguration_fieldWells :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe FunnelChartFieldWells)
funnelChartConfiguration_fieldWells = Lens.lens (\FunnelChartConfiguration' {fieldWells} -> fieldWells) (\s@FunnelChartConfiguration' {} a -> s {fieldWells = a} :: FunnelChartConfiguration)

-- | The sort configuration of a @FunnelChartVisual@.
funnelChartConfiguration_sortConfiguration :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe FunnelChartSortConfiguration)
funnelChartConfiguration_sortConfiguration = Lens.lens (\FunnelChartConfiguration' {sortConfiguration} -> sortConfiguration) (\s@FunnelChartConfiguration' {} a -> s {sortConfiguration = a} :: FunnelChartConfiguration)

-- | The tooltip configuration of a @FunnelChartVisual@.
funnelChartConfiguration_tooltip :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe TooltipOptions)
funnelChartConfiguration_tooltip = Lens.lens (\FunnelChartConfiguration' {tooltip} -> tooltip) (\s@FunnelChartConfiguration' {} a -> s {tooltip = a} :: FunnelChartConfiguration)

-- | The label options for the values that are displayed in a
-- @FunnelChartVisual@.
funnelChartConfiguration_valueLabelOptions :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe ChartAxisLabelOptions)
funnelChartConfiguration_valueLabelOptions = Lens.lens (\FunnelChartConfiguration' {valueLabelOptions} -> valueLabelOptions) (\s@FunnelChartConfiguration' {} a -> s {valueLabelOptions = a} :: FunnelChartConfiguration)

-- | The visual palette configuration of a @FunnelChartVisual@.
funnelChartConfiguration_visualPalette :: Lens.Lens' FunnelChartConfiguration (Prelude.Maybe VisualPalette)
funnelChartConfiguration_visualPalette = Lens.lens (\FunnelChartConfiguration' {visualPalette} -> visualPalette) (\s@FunnelChartConfiguration' {} a -> s {visualPalette = a} :: FunnelChartConfiguration)

instance Data.FromJSON FunnelChartConfiguration where
  parseJSON =
    Data.withObject
      "FunnelChartConfiguration"
      ( \x ->
          FunnelChartConfiguration'
            Prelude.<$> (x Data..:? "CategoryLabelOptions")
            Prelude.<*> (x Data..:? "DataLabelOptions")
            Prelude.<*> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "ValueLabelOptions")
            Prelude.<*> (x Data..:? "VisualPalette")
      )

instance Prelude.Hashable FunnelChartConfiguration where
  hashWithSalt _salt FunnelChartConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` categoryLabelOptions
      `Prelude.hashWithSalt` dataLabelOptions
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` valueLabelOptions
      `Prelude.hashWithSalt` visualPalette

instance Prelude.NFData FunnelChartConfiguration where
  rnf FunnelChartConfiguration' {..} =
    Prelude.rnf categoryLabelOptions
      `Prelude.seq` Prelude.rnf dataLabelOptions
      `Prelude.seq` Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf valueLabelOptions
      `Prelude.seq` Prelude.rnf visualPalette

instance Data.ToJSON FunnelChartConfiguration where
  toJSON FunnelChartConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoryLabelOptions" Data..=)
              Prelude.<$> categoryLabelOptions,
            ("DataLabelOptions" Data..=)
              Prelude.<$> dataLabelOptions,
            ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("ValueLabelOptions" Data..=)
              Prelude.<$> valueLabelOptions,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette
          ]
      )
