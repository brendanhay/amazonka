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
-- Module      : Amazonka.QuickSight.Types.FilledMapConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.FilledMapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FilledMapFieldWells
import Amazonka.QuickSight.Types.FilledMapSortConfiguration
import Amazonka.QuickSight.Types.GeospatialMapStyleOptions
import Amazonka.QuickSight.Types.GeospatialWindowOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.TooltipOptions

-- | The configuration for a @FilledMapVisual@.
--
-- /See:/ 'newFilledMapConfiguration' smart constructor.
data FilledMapConfiguration = FilledMapConfiguration'
  { -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe FilledMapFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The map style options of the filled map visual.
    mapStyleOptions :: Prelude.Maybe GeospatialMapStyleOptions,
    -- | The sort configuration of a @FilledMapVisual@.
    sortConfiguration :: Prelude.Maybe FilledMapSortConfiguration,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    -- | The window options of the filled map visual.
    windowOptions :: Prelude.Maybe GeospatialWindowOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilledMapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldWells', 'filledMapConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'filledMapConfiguration_legend' - The legend display setup of the visual.
--
-- 'mapStyleOptions', 'filledMapConfiguration_mapStyleOptions' - The map style options of the filled map visual.
--
-- 'sortConfiguration', 'filledMapConfiguration_sortConfiguration' - The sort configuration of a @FilledMapVisual@.
--
-- 'tooltip', 'filledMapConfiguration_tooltip' - The tooltip display setup of the visual.
--
-- 'windowOptions', 'filledMapConfiguration_windowOptions' - The window options of the filled map visual.
newFilledMapConfiguration ::
  FilledMapConfiguration
newFilledMapConfiguration =
  FilledMapConfiguration'
    { fieldWells =
        Prelude.Nothing,
      legend = Prelude.Nothing,
      mapStyleOptions = Prelude.Nothing,
      sortConfiguration = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      windowOptions = Prelude.Nothing
    }

-- | The field wells of the visual.
filledMapConfiguration_fieldWells :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe FilledMapFieldWells)
filledMapConfiguration_fieldWells = Lens.lens (\FilledMapConfiguration' {fieldWells} -> fieldWells) (\s@FilledMapConfiguration' {} a -> s {fieldWells = a} :: FilledMapConfiguration)

-- | The legend display setup of the visual.
filledMapConfiguration_legend :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe LegendOptions)
filledMapConfiguration_legend = Lens.lens (\FilledMapConfiguration' {legend} -> legend) (\s@FilledMapConfiguration' {} a -> s {legend = a} :: FilledMapConfiguration)

-- | The map style options of the filled map visual.
filledMapConfiguration_mapStyleOptions :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe GeospatialMapStyleOptions)
filledMapConfiguration_mapStyleOptions = Lens.lens (\FilledMapConfiguration' {mapStyleOptions} -> mapStyleOptions) (\s@FilledMapConfiguration' {} a -> s {mapStyleOptions = a} :: FilledMapConfiguration)

-- | The sort configuration of a @FilledMapVisual@.
filledMapConfiguration_sortConfiguration :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe FilledMapSortConfiguration)
filledMapConfiguration_sortConfiguration = Lens.lens (\FilledMapConfiguration' {sortConfiguration} -> sortConfiguration) (\s@FilledMapConfiguration' {} a -> s {sortConfiguration = a} :: FilledMapConfiguration)

-- | The tooltip display setup of the visual.
filledMapConfiguration_tooltip :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe TooltipOptions)
filledMapConfiguration_tooltip = Lens.lens (\FilledMapConfiguration' {tooltip} -> tooltip) (\s@FilledMapConfiguration' {} a -> s {tooltip = a} :: FilledMapConfiguration)

-- | The window options of the filled map visual.
filledMapConfiguration_windowOptions :: Lens.Lens' FilledMapConfiguration (Prelude.Maybe GeospatialWindowOptions)
filledMapConfiguration_windowOptions = Lens.lens (\FilledMapConfiguration' {windowOptions} -> windowOptions) (\s@FilledMapConfiguration' {} a -> s {windowOptions = a} :: FilledMapConfiguration)

instance Data.FromJSON FilledMapConfiguration where
  parseJSON =
    Data.withObject
      "FilledMapConfiguration"
      ( \x ->
          FilledMapConfiguration'
            Prelude.<$> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "MapStyleOptions")
            Prelude.<*> (x Data..:? "SortConfiguration")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "WindowOptions")
      )

instance Prelude.Hashable FilledMapConfiguration where
  hashWithSalt _salt FilledMapConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` mapStyleOptions
      `Prelude.hashWithSalt` sortConfiguration
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` windowOptions

instance Prelude.NFData FilledMapConfiguration where
  rnf FilledMapConfiguration' {..} =
    Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf mapStyleOptions
      `Prelude.seq` Prelude.rnf sortConfiguration
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf windowOptions

instance Data.ToJSON FilledMapConfiguration where
  toJSON FilledMapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("MapStyleOptions" Data..=)
              Prelude.<$> mapStyleOptions,
            ("SortConfiguration" Data..=)
              Prelude.<$> sortConfiguration,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("WindowOptions" Data..=) Prelude.<$> windowOptions
          ]
      )
