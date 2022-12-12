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
-- Module      : Amazonka.QuickSight.Types.GeospatialMapConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialMapConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.GeospatialMapFieldWells
import Amazonka.QuickSight.Types.GeospatialMapStyleOptions
import Amazonka.QuickSight.Types.GeospatialPointStyleOptions
import Amazonka.QuickSight.Types.GeospatialWindowOptions
import Amazonka.QuickSight.Types.LegendOptions
import Amazonka.QuickSight.Types.TooltipOptions
import Amazonka.QuickSight.Types.VisualPalette

-- | The configuration of a @GeospatialMapVisual@.
--
-- /See:/ 'newGeospatialMapConfiguration' smart constructor.
data GeospatialMapConfiguration = GeospatialMapConfiguration'
  { -- | The field wells of the visual.
    fieldWells :: Prelude.Maybe GeospatialMapFieldWells,
    -- | The legend display setup of the visual.
    legend :: Prelude.Maybe LegendOptions,
    -- | The map style options of the geospatial map.
    mapStyleOptions :: Prelude.Maybe GeospatialMapStyleOptions,
    -- | The point style options of the geospatial map.
    pointStyleOptions :: Prelude.Maybe GeospatialPointStyleOptions,
    -- | The tooltip display setup of the visual.
    tooltip :: Prelude.Maybe TooltipOptions,
    visualPalette :: Prelude.Maybe VisualPalette,
    -- | The window options of the geospatial map.
    windowOptions :: Prelude.Maybe GeospatialWindowOptions
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialMapConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fieldWells', 'geospatialMapConfiguration_fieldWells' - The field wells of the visual.
--
-- 'legend', 'geospatialMapConfiguration_legend' - The legend display setup of the visual.
--
-- 'mapStyleOptions', 'geospatialMapConfiguration_mapStyleOptions' - The map style options of the geospatial map.
--
-- 'pointStyleOptions', 'geospatialMapConfiguration_pointStyleOptions' - The point style options of the geospatial map.
--
-- 'tooltip', 'geospatialMapConfiguration_tooltip' - The tooltip display setup of the visual.
--
-- 'visualPalette', 'geospatialMapConfiguration_visualPalette' - Undocumented member.
--
-- 'windowOptions', 'geospatialMapConfiguration_windowOptions' - The window options of the geospatial map.
newGeospatialMapConfiguration ::
  GeospatialMapConfiguration
newGeospatialMapConfiguration =
  GeospatialMapConfiguration'
    { fieldWells =
        Prelude.Nothing,
      legend = Prelude.Nothing,
      mapStyleOptions = Prelude.Nothing,
      pointStyleOptions = Prelude.Nothing,
      tooltip = Prelude.Nothing,
      visualPalette = Prelude.Nothing,
      windowOptions = Prelude.Nothing
    }

-- | The field wells of the visual.
geospatialMapConfiguration_fieldWells :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe GeospatialMapFieldWells)
geospatialMapConfiguration_fieldWells = Lens.lens (\GeospatialMapConfiguration' {fieldWells} -> fieldWells) (\s@GeospatialMapConfiguration' {} a -> s {fieldWells = a} :: GeospatialMapConfiguration)

-- | The legend display setup of the visual.
geospatialMapConfiguration_legend :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe LegendOptions)
geospatialMapConfiguration_legend = Lens.lens (\GeospatialMapConfiguration' {legend} -> legend) (\s@GeospatialMapConfiguration' {} a -> s {legend = a} :: GeospatialMapConfiguration)

-- | The map style options of the geospatial map.
geospatialMapConfiguration_mapStyleOptions :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe GeospatialMapStyleOptions)
geospatialMapConfiguration_mapStyleOptions = Lens.lens (\GeospatialMapConfiguration' {mapStyleOptions} -> mapStyleOptions) (\s@GeospatialMapConfiguration' {} a -> s {mapStyleOptions = a} :: GeospatialMapConfiguration)

-- | The point style options of the geospatial map.
geospatialMapConfiguration_pointStyleOptions :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe GeospatialPointStyleOptions)
geospatialMapConfiguration_pointStyleOptions = Lens.lens (\GeospatialMapConfiguration' {pointStyleOptions} -> pointStyleOptions) (\s@GeospatialMapConfiguration' {} a -> s {pointStyleOptions = a} :: GeospatialMapConfiguration)

-- | The tooltip display setup of the visual.
geospatialMapConfiguration_tooltip :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe TooltipOptions)
geospatialMapConfiguration_tooltip = Lens.lens (\GeospatialMapConfiguration' {tooltip} -> tooltip) (\s@GeospatialMapConfiguration' {} a -> s {tooltip = a} :: GeospatialMapConfiguration)

-- | Undocumented member.
geospatialMapConfiguration_visualPalette :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe VisualPalette)
geospatialMapConfiguration_visualPalette = Lens.lens (\GeospatialMapConfiguration' {visualPalette} -> visualPalette) (\s@GeospatialMapConfiguration' {} a -> s {visualPalette = a} :: GeospatialMapConfiguration)

-- | The window options of the geospatial map.
geospatialMapConfiguration_windowOptions :: Lens.Lens' GeospatialMapConfiguration (Prelude.Maybe GeospatialWindowOptions)
geospatialMapConfiguration_windowOptions = Lens.lens (\GeospatialMapConfiguration' {windowOptions} -> windowOptions) (\s@GeospatialMapConfiguration' {} a -> s {windowOptions = a} :: GeospatialMapConfiguration)

instance Data.FromJSON GeospatialMapConfiguration where
  parseJSON =
    Data.withObject
      "GeospatialMapConfiguration"
      ( \x ->
          GeospatialMapConfiguration'
            Prelude.<$> (x Data..:? "FieldWells")
            Prelude.<*> (x Data..:? "Legend")
            Prelude.<*> (x Data..:? "MapStyleOptions")
            Prelude.<*> (x Data..:? "PointStyleOptions")
            Prelude.<*> (x Data..:? "Tooltip")
            Prelude.<*> (x Data..:? "VisualPalette")
            Prelude.<*> (x Data..:? "WindowOptions")
      )

instance Prelude.Hashable GeospatialMapConfiguration where
  hashWithSalt _salt GeospatialMapConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fieldWells
      `Prelude.hashWithSalt` legend
      `Prelude.hashWithSalt` mapStyleOptions
      `Prelude.hashWithSalt` pointStyleOptions
      `Prelude.hashWithSalt` tooltip
      `Prelude.hashWithSalt` visualPalette
      `Prelude.hashWithSalt` windowOptions

instance Prelude.NFData GeospatialMapConfiguration where
  rnf GeospatialMapConfiguration' {..} =
    Prelude.rnf fieldWells
      `Prelude.seq` Prelude.rnf legend
      `Prelude.seq` Prelude.rnf mapStyleOptions
      `Prelude.seq` Prelude.rnf pointStyleOptions
      `Prelude.seq` Prelude.rnf tooltip
      `Prelude.seq` Prelude.rnf visualPalette
      `Prelude.seq` Prelude.rnf windowOptions

instance Data.ToJSON GeospatialMapConfiguration where
  toJSON GeospatialMapConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FieldWells" Data..=) Prelude.<$> fieldWells,
            ("Legend" Data..=) Prelude.<$> legend,
            ("MapStyleOptions" Data..=)
              Prelude.<$> mapStyleOptions,
            ("PointStyleOptions" Data..=)
              Prelude.<$> pointStyleOptions,
            ("Tooltip" Data..=) Prelude.<$> tooltip,
            ("VisualPalette" Data..=) Prelude.<$> visualPalette,
            ("WindowOptions" Data..=) Prelude.<$> windowOptions
          ]
      )
