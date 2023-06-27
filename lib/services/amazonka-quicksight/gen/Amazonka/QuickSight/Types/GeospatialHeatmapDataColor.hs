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
-- Module      : Amazonka.QuickSight.Types.GeospatialHeatmapDataColor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialHeatmapDataColor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The color to be used in the heatmap point style.
--
-- /See:/ 'newGeospatialHeatmapDataColor' smart constructor.
data GeospatialHeatmapDataColor = GeospatialHeatmapDataColor'
  { -- | The hex color to be used in the heatmap point style.
    color :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialHeatmapDataColor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'color', 'geospatialHeatmapDataColor_color' - The hex color to be used in the heatmap point style.
newGeospatialHeatmapDataColor ::
  -- | 'color'
  Prelude.Text ->
  GeospatialHeatmapDataColor
newGeospatialHeatmapDataColor pColor_ =
  GeospatialHeatmapDataColor' {color = pColor_}

-- | The hex color to be used in the heatmap point style.
geospatialHeatmapDataColor_color :: Lens.Lens' GeospatialHeatmapDataColor Prelude.Text
geospatialHeatmapDataColor_color = Lens.lens (\GeospatialHeatmapDataColor' {color} -> color) (\s@GeospatialHeatmapDataColor' {} a -> s {color = a} :: GeospatialHeatmapDataColor)

instance Data.FromJSON GeospatialHeatmapDataColor where
  parseJSON =
    Data.withObject
      "GeospatialHeatmapDataColor"
      ( \x ->
          GeospatialHeatmapDataColor'
            Prelude.<$> (x Data..: "Color")
      )

instance Prelude.Hashable GeospatialHeatmapDataColor where
  hashWithSalt _salt GeospatialHeatmapDataColor' {..} =
    _salt `Prelude.hashWithSalt` color

instance Prelude.NFData GeospatialHeatmapDataColor where
  rnf GeospatialHeatmapDataColor' {..} =
    Prelude.rnf color

instance Data.ToJSON GeospatialHeatmapDataColor where
  toJSON GeospatialHeatmapDataColor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Color" Data..= color)]
      )
