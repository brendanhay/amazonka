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
-- Module      : Amazonka.QuickSight.Types.GeospatialPointStyleOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.GeospatialPointStyleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ClusterMarkerConfiguration
import Amazonka.QuickSight.Types.GeospatialSelectedPointStyle

-- | The point style of the geospatial map.
--
-- /See:/ 'newGeospatialPointStyleOptions' smart constructor.
data GeospatialPointStyleOptions = GeospatialPointStyleOptions'
  { -- | The cluster marker configuration of the geospatial point style.
    clusterMarkerConfiguration :: Prelude.Maybe ClusterMarkerConfiguration,
    -- | The selected point styles (point, cluster) of the geospatial map.
    selectedPointStyle :: Prelude.Maybe GeospatialSelectedPointStyle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeospatialPointStyleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterMarkerConfiguration', 'geospatialPointStyleOptions_clusterMarkerConfiguration' - The cluster marker configuration of the geospatial point style.
--
-- 'selectedPointStyle', 'geospatialPointStyleOptions_selectedPointStyle' - The selected point styles (point, cluster) of the geospatial map.
newGeospatialPointStyleOptions ::
  GeospatialPointStyleOptions
newGeospatialPointStyleOptions =
  GeospatialPointStyleOptions'
    { clusterMarkerConfiguration =
        Prelude.Nothing,
      selectedPointStyle = Prelude.Nothing
    }

-- | The cluster marker configuration of the geospatial point style.
geospatialPointStyleOptions_clusterMarkerConfiguration :: Lens.Lens' GeospatialPointStyleOptions (Prelude.Maybe ClusterMarkerConfiguration)
geospatialPointStyleOptions_clusterMarkerConfiguration = Lens.lens (\GeospatialPointStyleOptions' {clusterMarkerConfiguration} -> clusterMarkerConfiguration) (\s@GeospatialPointStyleOptions' {} a -> s {clusterMarkerConfiguration = a} :: GeospatialPointStyleOptions)

-- | The selected point styles (point, cluster) of the geospatial map.
geospatialPointStyleOptions_selectedPointStyle :: Lens.Lens' GeospatialPointStyleOptions (Prelude.Maybe GeospatialSelectedPointStyle)
geospatialPointStyleOptions_selectedPointStyle = Lens.lens (\GeospatialPointStyleOptions' {selectedPointStyle} -> selectedPointStyle) (\s@GeospatialPointStyleOptions' {} a -> s {selectedPointStyle = a} :: GeospatialPointStyleOptions)

instance Data.FromJSON GeospatialPointStyleOptions where
  parseJSON =
    Data.withObject
      "GeospatialPointStyleOptions"
      ( \x ->
          GeospatialPointStyleOptions'
            Prelude.<$> (x Data..:? "ClusterMarkerConfiguration")
            Prelude.<*> (x Data..:? "SelectedPointStyle")
      )

instance Prelude.Hashable GeospatialPointStyleOptions where
  hashWithSalt _salt GeospatialPointStyleOptions' {..} =
    _salt
      `Prelude.hashWithSalt` clusterMarkerConfiguration
      `Prelude.hashWithSalt` selectedPointStyle

instance Prelude.NFData GeospatialPointStyleOptions where
  rnf GeospatialPointStyleOptions' {..} =
    Prelude.rnf clusterMarkerConfiguration
      `Prelude.seq` Prelude.rnf selectedPointStyle

instance Data.ToJSON GeospatialPointStyleOptions where
  toJSON GeospatialPointStyleOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterMarkerConfiguration" Data..=)
              Prelude.<$> clusterMarkerConfiguration,
            ("SelectedPointStyle" Data..=)
              Prelude.<$> selectedPointStyle
          ]
      )
