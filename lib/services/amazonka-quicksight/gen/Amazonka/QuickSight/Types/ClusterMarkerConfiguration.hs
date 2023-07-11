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
-- Module      : Amazonka.QuickSight.Types.ClusterMarkerConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ClusterMarkerConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ClusterMarker

-- | The cluster marker configuration of the geospatial map selected point
-- style.
--
-- /See:/ 'newClusterMarkerConfiguration' smart constructor.
data ClusterMarkerConfiguration = ClusterMarkerConfiguration'
  { -- | The cluster marker that is a part of the cluster marker configuration
    clusterMarker :: Prelude.Maybe ClusterMarker
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterMarkerConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterMarker', 'clusterMarkerConfiguration_clusterMarker' - The cluster marker that is a part of the cluster marker configuration
newClusterMarkerConfiguration ::
  ClusterMarkerConfiguration
newClusterMarkerConfiguration =
  ClusterMarkerConfiguration'
    { clusterMarker =
        Prelude.Nothing
    }

-- | The cluster marker that is a part of the cluster marker configuration
clusterMarkerConfiguration_clusterMarker :: Lens.Lens' ClusterMarkerConfiguration (Prelude.Maybe ClusterMarker)
clusterMarkerConfiguration_clusterMarker = Lens.lens (\ClusterMarkerConfiguration' {clusterMarker} -> clusterMarker) (\s@ClusterMarkerConfiguration' {} a -> s {clusterMarker = a} :: ClusterMarkerConfiguration)

instance Data.FromJSON ClusterMarkerConfiguration where
  parseJSON =
    Data.withObject
      "ClusterMarkerConfiguration"
      ( \x ->
          ClusterMarkerConfiguration'
            Prelude.<$> (x Data..:? "ClusterMarker")
      )

instance Prelude.Hashable ClusterMarkerConfiguration where
  hashWithSalt _salt ClusterMarkerConfiguration' {..} =
    _salt `Prelude.hashWithSalt` clusterMarker

instance Prelude.NFData ClusterMarkerConfiguration where
  rnf ClusterMarkerConfiguration' {..} =
    Prelude.rnf clusterMarker

instance Data.ToJSON ClusterMarkerConfiguration where
  toJSON ClusterMarkerConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClusterMarker" Data..=)
              Prelude.<$> clusterMarker
          ]
      )
