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
-- Module      : Amazonka.QuickSight.Types.ClusterMarker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ClusterMarker where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SimpleClusterMarker

-- | The cluster marker that is a part of the cluster marker configuration.
--
-- /See:/ 'newClusterMarker' smart constructor.
data ClusterMarker = ClusterMarker'
  { -- | The simple cluster marker of the cluster marker.
    simpleClusterMarker :: Prelude.Maybe SimpleClusterMarker
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterMarker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simpleClusterMarker', 'clusterMarker_simpleClusterMarker' - The simple cluster marker of the cluster marker.
newClusterMarker ::
  ClusterMarker
newClusterMarker =
  ClusterMarker'
    { simpleClusterMarker =
        Prelude.Nothing
    }

-- | The simple cluster marker of the cluster marker.
clusterMarker_simpleClusterMarker :: Lens.Lens' ClusterMarker (Prelude.Maybe SimpleClusterMarker)
clusterMarker_simpleClusterMarker = Lens.lens (\ClusterMarker' {simpleClusterMarker} -> simpleClusterMarker) (\s@ClusterMarker' {} a -> s {simpleClusterMarker = a} :: ClusterMarker)

instance Data.FromJSON ClusterMarker where
  parseJSON =
    Data.withObject
      "ClusterMarker"
      ( \x ->
          ClusterMarker'
            Prelude.<$> (x Data..:? "SimpleClusterMarker")
      )

instance Prelude.Hashable ClusterMarker where
  hashWithSalt _salt ClusterMarker' {..} =
    _salt `Prelude.hashWithSalt` simpleClusterMarker

instance Prelude.NFData ClusterMarker where
  rnf ClusterMarker' {..} =
    Prelude.rnf simpleClusterMarker

instance Data.ToJSON ClusterMarker where
  toJSON ClusterMarker' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SimpleClusterMarker" Data..=)
              Prelude.<$> simpleClusterMarker
          ]
      )
