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
-- Module      : Network.AWS.Snowball.Types.ClusterListEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterListEntry where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Snowball.Types.ClusterState

-- | Contains a cluster\'s state, a cluster\'s ID, and other important
-- information.
--
-- /See:/ 'newClusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { -- | The current state of this cluster. For information about the state of a
    -- specific node, see JobListEntry$JobState.
    clusterState :: Prelude.Maybe ClusterState,
    -- | The 39-character ID for the cluster that you want to list, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The creation date for this cluster.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | Defines an optional description of the cluster, for example
    -- @Environmental Data Cluster-01@.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterListEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterState', 'clusterListEntry_clusterState' - The current state of this cluster. For information about the state of a
-- specific node, see JobListEntry$JobState.
--
-- 'clusterId', 'clusterListEntry_clusterId' - The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'creationDate', 'clusterListEntry_creationDate' - The creation date for this cluster.
--
-- 'description', 'clusterListEntry_description' - Defines an optional description of the cluster, for example
-- @Environmental Data Cluster-01@.
newClusterListEntry ::
  ClusterListEntry
newClusterListEntry =
  ClusterListEntry'
    { clusterState = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current state of this cluster. For information about the state of a
-- specific node, see JobListEntry$JobState.
clusterListEntry_clusterState :: Lens.Lens' ClusterListEntry (Prelude.Maybe ClusterState)
clusterListEntry_clusterState = Lens.lens (\ClusterListEntry' {clusterState} -> clusterState) (\s@ClusterListEntry' {} a -> s {clusterState = a} :: ClusterListEntry)

-- | The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
clusterListEntry_clusterId :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.Text)
clusterListEntry_clusterId = Lens.lens (\ClusterListEntry' {clusterId} -> clusterId) (\s@ClusterListEntry' {} a -> s {clusterId = a} :: ClusterListEntry)

-- | The creation date for this cluster.
clusterListEntry_creationDate :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.UTCTime)
clusterListEntry_creationDate = Lens.lens (\ClusterListEntry' {creationDate} -> creationDate) (\s@ClusterListEntry' {} a -> s {creationDate = a} :: ClusterListEntry) Prelude.. Lens.mapping Core._Time

-- | Defines an optional description of the cluster, for example
-- @Environmental Data Cluster-01@.
clusterListEntry_description :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.Text)
clusterListEntry_description = Lens.lens (\ClusterListEntry' {description} -> description) (\s@ClusterListEntry' {} a -> s {description = a} :: ClusterListEntry)

instance Core.FromJSON ClusterListEntry where
  parseJSON =
    Core.withObject
      "ClusterListEntry"
      ( \x ->
          ClusterListEntry'
            Prelude.<$> (x Core..:? "ClusterState")
            Prelude.<*> (x Core..:? "ClusterId")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable ClusterListEntry

instance Prelude.NFData ClusterListEntry
