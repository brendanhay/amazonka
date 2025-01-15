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
-- Module      : Amazonka.Snowball.Types.ClusterListEntry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ClusterListEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Snowball.Types.ClusterState

-- | Contains a cluster\'s state, a cluster\'s ID, and other important
-- information.
--
-- /See:/ 'newClusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { -- | The 39-character ID for the cluster that you want to list, for example
    -- @CID123e4567-e89b-12d3-a456-426655440000@.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The current state of this cluster. For information about the state of a
    -- specific node, see JobListEntry$JobState.
    clusterState :: Prelude.Maybe ClusterState,
    -- | The creation date for this cluster.
    creationDate :: Prelude.Maybe Data.POSIX,
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
-- 'clusterId', 'clusterListEntry_clusterId' - The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
--
-- 'clusterState', 'clusterListEntry_clusterState' - The current state of this cluster. For information about the state of a
-- specific node, see JobListEntry$JobState.
--
-- 'creationDate', 'clusterListEntry_creationDate' - The creation date for this cluster.
--
-- 'description', 'clusterListEntry_description' - Defines an optional description of the cluster, for example
-- @Environmental Data Cluster-01@.
newClusterListEntry ::
  ClusterListEntry
newClusterListEntry =
  ClusterListEntry'
    { clusterId = Prelude.Nothing,
      clusterState = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The 39-character ID for the cluster that you want to list, for example
-- @CID123e4567-e89b-12d3-a456-426655440000@.
clusterListEntry_clusterId :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.Text)
clusterListEntry_clusterId = Lens.lens (\ClusterListEntry' {clusterId} -> clusterId) (\s@ClusterListEntry' {} a -> s {clusterId = a} :: ClusterListEntry)

-- | The current state of this cluster. For information about the state of a
-- specific node, see JobListEntry$JobState.
clusterListEntry_clusterState :: Lens.Lens' ClusterListEntry (Prelude.Maybe ClusterState)
clusterListEntry_clusterState = Lens.lens (\ClusterListEntry' {clusterState} -> clusterState) (\s@ClusterListEntry' {} a -> s {clusterState = a} :: ClusterListEntry)

-- | The creation date for this cluster.
clusterListEntry_creationDate :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.UTCTime)
clusterListEntry_creationDate = Lens.lens (\ClusterListEntry' {creationDate} -> creationDate) (\s@ClusterListEntry' {} a -> s {creationDate = a} :: ClusterListEntry) Prelude.. Lens.mapping Data._Time

-- | Defines an optional description of the cluster, for example
-- @Environmental Data Cluster-01@.
clusterListEntry_description :: Lens.Lens' ClusterListEntry (Prelude.Maybe Prelude.Text)
clusterListEntry_description = Lens.lens (\ClusterListEntry' {description} -> description) (\s@ClusterListEntry' {} a -> s {description = a} :: ClusterListEntry)

instance Data.FromJSON ClusterListEntry where
  parseJSON =
    Data.withObject
      "ClusterListEntry"
      ( \x ->
          ClusterListEntry'
            Prelude.<$> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..:? "ClusterState")
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable ClusterListEntry where
  hashWithSalt _salt ClusterListEntry' {..} =
    _salt
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` clusterState
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` description

instance Prelude.NFData ClusterListEntry where
  rnf ClusterListEntry' {..} =
    Prelude.rnf clusterId `Prelude.seq`
      Prelude.rnf clusterState `Prelude.seq`
        Prelude.rnf creationDate `Prelude.seq`
          Prelude.rnf description
