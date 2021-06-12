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
-- Module      : Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal

-- | Returns the destination region and retention period that are configured
-- for cross-region snapshot copy.
--
-- /See:/ 'newClusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { -- | The destination region that snapshots are automatically copied to when
    -- cross-region snapshot copy is enabled.
    destinationRegion :: Core.Maybe Core.Text,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Core.Text,
    -- | The number of days that automated snapshots are retained in the
    -- destination region after they are copied from a source region. If the
    -- value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The number of days that automated snapshots are retained in the
    -- destination region after they are copied from a source region.
    retentionPeriod :: Core.Maybe Core.Integer
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterSnapshotCopyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationRegion', 'clusterSnapshotCopyStatus_destinationRegion' - The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
--
-- 'snapshotCopyGrantName', 'clusterSnapshotCopyStatus_snapshotCopyGrantName' - The name of the snapshot copy grant.
--
-- 'manualSnapshotRetentionPeriod', 'clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod' - The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- 'retentionPeriod', 'clusterSnapshotCopyStatus_retentionPeriod' - The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region.
newClusterSnapshotCopyStatus ::
  ClusterSnapshotCopyStatus
newClusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { destinationRegion =
        Core.Nothing,
      snapshotCopyGrantName = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      retentionPeriod = Core.Nothing
    }

-- | The destination region that snapshots are automatically copied to when
-- cross-region snapshot copy is enabled.
clusterSnapshotCopyStatus_destinationRegion :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Text)
clusterSnapshotCopyStatus_destinationRegion = Lens.lens (\ClusterSnapshotCopyStatus' {destinationRegion} -> destinationRegion) (\s@ClusterSnapshotCopyStatus' {} a -> s {destinationRegion = a} :: ClusterSnapshotCopyStatus)

-- | The name of the snapshot copy grant.
clusterSnapshotCopyStatus_snapshotCopyGrantName :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Text)
clusterSnapshotCopyStatus_snapshotCopyGrantName = Lens.lens (\ClusterSnapshotCopyStatus' {snapshotCopyGrantName} -> snapshotCopyGrantName) (\s@ClusterSnapshotCopyStatus' {} a -> s {snapshotCopyGrantName = a} :: ClusterSnapshotCopyStatus)

-- | The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region. If the
-- value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Int)
clusterSnapshotCopyStatus_manualSnapshotRetentionPeriod = Lens.lens (\ClusterSnapshotCopyStatus' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@ClusterSnapshotCopyStatus' {} a -> s {manualSnapshotRetentionPeriod = a} :: ClusterSnapshotCopyStatus)

-- | The number of days that automated snapshots are retained in the
-- destination region after they are copied from a source region.
clusterSnapshotCopyStatus_retentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Integer)
clusterSnapshotCopyStatus_retentionPeriod = Lens.lens (\ClusterSnapshotCopyStatus' {retentionPeriod} -> retentionPeriod) (\s@ClusterSnapshotCopyStatus' {} a -> s {retentionPeriod = a} :: ClusterSnapshotCopyStatus)

instance Core.FromXML ClusterSnapshotCopyStatus where
  parseXML x =
    ClusterSnapshotCopyStatus'
      Core.<$> (x Core..@? "DestinationRegion")
      Core.<*> (x Core..@? "SnapshotCopyGrantName")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "RetentionPeriod")

instance Core.Hashable ClusterSnapshotCopyStatus

instance Core.NFData ClusterSnapshotCopyStatus
