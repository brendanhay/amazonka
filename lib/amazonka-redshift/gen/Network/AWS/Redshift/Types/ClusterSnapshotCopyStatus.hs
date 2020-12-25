{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
  ( ClusterSnapshotCopyStatus (..),

    -- * Smart constructor
    mkClusterSnapshotCopyStatus,

    -- * Lenses
    cscsDestinationRegion,
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsSnapshotCopyGrantName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.DestinationRegion as Types
import qualified Network.AWS.Redshift.Types.SnapshotCopyGrantName as Types

-- | Returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /See:/ 'mkClusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { -- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
    destinationRegion :: Core.Maybe Types.DestinationRegion,
    -- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Core.Maybe Core.Int,
    -- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
    retentionPeriod :: Core.Maybe Core.Integer,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Core.Maybe Types.SnapshotCopyGrantName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSnapshotCopyStatus' value with any optional fields omitted.
mkClusterSnapshotCopyStatus ::
  ClusterSnapshotCopyStatus
mkClusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { destinationRegion = Core.Nothing,
      manualSnapshotRetentionPeriod = Core.Nothing,
      retentionPeriod = Core.Nothing,
      snapshotCopyGrantName = Core.Nothing
    }

-- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsDestinationRegion :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Types.DestinationRegion)
cscsDestinationRegion = Lens.field @"destinationRegion"
{-# DEPRECATED cscsDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsManualSnapshotRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Int)
cscsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# DEPRECATED cscsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Integer)
cscsRetentionPeriod = Lens.field @"retentionPeriod"
{-# DEPRECATED cscsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsSnapshotCopyGrantName :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Types.SnapshotCopyGrantName)
cscsSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# DEPRECATED cscsSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Core.FromXML ClusterSnapshotCopyStatus where
  parseXML x =
    ClusterSnapshotCopyStatus'
      Core.<$> (x Core..@? "DestinationRegion")
      Core.<*> (x Core..@? "ManualSnapshotRetentionPeriod")
      Core.<*> (x Core..@? "RetentionPeriod")
      Core.<*> (x Core..@? "SnapshotCopyGrantName")
