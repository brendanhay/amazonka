{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterSnapshotCopyStatus
  ( ClusterSnapshotCopyStatus (..)
  -- * Smart constructor
  , mkClusterSnapshotCopyStatus
  -- * Lenses
  , cscsDestinationRegion
  , cscsManualSnapshotRetentionPeriod
  , cscsRetentionPeriod
  , cscsSnapshotCopyGrantName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /See:/ 'mkClusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { destinationRegion :: Core.Maybe Core.Text
    -- ^ The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
  , manualSnapshotRetentionPeriod :: Core.Maybe Core.Int
    -- ^ The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
  , retentionPeriod :: Core.Maybe Core.Integer
    -- ^ The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
  , snapshotCopyGrantName :: Core.Maybe Core.Text
    -- ^ The name of the snapshot copy grant.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSnapshotCopyStatus' value with any optional fields omitted.
mkClusterSnapshotCopyStatus
    :: ClusterSnapshotCopyStatus
mkClusterSnapshotCopyStatus
  = ClusterSnapshotCopyStatus'{destinationRegion = Core.Nothing,
                               manualSnapshotRetentionPeriod = Core.Nothing,
                               retentionPeriod = Core.Nothing,
                               snapshotCopyGrantName = Core.Nothing}

-- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsDestinationRegion :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Text)
cscsDestinationRegion = Lens.field @"destinationRegion"
{-# INLINEABLE cscsDestinationRegion #-}
{-# DEPRECATED destinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead"  #-}

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely. 
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsManualSnapshotRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Int)
cscsManualSnapshotRetentionPeriod = Lens.field @"manualSnapshotRetentionPeriod"
{-# INLINEABLE cscsManualSnapshotRetentionPeriod #-}
{-# DEPRECATED manualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead"  #-}

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Integer)
cscsRetentionPeriod = Lens.field @"retentionPeriod"
{-# INLINEABLE cscsRetentionPeriod #-}
{-# DEPRECATED retentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead"  #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsSnapshotCopyGrantName :: Lens.Lens' ClusterSnapshotCopyStatus (Core.Maybe Core.Text)
cscsSnapshotCopyGrantName = Lens.field @"snapshotCopyGrantName"
{-# INLINEABLE cscsSnapshotCopyGrantName #-}
{-# DEPRECATED snapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead"  #-}

instance Core.FromXML ClusterSnapshotCopyStatus where
        parseXML x
          = ClusterSnapshotCopyStatus' Core.<$>
              (x Core..@? "DestinationRegion") Core.<*>
                x Core..@? "ManualSnapshotRetentionPeriod"
                Core.<*> x Core..@? "RetentionPeriod"
                Core.<*> x Core..@? "SnapshotCopyGrantName"
