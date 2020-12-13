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
    cscsManualSnapshotRetentionPeriod,
    cscsRetentionPeriod,
    cscsDestinationRegion,
    cscsSnapshotCopyGrantName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Returns the destination region and retention period that are configured for cross-region snapshot copy.
--
-- /See:/ 'mkClusterSnapshotCopyStatus' smart constructor.
data ClusterSnapshotCopyStatus = ClusterSnapshotCopyStatus'
  { -- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
    retentionPeriod :: Lude.Maybe Lude.Integer,
    -- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
    destinationRegion :: Lude.Maybe Lude.Text,
    -- | The name of the snapshot copy grant.
    snapshotCopyGrantName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterSnapshotCopyStatus' with the minimum fields required to make a request.
--
-- * 'manualSnapshotRetentionPeriod' - The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'retentionPeriod' - The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
-- * 'destinationRegion' - The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant.
mkClusterSnapshotCopyStatus ::
  ClusterSnapshotCopyStatus
mkClusterSnapshotCopyStatus =
  ClusterSnapshotCopyStatus'
    { manualSnapshotRetentionPeriod =
        Lude.Nothing,
      retentionPeriod = Lude.Nothing,
      destinationRegion = Lude.Nothing,
      snapshotCopyGrantName = Lude.Nothing
    }

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsManualSnapshotRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Lude.Maybe Lude.Int)
cscsManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: ClusterSnapshotCopyStatus -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: ClusterSnapshotCopyStatus)
{-# DEPRECATED cscsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The number of days that automated snapshots are retained in the destination region after they are copied from a source region.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsRetentionPeriod :: Lens.Lens' ClusterSnapshotCopyStatus (Lude.Maybe Lude.Integer)
cscsRetentionPeriod = Lens.lens (retentionPeriod :: ClusterSnapshotCopyStatus -> Lude.Maybe Lude.Integer) (\s a -> s {retentionPeriod = a} :: ClusterSnapshotCopyStatus)
{-# DEPRECATED cscsRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The destination region that snapshots are automatically copied to when cross-region snapshot copy is enabled.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsDestinationRegion :: Lens.Lens' ClusterSnapshotCopyStatus (Lude.Maybe Lude.Text)
cscsDestinationRegion = Lens.lens (destinationRegion :: ClusterSnapshotCopyStatus -> Lude.Maybe Lude.Text) (\s a -> s {destinationRegion = a} :: ClusterSnapshotCopyStatus)
{-# DEPRECATED cscsDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The name of the snapshot copy grant.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscsSnapshotCopyGrantName :: Lens.Lens' ClusterSnapshotCopyStatus (Lude.Maybe Lude.Text)
cscsSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: ClusterSnapshotCopyStatus -> Lude.Maybe Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: ClusterSnapshotCopyStatus)
{-# DEPRECATED cscsSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Lude.FromXML ClusterSnapshotCopyStatus where
  parseXML x =
    ClusterSnapshotCopyStatus'
      Lude.<$> (x Lude..@? "ManualSnapshotRetentionPeriod")
      Lude.<*> (x Lude..@? "RetentionPeriod")
      Lude.<*> (x Lude..@? "DestinationRegion")
      Lude.<*> (x Lude..@? "SnapshotCopyGrantName")
