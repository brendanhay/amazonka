{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.EnableSnapshotCopy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the automatic copy of snapshots from one region to another region for a specified cluster.
module Network.AWS.Redshift.EnableSnapshotCopy
  ( -- * Creating a request
    EnableSnapshotCopy (..),
    mkEnableSnapshotCopy,

    -- ** Request lenses
    escManualSnapshotRetentionPeriod,
    escClusterIdentifier,
    escRetentionPeriod,
    escDestinationRegion,
    escSnapshotCopyGrantName,

    -- * Destructuring the response
    EnableSnapshotCopyResponse (..),
    mkEnableSnapshotCopyResponse,

    -- ** Response lenses
    escrsCluster,
    escrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkEnableSnapshotCopy' smart constructor.
data EnableSnapshotCopy = EnableSnapshotCopy'
  { -- | The number of days to retain newly copied snapshots in the destination AWS Region after they are copied from the source AWS Region. If the value is -1, the manual snapshot is retained indefinitely.
    --
    -- The value must be either -1 or an integer between 1 and 3,653.
    manualSnapshotRetentionPeriod :: Lude.Maybe Lude.Int,
    -- | The unique identifier of the source cluster to copy snapshots from.
    --
    -- Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
    clusterIdentifier :: Lude.Text,
    -- | The number of days to retain automated snapshots in the destination region after they are copied from the source region.
    --
    -- Default: 7.
    -- Constraints: Must be at least 1 and no more than 35.
    retentionPeriod :: Lude.Maybe Lude.Int,
    -- | The destination AWS Region that you want to copy snapshots to.
    --
    -- Constraints: Must be the name of a valid AWS Region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
    destinationRegion :: Lude.Text,
    -- | The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
    snapshotCopyGrantName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableSnapshotCopy' with the minimum fields required to make a request.
--
-- * 'manualSnapshotRetentionPeriod' - The number of days to retain newly copied snapshots in the destination AWS Region after they are copied from the source AWS Region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- * 'clusterIdentifier' - The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
-- * 'retentionPeriod' - The number of days to retain automated snapshots in the destination region after they are copied from the source region.
--
-- Default: 7.
-- Constraints: Must be at least 1 and no more than 35.
-- * 'destinationRegion' - The destination AWS Region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid AWS Region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
-- * 'snapshotCopyGrantName' - The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
mkEnableSnapshotCopy ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'destinationRegion'
  Lude.Text ->
  EnableSnapshotCopy
mkEnableSnapshotCopy pClusterIdentifier_ pDestinationRegion_ =
  EnableSnapshotCopy'
    { manualSnapshotRetentionPeriod = Lude.Nothing,
      clusterIdentifier = pClusterIdentifier_,
      retentionPeriod = Lude.Nothing,
      destinationRegion = pDestinationRegion_,
      snapshotCopyGrantName = Lude.Nothing
    }

-- | The number of days to retain newly copied snapshots in the destination AWS Region after they are copied from the source AWS Region. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escManualSnapshotRetentionPeriod :: Lens.Lens' EnableSnapshotCopy (Lude.Maybe Lude.Int)
escManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: EnableSnapshotCopy -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: EnableSnapshotCopy)
{-# DEPRECATED escManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The unique identifier of the source cluster to copy snapshots from.
--
-- Constraints: Must be the valid name of an existing cluster that does not already have cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escClusterIdentifier :: Lens.Lens' EnableSnapshotCopy Lude.Text
escClusterIdentifier = Lens.lens (clusterIdentifier :: EnableSnapshotCopy -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: EnableSnapshotCopy)
{-# DEPRECATED escClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of days to retain automated snapshots in the destination region after they are copied from the source region.
--
-- Default: 7.
-- Constraints: Must be at least 1 and no more than 35.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escRetentionPeriod :: Lens.Lens' EnableSnapshotCopy (Lude.Maybe Lude.Int)
escRetentionPeriod = Lens.lens (retentionPeriod :: EnableSnapshotCopy -> Lude.Maybe Lude.Int) (\s a -> s {retentionPeriod = a} :: EnableSnapshotCopy)
{-# DEPRECATED escRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

-- | The destination AWS Region that you want to copy snapshots to.
--
-- Constraints: Must be the name of a valid AWS Region. For more information, see <https://docs.aws.amazon.com/general/latest/gr/rande.html#redshift_region Regions and Endpoints> in the Amazon Web Services General Reference.
--
-- /Note:/ Consider using 'destinationRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escDestinationRegion :: Lens.Lens' EnableSnapshotCopy Lude.Text
escDestinationRegion = Lens.lens (destinationRegion :: EnableSnapshotCopy -> Lude.Text) (\s a -> s {destinationRegion = a} :: EnableSnapshotCopy)
{-# DEPRECATED escDestinationRegion "Use generic-lens or generic-optics with 'destinationRegion' instead." #-}

-- | The name of the snapshot copy grant to use when snapshots of an AWS KMS-encrypted cluster are copied to the destination region.
--
-- /Note:/ Consider using 'snapshotCopyGrantName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escSnapshotCopyGrantName :: Lens.Lens' EnableSnapshotCopy (Lude.Maybe Lude.Text)
escSnapshotCopyGrantName = Lens.lens (snapshotCopyGrantName :: EnableSnapshotCopy -> Lude.Maybe Lude.Text) (\s a -> s {snapshotCopyGrantName = a} :: EnableSnapshotCopy)
{-# DEPRECATED escSnapshotCopyGrantName "Use generic-lens or generic-optics with 'snapshotCopyGrantName' instead." #-}

instance Lude.AWSRequest EnableSnapshotCopy where
  type Rs EnableSnapshotCopy = EnableSnapshotCopyResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "EnableSnapshotCopyResult"
      ( \s h x ->
          EnableSnapshotCopyResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableSnapshotCopy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableSnapshotCopy where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableSnapshotCopy where
  toQuery EnableSnapshotCopy' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableSnapshotCopy" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "RetentionPeriod" Lude.=: retentionPeriod,
        "DestinationRegion" Lude.=: destinationRegion,
        "SnapshotCopyGrantName" Lude.=: snapshotCopyGrantName
      ]

-- | /See:/ 'mkEnableSnapshotCopyResponse' smart constructor.
data EnableSnapshotCopyResponse = EnableSnapshotCopyResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableSnapshotCopyResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkEnableSnapshotCopyResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableSnapshotCopyResponse
mkEnableSnapshotCopyResponse pResponseStatus_ =
  EnableSnapshotCopyResponse'
    { cluster = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escrsCluster :: Lens.Lens' EnableSnapshotCopyResponse (Lude.Maybe Cluster)
escrsCluster = Lens.lens (cluster :: EnableSnapshotCopyResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: EnableSnapshotCopyResponse)
{-# DEPRECATED escrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
escrsResponseStatus :: Lens.Lens' EnableSnapshotCopyResponse Lude.Int
escrsResponseStatus = Lens.lens (responseStatus :: EnableSnapshotCopyResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableSnapshotCopyResponse)
{-# DEPRECATED escrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
