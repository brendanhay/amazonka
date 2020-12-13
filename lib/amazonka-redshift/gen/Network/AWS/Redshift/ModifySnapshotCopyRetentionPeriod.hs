{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the number of days to retain snapshots in the destination AWS Region after they are copied from the source AWS Region. By default, this operation only changes the retention period of copied automated snapshots. The retention periods for both new and existing copied automated snapshots are updated with the new retention period. You can set the manual option to change only the retention periods of copied manual snapshots. If you set this option, only newly copied manual snapshots have the new retention period.
module Network.AWS.Redshift.ModifySnapshotCopyRetentionPeriod
  ( -- * Creating a request
    ModifySnapshotCopyRetentionPeriod (..),
    mkModifySnapshotCopyRetentionPeriod,

    -- ** Request lenses
    mscrpManual,
    mscrpClusterIdentifier,
    mscrpRetentionPeriod,

    -- * Destructuring the response
    ModifySnapshotCopyRetentionPeriodResponse (..),
    mkModifySnapshotCopyRetentionPeriodResponse,

    -- ** Response lenses
    mscrprsCluster,
    mscrprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifySnapshotCopyRetentionPeriod' smart constructor.
data ModifySnapshotCopyRetentionPeriod = ModifySnapshotCopyRetentionPeriod'
  { -- | Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
    manual :: Lude.Maybe Lude.Bool,
    -- | The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region.
    --
    -- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
    clusterIdentifier :: Lude.Text,
    -- | The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region.
    --
    -- By default, this only changes the retention period of copied automated snapshots.
    -- If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period.
    -- Constraints: Must be at least 1 and no more than 35 for automated snapshots.
    -- If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.
    -- If you specify the value of -1 newly copied manual snapshots are retained indefinitely.
    -- Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
    retentionPeriod :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySnapshotCopyRetentionPeriod' with the minimum fields required to make a request.
--
-- * 'manual' - Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
-- * 'clusterIdentifier' - The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
-- * 'retentionPeriod' - The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region.
--
-- By default, this only changes the retention period of copied automated snapshots.
-- If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period.
-- Constraints: Must be at least 1 and no more than 35 for automated snapshots.
-- If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.
-- If you specify the value of -1 newly copied manual snapshots are retained indefinitely.
-- Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
mkModifySnapshotCopyRetentionPeriod ::
  -- | 'clusterIdentifier'
  Lude.Text ->
  -- | 'retentionPeriod'
  Lude.Int ->
  ModifySnapshotCopyRetentionPeriod
mkModifySnapshotCopyRetentionPeriod
  pClusterIdentifier_
  pRetentionPeriod_ =
    ModifySnapshotCopyRetentionPeriod'
      { manual = Lude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        retentionPeriod = pRetentionPeriod_
      }

-- | Indicates whether to apply the snapshot retention period to newly copied manual snapshots instead of automated snapshots.
--
-- /Note:/ Consider using 'manual' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpManual :: Lens.Lens' ModifySnapshotCopyRetentionPeriod (Lude.Maybe Lude.Bool)
mscrpManual = Lens.lens (manual :: ModifySnapshotCopyRetentionPeriod -> Lude.Maybe Lude.Bool) (\s a -> s {manual = a} :: ModifySnapshotCopyRetentionPeriod)
{-# DEPRECATED mscrpManual "Use generic-lens or generic-optics with 'manual' instead." #-}

-- | The unique identifier of the cluster for which you want to change the retention period for either automated or manual snapshots that are copied to a destination AWS Region.
--
-- Constraints: Must be the valid name of an existing cluster that has cross-region snapshot copy enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpClusterIdentifier :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Lude.Text
mscrpClusterIdentifier = Lens.lens (clusterIdentifier :: ModifySnapshotCopyRetentionPeriod -> Lude.Text) (\s a -> s {clusterIdentifier = a} :: ModifySnapshotCopyRetentionPeriod)
{-# DEPRECATED mscrpClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

-- | The number of days to retain automated snapshots in the destination AWS Region after they are copied from the source AWS Region.
--
-- By default, this only changes the retention period of copied automated snapshots.
-- If you decrease the retention period for automated snapshots that are copied to a destination AWS Region, Amazon Redshift deletes any existing automated snapshots that were copied to the destination AWS Region and that fall outside of the new retention period.
-- Constraints: Must be at least 1 and no more than 35 for automated snapshots.
-- If you specify the @manual@ option, only newly copied manual snapshots will have the new retention period.
-- If you specify the value of -1 newly copied manual snapshots are retained indefinitely.
-- Constraints: The number of days must be either -1 or an integer between 1 and 3,653 for manual snapshots.
--
-- /Note:/ Consider using 'retentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrpRetentionPeriod :: Lens.Lens' ModifySnapshotCopyRetentionPeriod Lude.Int
mscrpRetentionPeriod = Lens.lens (retentionPeriod :: ModifySnapshotCopyRetentionPeriod -> Lude.Int) (\s a -> s {retentionPeriod = a} :: ModifySnapshotCopyRetentionPeriod)
{-# DEPRECATED mscrpRetentionPeriod "Use generic-lens or generic-optics with 'retentionPeriod' instead." #-}

instance Lude.AWSRequest ModifySnapshotCopyRetentionPeriod where
  type
    Rs ModifySnapshotCopyRetentionPeriod =
      ModifySnapshotCopyRetentionPeriodResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "ModifySnapshotCopyRetentionPeriodResult"
      ( \s h x ->
          ModifySnapshotCopyRetentionPeriodResponse'
            Lude.<$> (x Lude..@? "Cluster") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifySnapshotCopyRetentionPeriod where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ModifySnapshotCopyRetentionPeriod where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifySnapshotCopyRetentionPeriod where
  toQuery ModifySnapshotCopyRetentionPeriod' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ModifySnapshotCopyRetentionPeriod" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Manual" Lude.=: manual,
        "ClusterIdentifier" Lude.=: clusterIdentifier,
        "RetentionPeriod" Lude.=: retentionPeriod
      ]

-- | /See:/ 'mkModifySnapshotCopyRetentionPeriodResponse' smart constructor.
data ModifySnapshotCopyRetentionPeriodResponse = ModifySnapshotCopyRetentionPeriodResponse'
  { cluster :: Lude.Maybe Cluster,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifySnapshotCopyRetentionPeriodResponse' with the minimum fields required to make a request.
--
-- * 'cluster' -
-- * 'responseStatus' - The response status code.
mkModifySnapshotCopyRetentionPeriodResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifySnapshotCopyRetentionPeriodResponse
mkModifySnapshotCopyRetentionPeriodResponse pResponseStatus_ =
  ModifySnapshotCopyRetentionPeriodResponse'
    { cluster =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrprsCluster :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse (Lude.Maybe Cluster)
mscrprsCluster = Lens.lens (cluster :: ModifySnapshotCopyRetentionPeriodResponse -> Lude.Maybe Cluster) (\s a -> s {cluster = a} :: ModifySnapshotCopyRetentionPeriodResponse)
{-# DEPRECATED mscrprsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mscrprsResponseStatus :: Lens.Lens' ModifySnapshotCopyRetentionPeriodResponse Lude.Int
mscrprsResponseStatus = Lens.lens (responseStatus :: ModifySnapshotCopyRetentionPeriodResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifySnapshotCopyRetentionPeriodResponse)
{-# DEPRECATED mscrprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
