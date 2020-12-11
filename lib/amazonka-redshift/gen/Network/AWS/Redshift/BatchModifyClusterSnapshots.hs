{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.BatchModifyClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the settings for a set of cluster snapshots.
module Network.AWS.Redshift.BatchModifyClusterSnapshots
  ( -- * Creating a request
    BatchModifyClusterSnapshots (..),
    mkBatchModifyClusterSnapshots,

    -- ** Request lenses
    bmcsManualSnapshotRetentionPeriod,
    bmcsForce,
    bmcsSnapshotIdentifierList,

    -- * Destructuring the response
    BatchModifyClusterSnapshotsResponse (..),
    mkBatchModifyClusterSnapshotsResponse,

    -- ** Response lenses
    bmcsrsResources,
    bmcsrsErrors,
    bmcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchModifyClusterSnapshots' smart constructor.
data BatchModifyClusterSnapshots = BatchModifyClusterSnapshots'
  { manualSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    force :: Lude.Maybe Lude.Bool,
    snapshotIdentifierList ::
      [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchModifyClusterSnapshots' with the minimum fields required to make a request.
--
-- * 'force' - A boolean value indicating whether to override an exception if the retention period has passed.
-- * 'manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely.
--
-- The number must be either -1 or an integer between 1 and 3,653.
-- If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
-- * 'snapshotIdentifierList' - A list of snapshot identifiers you want to modify.
mkBatchModifyClusterSnapshots ::
  BatchModifyClusterSnapshots
mkBatchModifyClusterSnapshots =
  BatchModifyClusterSnapshots'
    { manualSnapshotRetentionPeriod =
        Lude.Nothing,
      force = Lude.Nothing,
      snapshotIdentifierList = Lude.mempty
    }

-- | The number of days that a manual snapshot is retained. If you specify the value -1, the manual snapshot is retained indefinitely.
--
-- The number must be either -1 or an integer between 1 and 3,653.
-- If you decrease the manual snapshot retention period from its current value, existing manual snapshots that fall outside of the new retention period will return an error. If you want to suppress the errors and delete the snapshots, use the force option.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsManualSnapshotRetentionPeriod :: Lens.Lens' BatchModifyClusterSnapshots (Lude.Maybe Lude.Int)
bmcsManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: BatchModifyClusterSnapshots -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: BatchModifyClusterSnapshots)
{-# DEPRECATED bmcsManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | A boolean value indicating whether to override an exception if the retention period has passed.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsForce :: Lens.Lens' BatchModifyClusterSnapshots (Lude.Maybe Lude.Bool)
bmcsForce = Lens.lens (force :: BatchModifyClusterSnapshots -> Lude.Maybe Lude.Bool) (\s a -> s {force = a} :: BatchModifyClusterSnapshots)
{-# DEPRECATED bmcsForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | A list of snapshot identifiers you want to modify.
--
-- /Note:/ Consider using 'snapshotIdentifierList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsSnapshotIdentifierList :: Lens.Lens' BatchModifyClusterSnapshots [Lude.Text]
bmcsSnapshotIdentifierList = Lens.lens (snapshotIdentifierList :: BatchModifyClusterSnapshots -> [Lude.Text]) (\s a -> s {snapshotIdentifierList = a} :: BatchModifyClusterSnapshots)
{-# DEPRECATED bmcsSnapshotIdentifierList "Use generic-lens or generic-optics with 'snapshotIdentifierList' instead." #-}

instance Lude.AWSRequest BatchModifyClusterSnapshots where
  type
    Rs BatchModifyClusterSnapshots =
      BatchModifyClusterSnapshotsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "BatchModifyClusterSnapshotsResult"
      ( \s h x ->
          BatchModifyClusterSnapshotsResponse'
            Lude.<$> ( x Lude..@? "Resources" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "String")
                     )
            Lude.<*> ( x Lude..@? "Errors" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "SnapshotErrorMessage")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchModifyClusterSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchModifyClusterSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchModifyClusterSnapshots where
  toQuery BatchModifyClusterSnapshots' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("BatchModifyClusterSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "Force" Lude.=: force,
        "SnapshotIdentifierList"
          Lude.=: Lude.toQueryList "String" snapshotIdentifierList
      ]

-- | /See:/ 'mkBatchModifyClusterSnapshotsResponse' smart constructor.
data BatchModifyClusterSnapshotsResponse = BatchModifyClusterSnapshotsResponse'
  { resources ::
      Lude.Maybe
        [Lude.Text],
    errors ::
      Lude.Maybe
        [SnapshotErrorMessage],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchModifyClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of any errors returned.
-- * 'resources' - A list of the snapshots that were modified.
-- * 'responseStatus' - The response status code.
mkBatchModifyClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchModifyClusterSnapshotsResponse
mkBatchModifyClusterSnapshotsResponse pResponseStatus_ =
  BatchModifyClusterSnapshotsResponse'
    { resources = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the snapshots that were modified.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrsResources :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Lude.Maybe [Lude.Text])
bmcsrsResources = Lens.lens (resources :: BatchModifyClusterSnapshotsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: BatchModifyClusterSnapshotsResponse)
{-# DEPRECATED bmcsrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | A list of any errors returned.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrsErrors :: Lens.Lens' BatchModifyClusterSnapshotsResponse (Lude.Maybe [SnapshotErrorMessage])
bmcsrsErrors = Lens.lens (errors :: BatchModifyClusterSnapshotsResponse -> Lude.Maybe [SnapshotErrorMessage]) (\s a -> s {errors = a} :: BatchModifyClusterSnapshotsResponse)
{-# DEPRECATED bmcsrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bmcsrsResponseStatus :: Lens.Lens' BatchModifyClusterSnapshotsResponse Lude.Int
bmcsrsResponseStatus = Lens.lens (responseStatus :: BatchModifyClusterSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchModifyClusterSnapshotsResponse)
{-# DEPRECATED bmcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
