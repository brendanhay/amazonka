{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.CopyClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified automated cluster snapshot to a new manual cluster snapshot. The source must be an automated snapshot and it must be in the available state.
--
-- When you delete a cluster, Amazon Redshift deletes any automated snapshots of the cluster. Also, when the retention period of the snapshot expires, Amazon Redshift automatically deletes it. If you want to keep an automated snapshot for a longer period, you can make a manual copy of the snapshot. Manual snapshots are retained until you delete them.
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.CopyClusterSnapshot
  ( -- * Creating a request
    CopyClusterSnapshot (..),
    mkCopyClusterSnapshot,

    -- ** Request lenses
    copManualSnapshotRetentionPeriod,
    copSourceSnapshotClusterIdentifier,
    copSourceSnapshotIdentifier,
    copTargetSnapshotIdentifier,

    -- * Destructuring the response
    CopyClusterSnapshotResponse (..),
    mkCopyClusterSnapshotResponse,

    -- ** Response lenses
    ccsrsSnapshot,
    ccsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCopyClusterSnapshot' smart constructor.
data CopyClusterSnapshot = CopyClusterSnapshot'
  { manualSnapshotRetentionPeriod ::
      Lude.Maybe Lude.Int,
    sourceSnapshotClusterIdentifier ::
      Lude.Maybe Lude.Text,
    sourceSnapshotIdentifier :: Lude.Text,
    targetSnapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'manualSnapshotRetentionPeriod' - The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
-- * 'sourceSnapshotClusterIdentifier' - The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints:
--
--     * Must be the identifier for a valid cluster.
--
--
-- * 'sourceSnapshotIdentifier' - The identifier for the source snapshot.
--
-- Constraints:
--
--     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
--
-- * 'targetSnapshotIdentifier' - The identifier given to the new manual snapshot.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank.
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for the AWS account that is making the request.
mkCopyClusterSnapshot ::
  -- | 'sourceSnapshotIdentifier'
  Lude.Text ->
  -- | 'targetSnapshotIdentifier'
  Lude.Text ->
  CopyClusterSnapshot
mkCopyClusterSnapshot
  pSourceSnapshotIdentifier_
  pTargetSnapshotIdentifier_ =
    CopyClusterSnapshot'
      { manualSnapshotRetentionPeriod =
          Lude.Nothing,
        sourceSnapshotClusterIdentifier = Lude.Nothing,
        sourceSnapshotIdentifier = pSourceSnapshotIdentifier_,
        targetSnapshotIdentifier = pTargetSnapshotIdentifier_
      }

-- | The number of days that a manual snapshot is retained. If the value is -1, the manual snapshot is retained indefinitely.
--
-- The value must be either -1 or an integer between 1 and 3,653.
-- The default value is -1.
--
-- /Note:/ Consider using 'manualSnapshotRetentionPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
copManualSnapshotRetentionPeriod :: Lens.Lens' CopyClusterSnapshot (Lude.Maybe Lude.Int)
copManualSnapshotRetentionPeriod = Lens.lens (manualSnapshotRetentionPeriod :: CopyClusterSnapshot -> Lude.Maybe Lude.Int) (\s a -> s {manualSnapshotRetentionPeriod = a} :: CopyClusterSnapshot)
{-# DEPRECATED copManualSnapshotRetentionPeriod "Use generic-lens or generic-optics with 'manualSnapshotRetentionPeriod' instead." #-}

-- | The identifier of the cluster the source snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints:
--
--     * Must be the identifier for a valid cluster.
--
--
--
-- /Note:/ Consider using 'sourceSnapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
copSourceSnapshotClusterIdentifier :: Lens.Lens' CopyClusterSnapshot (Lude.Maybe Lude.Text)
copSourceSnapshotClusterIdentifier = Lens.lens (sourceSnapshotClusterIdentifier :: CopyClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceSnapshotClusterIdentifier = a} :: CopyClusterSnapshot)
{-# DEPRECATED copSourceSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotClusterIdentifier' instead." #-}

-- | The identifier for the source snapshot.
--
-- Constraints:
--
--     * Must be the identifier for a valid automated snapshot whose state is @available@ .
--
--
--
-- /Note:/ Consider using 'sourceSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
copSourceSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Lude.Text
copSourceSnapshotIdentifier = Lens.lens (sourceSnapshotIdentifier :: CopyClusterSnapshot -> Lude.Text) (\s a -> s {sourceSnapshotIdentifier = a} :: CopyClusterSnapshot)
{-# DEPRECATED copSourceSnapshotIdentifier "Use generic-lens or generic-optics with 'sourceSnapshotIdentifier' instead." #-}

-- | The identifier given to the new manual snapshot.
--
-- Constraints:
--
--     * Cannot be null, empty, or blank.
--
--
--     * Must contain from 1 to 255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--     * Must be unique for the AWS account that is making the request.
--
--
--
-- /Note:/ Consider using 'targetSnapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
copTargetSnapshotIdentifier :: Lens.Lens' CopyClusterSnapshot Lude.Text
copTargetSnapshotIdentifier = Lens.lens (targetSnapshotIdentifier :: CopyClusterSnapshot -> Lude.Text) (\s a -> s {targetSnapshotIdentifier = a} :: CopyClusterSnapshot)
{-# DEPRECATED copTargetSnapshotIdentifier "Use generic-lens or generic-optics with 'targetSnapshotIdentifier' instead." #-}

instance Lude.AWSRequest CopyClusterSnapshot where
  type Rs CopyClusterSnapshot = CopyClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "CopyClusterSnapshotResult"
      ( \s h x ->
          CopyClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopyClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CopyClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopyClusterSnapshot where
  toQuery CopyClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CopyClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "ManualSnapshotRetentionPeriod"
          Lude.=: manualSnapshotRetentionPeriod,
        "SourceSnapshotClusterIdentifier"
          Lude.=: sourceSnapshotClusterIdentifier,
        "SourceSnapshotIdentifier" Lude.=: sourceSnapshotIdentifier,
        "TargetSnapshotIdentifier" Lude.=: targetSnapshotIdentifier
      ]

-- | /See:/ 'mkCopyClusterSnapshotResponse' smart constructor.
data CopyClusterSnapshotResponse = CopyClusterSnapshotResponse'
  { snapshot ::
      Lude.Maybe Snapshot,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopyClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkCopyClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopyClusterSnapshotResponse
mkCopyClusterSnapshotResponse pResponseStatus_ =
  CopyClusterSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsSnapshot :: Lens.Lens' CopyClusterSnapshotResponse (Lude.Maybe Snapshot)
ccsrsSnapshot = Lens.lens (snapshot :: CopyClusterSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: CopyClusterSnapshotResponse)
{-# DEPRECATED ccsrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsrsResponseStatus :: Lens.Lens' CopyClusterSnapshotResponse Lude.Int
ccsrsResponseStatus = Lens.lens (responseStatus :: CopyClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopyClusterSnapshotResponse)
{-# DEPRECATED ccsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
