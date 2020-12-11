{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DeleteClusterSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified manual snapshot. The snapshot must be in the @available@ state, with no other users authorized to access the snapshot.
--
-- Unlike automated snapshots, manual snapshots are retained even after you delete your cluster. Amazon Redshift does not delete your manual snapshots. You must delete manual snapshot explicitly to avoid getting charged. If other accounts are authorized to access the snapshot, you must revoke all of the authorizations before you can delete the snapshot.
module Network.AWS.Redshift.DeleteClusterSnapshot
  ( -- * Creating a request
    DeleteClusterSnapshot (..),
    mkDeleteClusterSnapshot,

    -- ** Request lenses
    dcsSnapshotClusterIdentifier,
    dcsSnapshotIdentifier,

    -- * Destructuring the response
    DeleteClusterSnapshotResponse (..),
    mkDeleteClusterSnapshotResponse,

    -- ** Response lenses
    dcsrsSnapshot,
    dcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteClusterSnapshot' smart constructor.
data DeleteClusterSnapshot = DeleteClusterSnapshot'
  { snapshotClusterIdentifier ::
      Lude.Maybe Lude.Text,
    snapshotIdentifier :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClusterSnapshot' with the minimum fields required to make a request.
--
-- * 'snapshotClusterIdentifier' - The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
-- * 'snapshotIdentifier' - The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
mkDeleteClusterSnapshot ::
  -- | 'snapshotIdentifier'
  Lude.Text ->
  DeleteClusterSnapshot
mkDeleteClusterSnapshot pSnapshotIdentifier_ =
  DeleteClusterSnapshot'
    { snapshotClusterIdentifier = Lude.Nothing,
      snapshotIdentifier = pSnapshotIdentifier_
    }

-- | The unique identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- Constraints: Must be the name of valid cluster.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotClusterIdentifier :: Lens.Lens' DeleteClusterSnapshot (Lude.Maybe Lude.Text)
dcsSnapshotClusterIdentifier = Lens.lens (snapshotClusterIdentifier :: DeleteClusterSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {snapshotClusterIdentifier = a} :: DeleteClusterSnapshot)
{-# DEPRECATED dcsSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

-- | The unique identifier of the manual snapshot to be deleted.
--
-- Constraints: Must be the name of an existing snapshot that is in the @available@ , @failed@ , or @cancelled@ state.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsSnapshotIdentifier :: Lens.Lens' DeleteClusterSnapshot Lude.Text
dcsSnapshotIdentifier = Lens.lens (snapshotIdentifier :: DeleteClusterSnapshot -> Lude.Text) (\s a -> s {snapshotIdentifier = a} :: DeleteClusterSnapshot)
{-# DEPRECATED dcsSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

instance Lude.AWSRequest DeleteClusterSnapshot where
  type Rs DeleteClusterSnapshot = DeleteClusterSnapshotResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DeleteClusterSnapshotResult"
      ( \s h x ->
          DeleteClusterSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteClusterSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClusterSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClusterSnapshot where
  toQuery DeleteClusterSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteClusterSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "SnapshotClusterIdentifier" Lude.=: snapshotClusterIdentifier,
        "SnapshotIdentifier" Lude.=: snapshotIdentifier
      ]

-- | /See:/ 'mkDeleteClusterSnapshotResponse' smart constructor.
data DeleteClusterSnapshotResponse = DeleteClusterSnapshotResponse'
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

-- | Creates a value of 'DeleteClusterSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkDeleteClusterSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteClusterSnapshotResponse
mkDeleteClusterSnapshotResponse pResponseStatus_ =
  DeleteClusterSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsSnapshot :: Lens.Lens' DeleteClusterSnapshotResponse (Lude.Maybe Snapshot)
dcsrsSnapshot = Lens.lens (snapshot :: DeleteClusterSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: DeleteClusterSnapshotResponse)
{-# DEPRECATED dcsrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsrsResponseStatus :: Lens.Lens' DeleteClusterSnapshotResponse Lude.Int
dcsrsResponseStatus = Lens.lens (responseStatus :: DeleteClusterSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClusterSnapshotResponse)
{-# DEPRECATED dcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
