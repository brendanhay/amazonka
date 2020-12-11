{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing snapshot. When you receive a successful response from this operation, ElastiCache immediately begins deleting the snapshot; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteSnapshot
  ( -- * Creating a request
    DeleteSnapshot (..),
    mkDeleteSnapshot,

    -- ** Request lenses
    dSnapshotName,

    -- * Destructuring the response
    DeleteSnapshotResponse (..),
    mkDeleteSnapshotResponse,

    -- ** Response lenses
    dsrsSnapshot,
    dsrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input of a @DeleteSnapshot@ operation.
--
-- /See:/ 'mkDeleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot' {snapshotName :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteSnapshot' with the minimum fields required to make a request.
--
-- * 'snapshotName' - The name of the snapshot to be deleted.
mkDeleteSnapshot ::
  -- | 'snapshotName'
  Lude.Text ->
  DeleteSnapshot
mkDeleteSnapshot pSnapshotName_ =
  DeleteSnapshot' {snapshotName = pSnapshotName_}

-- | The name of the snapshot to be deleted.
--
-- /Note:/ Consider using 'snapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSnapshotName :: Lens.Lens' DeleteSnapshot Lude.Text
dSnapshotName = Lens.lens (snapshotName :: DeleteSnapshot -> Lude.Text) (\s a -> s {snapshotName = a} :: DeleteSnapshot)
{-# DEPRECATED dSnapshotName "Use generic-lens or generic-optics with 'snapshotName' instead." #-}

instance Lude.AWSRequest DeleteSnapshot where
  type Rs DeleteSnapshot = DeleteSnapshotResponse
  request = Req.postQuery elastiCacheService
  response =
    Res.receiveXMLWrapper
      "DeleteSnapshotResult"
      ( \s h x ->
          DeleteSnapshotResponse'
            Lude.<$> (x Lude..@? "Snapshot") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSnapshot where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshot where
  toQuery DeleteSnapshot' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteSnapshot" :: Lude.ByteString),
        "Version" Lude.=: ("2015-02-02" :: Lude.ByteString),
        "SnapshotName" Lude.=: snapshotName
      ]

-- | /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
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

-- | Creates a value of 'DeleteSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'snapshot' - Undocumented field.
mkDeleteSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSnapshotResponse
mkDeleteSnapshotResponse pResponseStatus_ =
  DeleteSnapshotResponse'
    { snapshot = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSnapshot :: Lens.Lens' DeleteSnapshotResponse (Lude.Maybe Snapshot)
dsrsSnapshot = Lens.lens (snapshot :: DeleteSnapshotResponse -> Lude.Maybe Snapshot) (\s a -> s {snapshot = a} :: DeleteSnapshotResponse)
{-# DEPRECATED dsrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSnapshotResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSnapshotResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
