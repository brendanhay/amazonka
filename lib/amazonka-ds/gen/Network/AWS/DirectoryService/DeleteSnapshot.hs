{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a directory snapshot.
module Network.AWS.DirectoryService.DeleteSnapshot
  ( -- * Creating a request
    DeleteSnapshot (..),
    mkDeleteSnapshot,

    -- ** Request lenses
    dsSnapshotId,

    -- * Destructuring the response
    DeleteSnapshotResponse (..),
    mkDeleteSnapshotResponse,

    -- ** Response lenses
    dsrsSnapshotId,
    dsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the inputs for the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshot' smart constructor.
newtype DeleteSnapshot = DeleteSnapshot' {snapshotId :: Lude.Text}
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
-- * 'snapshotId' - The identifier of the directory snapshot to be deleted.
mkDeleteSnapshot ::
  -- | 'snapshotId'
  Lude.Text ->
  DeleteSnapshot
mkDeleteSnapshot pSnapshotId_ =
  DeleteSnapshot' {snapshotId = pSnapshotId_}

-- | The identifier of the directory snapshot to be deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsSnapshotId :: Lens.Lens' DeleteSnapshot Lude.Text
dsSnapshotId = Lens.lens (snapshotId :: DeleteSnapshot -> Lude.Text) (\s a -> s {snapshotId = a} :: DeleteSnapshot)
{-# DEPRECATED dsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest DeleteSnapshot where
  type Rs DeleteSnapshot = DeleteSnapshotResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteSnapshotResponse'
            Lude.<$> (x Lude..?> "SnapshotId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DeleteSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteSnapshot where
  toJSON DeleteSnapshot' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SnapshotId" Lude..= snapshotId)])

instance Lude.ToPath DeleteSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteSnapshot where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'DeleteSnapshot' operation.
--
-- /See:/ 'mkDeleteSnapshotResponse' smart constructor.
data DeleteSnapshotResponse = DeleteSnapshotResponse'
  { snapshotId ::
      Lude.Maybe Lude.Text,
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
-- * 'snapshotId' - The identifier of the directory snapshot that was deleted.
mkDeleteSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteSnapshotResponse
mkDeleteSnapshotResponse pResponseStatus_ =
  DeleteSnapshotResponse'
    { snapshotId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The identifier of the directory snapshot that was deleted.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsSnapshotId :: Lens.Lens' DeleteSnapshotResponse (Lude.Maybe Lude.Text)
dsrsSnapshotId = Lens.lens (snapshotId :: DeleteSnapshotResponse -> Lude.Maybe Lude.Text) (\s a -> s {snapshotId = a} :: DeleteSnapshotResponse)
{-# DEPRECATED dsrsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DeleteSnapshotResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DeleteSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteSnapshotResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
