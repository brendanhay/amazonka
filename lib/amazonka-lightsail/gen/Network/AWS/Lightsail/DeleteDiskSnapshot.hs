{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteDiskSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified disk snapshot.
--
-- When you make periodic snapshots of a disk, the snapshots are incremental, and only the blocks on the device that have changed since your last snapshot are saved in the new snapshot. When you delete a snapshot, only the data not needed for any other snapshot is removed. So regardless of which prior snapshots have been deleted, all active snapshots will have access to all the information needed to restore the disk.
-- The @delete disk snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @disk snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteDiskSnapshot
  ( -- * Creating a request
    DeleteDiskSnapshot (..),
    mkDeleteDiskSnapshot,

    -- ** Request lenses
    ddsDiskSnapshotName,

    -- * Destructuring the response
    DeleteDiskSnapshotResponse (..),
    mkDeleteDiskSnapshotResponse,

    -- ** Response lenses
    ddsrsOperations,
    ddsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDiskSnapshot' smart constructor.
newtype DeleteDiskSnapshot = DeleteDiskSnapshot'
  { diskSnapshotName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDiskSnapshot' with the minimum fields required to make a request.
--
-- * 'diskSnapshotName' - The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
mkDeleteDiskSnapshot ::
  -- | 'diskSnapshotName'
  Lude.Text ->
  DeleteDiskSnapshot
mkDeleteDiskSnapshot pDiskSnapshotName_ =
  DeleteDiskSnapshot' {diskSnapshotName = pDiskSnapshotName_}

-- | The name of the disk snapshot you want to delete (e.g., @my-disk-snapshot@ ).
--
-- /Note:/ Consider using 'diskSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsDiskSnapshotName :: Lens.Lens' DeleteDiskSnapshot Lude.Text
ddsDiskSnapshotName = Lens.lens (diskSnapshotName :: DeleteDiskSnapshot -> Lude.Text) (\s a -> s {diskSnapshotName = a} :: DeleteDiskSnapshot)
{-# DEPRECATED ddsDiskSnapshotName "Use generic-lens or generic-optics with 'diskSnapshotName' instead." #-}

instance Lude.AWSRequest DeleteDiskSnapshot where
  type Rs DeleteDiskSnapshot = DeleteDiskSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDiskSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDiskSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteDiskSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDiskSnapshot where
  toJSON DeleteDiskSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("diskSnapshotName" Lude..= diskSnapshotName)]
      )

instance Lude.ToPath DeleteDiskSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDiskSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDiskSnapshotResponse' smart constructor.
data DeleteDiskSnapshotResponse = DeleteDiskSnapshotResponse'
  { operations ::
      Lude.Maybe [Operation],
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

-- | Creates a value of 'DeleteDiskSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteDiskSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDiskSnapshotResponse
mkDeleteDiskSnapshotResponse pResponseStatus_ =
  DeleteDiskSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsOperations :: Lens.Lens' DeleteDiskSnapshotResponse (Lude.Maybe [Operation])
ddsrsOperations = Lens.lens (operations :: DeleteDiskSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteDiskSnapshotResponse)
{-# DEPRECATED ddsrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddsrsResponseStatus :: Lens.Lens' DeleteDiskSnapshotResponse Lude.Int
ddsrsResponseStatus = Lens.lens (responseStatus :: DeleteDiskSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDiskSnapshotResponse)
{-# DEPRECATED ddsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
