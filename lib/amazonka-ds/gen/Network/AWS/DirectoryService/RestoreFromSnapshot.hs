{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.RestoreFromSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a directory using an existing directory snapshot.
--
-- When you restore a directory from a snapshot, any changes made to the directory after the snapshot date are overwritten.
-- This action returns as soon as the restore operation is initiated. You can monitor the progress of the restore operation by calling the 'DescribeDirectories' operation with the directory identifier. When the __DirectoryDescription.Stage__ value changes to @Active@ , the restore operation is complete.
module Network.AWS.DirectoryService.RestoreFromSnapshot
  ( -- * Creating a request
    RestoreFromSnapshot (..),
    mkRestoreFromSnapshot,

    -- ** Request lenses
    rfsSnapshotId,

    -- * Destructuring the response
    RestoreFromSnapshotResponse (..),
    mkRestoreFromSnapshotResponse,

    -- ** Response lenses
    rfsrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | An object representing the inputs for the 'RestoreFromSnapshot' operation.
--
-- /See:/ 'mkRestoreFromSnapshot' smart constructor.
newtype RestoreFromSnapshot = RestoreFromSnapshot'
  { -- | The identifier of the snapshot to restore from.
    snapshotId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreFromSnapshot' with the minimum fields required to make a request.
--
-- * 'snapshotId' - The identifier of the snapshot to restore from.
mkRestoreFromSnapshot ::
  -- | 'snapshotId'
  Lude.Text ->
  RestoreFromSnapshot
mkRestoreFromSnapshot pSnapshotId_ =
  RestoreFromSnapshot' {snapshotId = pSnapshotId_}

-- | The identifier of the snapshot to restore from.
--
-- /Note:/ Consider using 'snapshotId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfsSnapshotId :: Lens.Lens' RestoreFromSnapshot Lude.Text
rfsSnapshotId = Lens.lens (snapshotId :: RestoreFromSnapshot -> Lude.Text) (\s a -> s {snapshotId = a} :: RestoreFromSnapshot)
{-# DEPRECATED rfsSnapshotId "Use generic-lens or generic-optics with 'snapshotId' instead." #-}

instance Lude.AWSRequest RestoreFromSnapshot where
  type Rs RestoreFromSnapshot = RestoreFromSnapshotResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RestoreFromSnapshotResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RestoreFromSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "DirectoryService_20150416.RestoreFromSnapshot" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RestoreFromSnapshot where
  toJSON RestoreFromSnapshot' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("SnapshotId" Lude..= snapshotId)])

instance Lude.ToPath RestoreFromSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery RestoreFromSnapshot where
  toQuery = Lude.const Lude.mempty

-- | Contains the results of the 'RestoreFromSnapshot' operation.
--
-- /See:/ 'mkRestoreFromSnapshotResponse' smart constructor.
newtype RestoreFromSnapshotResponse = RestoreFromSnapshotResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RestoreFromSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRestoreFromSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RestoreFromSnapshotResponse
mkRestoreFromSnapshotResponse pResponseStatus_ =
  RestoreFromSnapshotResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfsrsResponseStatus :: Lens.Lens' RestoreFromSnapshotResponse Lude.Int
rfsrsResponseStatus = Lens.lens (responseStatus :: RestoreFromSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RestoreFromSnapshotResponse)
{-# DEPRECATED rfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
