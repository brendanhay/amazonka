{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteInstanceSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specific snapshot of a virtual private server (or /instance/ ).
--
-- The @delete instance snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @instance snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteInstanceSnapshot
  ( -- * Creating a request
    DeleteInstanceSnapshot (..),
    mkDeleteInstanceSnapshot,

    -- ** Request lenses
    disInstanceSnapshotName,

    -- * Destructuring the response
    DeleteInstanceSnapshotResponse (..),
    mkDeleteInstanceSnapshotResponse,

    -- ** Response lenses
    disrsOperations,
    disrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteInstanceSnapshot' smart constructor.
newtype DeleteInstanceSnapshot = DeleteInstanceSnapshot'
  { instanceSnapshotName ::
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

-- | Creates a value of 'DeleteInstanceSnapshot' with the minimum fields required to make a request.
--
-- * 'instanceSnapshotName' - The name of the snapshot to delete.
mkDeleteInstanceSnapshot ::
  -- | 'instanceSnapshotName'
  Lude.Text ->
  DeleteInstanceSnapshot
mkDeleteInstanceSnapshot pInstanceSnapshotName_ =
  DeleteInstanceSnapshot'
    { instanceSnapshotName =
        pInstanceSnapshotName_
    }

-- | The name of the snapshot to delete.
--
-- /Note:/ Consider using 'instanceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disInstanceSnapshotName :: Lens.Lens' DeleteInstanceSnapshot Lude.Text
disInstanceSnapshotName = Lens.lens (instanceSnapshotName :: DeleteInstanceSnapshot -> Lude.Text) (\s a -> s {instanceSnapshotName = a} :: DeleteInstanceSnapshot)
{-# DEPRECATED disInstanceSnapshotName "Use generic-lens or generic-optics with 'instanceSnapshotName' instead." #-}

instance Lude.AWSRequest DeleteInstanceSnapshot where
  type Rs DeleteInstanceSnapshot = DeleteInstanceSnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteInstanceSnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteInstanceSnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteInstanceSnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteInstanceSnapshot where
  toJSON DeleteInstanceSnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("instanceSnapshotName" Lude..= instanceSnapshotName)]
      )

instance Lude.ToPath DeleteInstanceSnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteInstanceSnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteInstanceSnapshotResponse' smart constructor.
data DeleteInstanceSnapshotResponse = DeleteInstanceSnapshotResponse'
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

-- | Creates a value of 'DeleteInstanceSnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteInstanceSnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteInstanceSnapshotResponse
mkDeleteInstanceSnapshotResponse pResponseStatus_ =
  DeleteInstanceSnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsOperations :: Lens.Lens' DeleteInstanceSnapshotResponse (Lude.Maybe [Operation])
disrsOperations = Lens.lens (operations :: DeleteInstanceSnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteInstanceSnapshotResponse)
{-# DEPRECATED disrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disrsResponseStatus :: Lens.Lens' DeleteInstanceSnapshotResponse Lude.Int
disrsResponseStatus = Lens.lens (responseStatus :: DeleteInstanceSnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteInstanceSnapshotResponse)
{-# DEPRECATED disrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
