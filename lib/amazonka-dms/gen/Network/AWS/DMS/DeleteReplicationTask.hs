{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication task.
module Network.AWS.DMS.DeleteReplicationTask
  ( -- * Creating a request
    DeleteReplicationTask (..),
    mkDeleteReplicationTask,

    -- ** Request lenses
    drtReplicationTaskARN,

    -- * Destructuring the response
    DeleteReplicationTaskResponse (..),
    mkDeleteReplicationTaskResponse,

    -- ** Response lenses
    drtrsReplicationTask,
    drtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteReplicationTask' smart constructor.
newtype DeleteReplicationTask = DeleteReplicationTask'
  { -- | The Amazon Resource Name (ARN) of the replication task to be deleted.
    replicationTaskARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationTask' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task to be deleted.
mkDeleteReplicationTask ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  DeleteReplicationTask
mkDeleteReplicationTask pReplicationTaskARN_ =
  DeleteReplicationTask' {replicationTaskARN = pReplicationTaskARN_}

-- | The Amazon Resource Name (ARN) of the replication task to be deleted.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtReplicationTaskARN :: Lens.Lens' DeleteReplicationTask Lude.Text
drtReplicationTaskARN = Lens.lens (replicationTaskARN :: DeleteReplicationTask -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: DeleteReplicationTask)
{-# DEPRECATED drtReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.AWSRequest DeleteReplicationTask where
  type Rs DeleteReplicationTask = DeleteReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.DeleteReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteReplicationTask where
  toJSON DeleteReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)]
      )

instance Lude.ToPath DeleteReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteReplicationTaskResponse' smart constructor.
data DeleteReplicationTaskResponse = DeleteReplicationTaskResponse'
  { -- | The deleted replication task.
    replicationTask :: Lude.Maybe ReplicationTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The deleted replication task.
-- * 'responseStatus' - The response status code.
mkDeleteReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteReplicationTaskResponse
mkDeleteReplicationTaskResponse pResponseStatus_ =
  DeleteReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The deleted replication task.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrsReplicationTask :: Lens.Lens' DeleteReplicationTaskResponse (Lude.Maybe ReplicationTask)
drtrsReplicationTask = Lens.lens (replicationTask :: DeleteReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: DeleteReplicationTaskResponse)
{-# DEPRECATED drtrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drtrsResponseStatus :: Lens.Lens' DeleteReplicationTaskResponse Lude.Int
drtrsResponseStatus = Lens.lens (responseStatus :: DeleteReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteReplicationTaskResponse)
{-# DEPRECATED drtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
