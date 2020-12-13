{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StopReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the replication task.
module Network.AWS.DMS.StopReplicationTask
  ( -- * Creating a request
    StopReplicationTask (..),
    mkStopReplicationTask,

    -- ** Request lenses
    sReplicationTaskARN,

    -- * Destructuring the response
    StopReplicationTaskResponse (..),
    mkStopReplicationTaskResponse,

    -- ** Response lenses
    srsReplicationTask,
    srsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStopReplicationTask' smart constructor.
newtype StopReplicationTask = StopReplicationTask'
  { -- | The Amazon Resource Name(ARN) of the replication task to be stopped.
    replicationTaskARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopReplicationTask' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name(ARN) of the replication task to be stopped.
mkStopReplicationTask ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  StopReplicationTask
mkStopReplicationTask pReplicationTaskARN_ =
  StopReplicationTask' {replicationTaskARN = pReplicationTaskARN_}

-- | The Amazon Resource Name(ARN) of the replication task to be stopped.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sReplicationTaskARN :: Lens.Lens' StopReplicationTask Lude.Text
sReplicationTaskARN = Lens.lens (replicationTaskARN :: StopReplicationTask -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: StopReplicationTask)
{-# DEPRECATED sReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.AWSRequest StopReplicationTask where
  type Rs StopReplicationTask = StopReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.StopReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopReplicationTask where
  toJSON StopReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)]
      )

instance Lude.ToPath StopReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StopReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStopReplicationTaskResponse' smart constructor.
data StopReplicationTaskResponse = StopReplicationTaskResponse'
  { -- | The replication task stopped.
    replicationTask :: Lude.Maybe ReplicationTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The replication task stopped.
-- * 'responseStatus' - The response status code.
mkStopReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopReplicationTaskResponse
mkStopReplicationTaskResponse pResponseStatus_ =
  StopReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication task stopped.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsReplicationTask :: Lens.Lens' StopReplicationTaskResponse (Lude.Maybe ReplicationTask)
srsReplicationTask = Lens.lens (replicationTask :: StopReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: StopReplicationTaskResponse)
{-# DEPRECATED srsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopReplicationTaskResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopReplicationTaskResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
