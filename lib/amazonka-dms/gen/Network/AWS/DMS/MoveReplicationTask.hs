{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.MoveReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Moves a replication task from its current replication instance to a different target replication instance using the specified parameters. The target replication instance must be created with the same or later AWS DMS version as the current replication instance.
module Network.AWS.DMS.MoveReplicationTask
  ( -- * Creating a request
    MoveReplicationTask (..),
    mkMoveReplicationTask,

    -- ** Request lenses
    mrtReplicationTaskARN,
    mrtTargetReplicationInstanceARN,

    -- * Destructuring the response
    MoveReplicationTaskResponse (..),
    mkMoveReplicationTaskResponse,

    -- ** Response lenses
    mrtrsReplicationTask,
    mrtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkMoveReplicationTask' smart constructor.
data MoveReplicationTask = MoveReplicationTask'
  { replicationTaskARN ::
      Lude.Text,
    targetReplicationInstanceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MoveReplicationTask' with the minimum fields required to make a request.
--
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the task that you want to move.
-- * 'targetReplicationInstanceARN' - The ARN of the replication instance where you want to move the task to.
mkMoveReplicationTask ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  -- | 'targetReplicationInstanceARN'
  Lude.Text ->
  MoveReplicationTask
mkMoveReplicationTask
  pReplicationTaskARN_
  pTargetReplicationInstanceARN_ =
    MoveReplicationTask'
      { replicationTaskARN = pReplicationTaskARN_,
        targetReplicationInstanceARN = pTargetReplicationInstanceARN_
      }

-- | The Amazon Resource Name (ARN) of the task that you want to move.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtReplicationTaskARN :: Lens.Lens' MoveReplicationTask Lude.Text
mrtReplicationTaskARN = Lens.lens (replicationTaskARN :: MoveReplicationTask -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: MoveReplicationTask)
{-# DEPRECATED mrtReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | The ARN of the replication instance where you want to move the task to.
--
-- /Note:/ Consider using 'targetReplicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtTargetReplicationInstanceARN :: Lens.Lens' MoveReplicationTask Lude.Text
mrtTargetReplicationInstanceARN = Lens.lens (targetReplicationInstanceARN :: MoveReplicationTask -> Lude.Text) (\s a -> s {targetReplicationInstanceARN = a} :: MoveReplicationTask)
{-# DEPRECATED mrtTargetReplicationInstanceARN "Use generic-lens or generic-optics with 'targetReplicationInstanceARN' instead." #-}

instance Lude.AWSRequest MoveReplicationTask where
  type Rs MoveReplicationTask = MoveReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          MoveReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders MoveReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.MoveReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON MoveReplicationTask where
  toJSON MoveReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN),
            Lude.Just
              ( "TargetReplicationInstanceArn"
                  Lude..= targetReplicationInstanceARN
              )
          ]
      )

instance Lude.ToPath MoveReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery MoveReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkMoveReplicationTaskResponse' smart constructor.
data MoveReplicationTaskResponse = MoveReplicationTaskResponse'
  { replicationTask ::
      Lude.Maybe ReplicationTask,
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

-- | Creates a value of 'MoveReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The replication task that was moved.
-- * 'responseStatus' - The response status code.
mkMoveReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  MoveReplicationTaskResponse
mkMoveReplicationTaskResponse pResponseStatus_ =
  MoveReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication task that was moved.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtrsReplicationTask :: Lens.Lens' MoveReplicationTaskResponse (Lude.Maybe ReplicationTask)
mrtrsReplicationTask = Lens.lens (replicationTask :: MoveReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: MoveReplicationTaskResponse)
{-# DEPRECATED mrtrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrtrsResponseStatus :: Lens.Lens' MoveReplicationTaskResponse Lude.Int
mrtrsResponseStatus = Lens.lens (responseStatus :: MoveReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: MoveReplicationTaskResponse)
{-# DEPRECATED mrtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
