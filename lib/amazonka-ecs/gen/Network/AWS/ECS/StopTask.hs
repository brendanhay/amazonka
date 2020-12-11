{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.StopTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a running task. Any tags associated with the task will be deleted.
--
-- When 'StopTask' is called on a task, the equivalent of @docker stop@ is issued to the containers running in the task. This results in a @SIGTERM@ value and a default 30-second timeout, after which the @SIGKILL@ value is sent and the containers are forcibly stopped. If the container handles the @SIGTERM@ value gracefully and exits within 30 seconds from receiving it, no @SIGKILL@ value is sent.
module Network.AWS.ECS.StopTask
  ( -- * Creating a request
    StopTask (..),
    mkStopTask,

    -- ** Request lenses
    stCluster,
    stReason,
    stTask,

    -- * Destructuring the response
    StopTaskResponse (..),
    mkStopTaskResponse,

    -- ** Response lenses
    srsTask,
    srsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStopTask' smart constructor.
data StopTask = StopTask'
  { cluster :: Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text,
    task :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StopTask' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
-- * 'reason' - An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
-- * 'task' - The task ID or full Amazon Resource Name (ARN) of the task to stop.
mkStopTask ::
  -- | 'task'
  Lude.Text ->
  StopTask
mkStopTask pTask_ =
  StopTask'
    { cluster = Lude.Nothing,
      reason = Lude.Nothing,
      task = pTask_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task to stop. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stCluster :: Lens.Lens' StopTask (Lude.Maybe Lude.Text)
stCluster = Lens.lens (cluster :: StopTask -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: StopTask)
{-# DEPRECATED stCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | An optional message specified when a task is stopped. For example, if you are using a custom scheduler, you can use this parameter to specify the reason for stopping the task here, and the message appears in subsequent 'DescribeTasks' API operations on this task. Up to 255 characters are allowed in this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stReason :: Lens.Lens' StopTask (Lude.Maybe Lude.Text)
stReason = Lens.lens (reason :: StopTask -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: StopTask)
{-# DEPRECATED stReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The task ID or full Amazon Resource Name (ARN) of the task to stop.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stTask :: Lens.Lens' StopTask Lude.Text
stTask = Lens.lens (task :: StopTask -> Lude.Text) (\s a -> s {task = a} :: StopTask)
{-# DEPRECATED stTask "Use generic-lens or generic-optics with 'task' instead." #-}

instance Lude.AWSRequest StopTask where
  type Rs StopTask = StopTaskResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StopTaskResponse'
            Lude.<$> (x Lude..?> "task") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StopTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonEC2ContainerServiceV20141113.StopTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StopTask where
  toJSON StopTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            ("reason" Lude..=) Lude.<$> reason,
            Lude.Just ("task" Lude..= task)
          ]
      )

instance Lude.ToPath StopTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StopTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStopTaskResponse' smart constructor.
data StopTaskResponse = StopTaskResponse'
  { task :: Lude.Maybe Task,
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

-- | Creates a value of 'StopTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'task' - The task that was stopped.
mkStopTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StopTaskResponse
mkStopTaskResponse pResponseStatus_ =
  StopTaskResponse'
    { task = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The task that was stopped.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsTask :: Lens.Lens' StopTaskResponse (Lude.Maybe Task)
srsTask = Lens.lens (task :: StopTaskResponse -> Lude.Maybe Task) (\s a -> s {task = a} :: StopTaskResponse)
{-# DEPRECATED srsTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopTaskResponse Lude.Int
srsResponseStatus = Lens.lens (responseStatus :: StopTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StopTaskResponse)
{-# DEPRECATED srsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
