{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitTaskStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that a task changed states.
module Network.AWS.ECS.SubmitTaskStateChange
  ( -- * Creating a request
    SubmitTaskStateChange (..),
    mkSubmitTaskStateChange,

    -- ** Request lenses
    stscStatus,
    stscCluster,
    stscAttachments,
    stscExecutionStoppedAt,
    stscPullStoppedAt,
    stscContainers,
    stscReason,
    stscTask,
    stscPullStartedAt,

    -- * Destructuring the response
    SubmitTaskStateChangeResponse (..),
    mkSubmitTaskStateChangeResponse,

    -- ** Response lenses
    stscrsAcknowledgment,
    stscrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubmitTaskStateChange' smart constructor.
data SubmitTaskStateChange = SubmitTaskStateChange'
  { -- | The status of the state change request.
    status :: Lude.Maybe Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task.
    cluster :: Lude.Maybe Lude.Text,
    -- | Any attachments associated with the state change request.
    attachments :: Lude.Maybe [AttachmentStateChange],
    -- | The Unix timestamp for when the task execution stopped.
    executionStoppedAt :: Lude.Maybe Lude.Timestamp,
    -- | The Unix timestamp for when the container image pull completed.
    pullStoppedAt :: Lude.Maybe Lude.Timestamp,
    -- | Any containers associated with the state change request.
    containers :: Lude.Maybe [ContainerStateChange],
    -- | The reason for the state change request.
    reason :: Lude.Maybe Lude.Text,
    -- | The task ID or full ARN of the task in the state change request.
    task :: Lude.Maybe Lude.Text,
    -- | The Unix timestamp for when the container image pull began.
    pullStartedAt :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitTaskStateChange' with the minimum fields required to make a request.
--
-- * 'status' - The status of the state change request.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task.
-- * 'attachments' - Any attachments associated with the state change request.
-- * 'executionStoppedAt' - The Unix timestamp for when the task execution stopped.
-- * 'pullStoppedAt' - The Unix timestamp for when the container image pull completed.
-- * 'containers' - Any containers associated with the state change request.
-- * 'reason' - The reason for the state change request.
-- * 'task' - The task ID or full ARN of the task in the state change request.
-- * 'pullStartedAt' - The Unix timestamp for when the container image pull began.
mkSubmitTaskStateChange ::
  SubmitTaskStateChange
mkSubmitTaskStateChange =
  SubmitTaskStateChange'
    { status = Lude.Nothing,
      cluster = Lude.Nothing,
      attachments = Lude.Nothing,
      executionStoppedAt = Lude.Nothing,
      pullStoppedAt = Lude.Nothing,
      containers = Lude.Nothing,
      reason = Lude.Nothing,
      task = Lude.Nothing,
      pullStartedAt = Lude.Nothing
    }

-- | The status of the state change request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscStatus :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Text)
stscStatus = Lens.lens (status :: SubmitTaskStateChange -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscCluster :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Text)
stscCluster = Lens.lens (cluster :: SubmitTaskStateChange -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | Any attachments associated with the state change request.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscAttachments :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe [AttachmentStateChange])
stscAttachments = Lens.lens (attachments :: SubmitTaskStateChange -> Lude.Maybe [AttachmentStateChange]) (\s a -> s {attachments = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The Unix timestamp for when the task execution stopped.
--
-- /Note:/ Consider using 'executionStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscExecutionStoppedAt :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Timestamp)
stscExecutionStoppedAt = Lens.lens (executionStoppedAt :: SubmitTaskStateChange -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionStoppedAt = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscExecutionStoppedAt "Use generic-lens or generic-optics with 'executionStoppedAt' instead." #-}

-- | The Unix timestamp for when the container image pull completed.
--
-- /Note:/ Consider using 'pullStoppedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscPullStoppedAt :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Timestamp)
stscPullStoppedAt = Lens.lens (pullStoppedAt :: SubmitTaskStateChange -> Lude.Maybe Lude.Timestamp) (\s a -> s {pullStoppedAt = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscPullStoppedAt "Use generic-lens or generic-optics with 'pullStoppedAt' instead." #-}

-- | Any containers associated with the state change request.
--
-- /Note:/ Consider using 'containers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscContainers :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe [ContainerStateChange])
stscContainers = Lens.lens (containers :: SubmitTaskStateChange -> Lude.Maybe [ContainerStateChange]) (\s a -> s {containers = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscContainers "Use generic-lens or generic-optics with 'containers' instead." #-}

-- | The reason for the state change request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscReason :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Text)
stscReason = Lens.lens (reason :: SubmitTaskStateChange -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The task ID or full ARN of the task in the state change request.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscTask :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Text)
stscTask = Lens.lens (task :: SubmitTaskStateChange -> Lude.Maybe Lude.Text) (\s a -> s {task = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The Unix timestamp for when the container image pull began.
--
-- /Note:/ Consider using 'pullStartedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscPullStartedAt :: Lens.Lens' SubmitTaskStateChange (Lude.Maybe Lude.Timestamp)
stscPullStartedAt = Lens.lens (pullStartedAt :: SubmitTaskStateChange -> Lude.Maybe Lude.Timestamp) (\s a -> s {pullStartedAt = a} :: SubmitTaskStateChange)
{-# DEPRECATED stscPullStartedAt "Use generic-lens or generic-optics with 'pullStartedAt' instead." #-}

instance Lude.AWSRequest SubmitTaskStateChange where
  type Rs SubmitTaskStateChange = SubmitTaskStateChangeResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SubmitTaskStateChangeResponse'
            Lude.<$> (x Lude..?> "acknowledgment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubmitTaskStateChange where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.SubmitTaskStateChange" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubmitTaskStateChange where
  toJSON SubmitTaskStateChange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("attachments" Lude..=) Lude.<$> attachments,
            ("executionStoppedAt" Lude..=) Lude.<$> executionStoppedAt,
            ("pullStoppedAt" Lude..=) Lude.<$> pullStoppedAt,
            ("containers" Lude..=) Lude.<$> containers,
            ("reason" Lude..=) Lude.<$> reason,
            ("task" Lude..=) Lude.<$> task,
            ("pullStartedAt" Lude..=) Lude.<$> pullStartedAt
          ]
      )

instance Lude.ToPath SubmitTaskStateChange where
  toPath = Lude.const "/"

instance Lude.ToQuery SubmitTaskStateChange where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubmitTaskStateChangeResponse' smart constructor.
data SubmitTaskStateChangeResponse = SubmitTaskStateChangeResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitTaskStateChangeResponse' with the minimum fields required to make a request.
--
-- * 'acknowledgment' - Acknowledgement of the state change.
-- * 'responseStatus' - The response status code.
mkSubmitTaskStateChangeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SubmitTaskStateChangeResponse
mkSubmitTaskStateChangeResponse pResponseStatus_ =
  SubmitTaskStateChangeResponse'
    { acknowledgment = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscrsAcknowledgment :: Lens.Lens' SubmitTaskStateChangeResponse (Lude.Maybe Lude.Text)
stscrsAcknowledgment = Lens.lens (acknowledgment :: SubmitTaskStateChangeResponse -> Lude.Maybe Lude.Text) (\s a -> s {acknowledgment = a} :: SubmitTaskStateChangeResponse)
{-# DEPRECATED stscrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stscrsResponseStatus :: Lens.Lens' SubmitTaskStateChangeResponse Lude.Int
stscrsResponseStatus = Lens.lens (responseStatus :: SubmitTaskStateChangeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubmitTaskStateChangeResponse)
{-# DEPRECATED stscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
