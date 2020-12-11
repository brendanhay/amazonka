{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitContainerStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that a container changed states.
module Network.AWS.ECS.SubmitContainerStateChange
  ( -- * Creating a request
    SubmitContainerStateChange (..),
    mkSubmitContainerStateChange,

    -- ** Request lenses
    scscNetworkBindings,
    scscStatus,
    scscCluster,
    scscContainerName,
    scscReason,
    scscExitCode,
    scscTask,
    scscRuntimeId,

    -- * Destructuring the response
    SubmitContainerStateChangeResponse (..),
    mkSubmitContainerStateChangeResponse,

    -- ** Response lenses
    scscrsAcknowledgment,
    scscrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSubmitContainerStateChange' smart constructor.
data SubmitContainerStateChange = SubmitContainerStateChange'
  { networkBindings ::
      Lude.Maybe [NetworkBinding],
    status :: Lude.Maybe Lude.Text,
    cluster :: Lude.Maybe Lude.Text,
    containerName :: Lude.Maybe Lude.Text,
    reason :: Lude.Maybe Lude.Text,
    exitCode :: Lude.Maybe Lude.Int,
    task :: Lude.Maybe Lude.Text,
    runtimeId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitContainerStateChange' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full ARN of the cluster that hosts the container.
-- * 'containerName' - The name of the container.
-- * 'exitCode' - The exit code returned for the state change request.
-- * 'networkBindings' - The network bindings of the container.
-- * 'reason' - The reason for the state change request.
-- * 'runtimeId' - The ID of the Docker container.
-- * 'status' - The status of the state change request.
-- * 'task' - The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
mkSubmitContainerStateChange ::
  SubmitContainerStateChange
mkSubmitContainerStateChange =
  SubmitContainerStateChange'
    { networkBindings = Lude.Nothing,
      status = Lude.Nothing,
      cluster = Lude.Nothing,
      containerName = Lude.Nothing,
      reason = Lude.Nothing,
      exitCode = Lude.Nothing,
      task = Lude.Nothing,
      runtimeId = Lude.Nothing
    }

-- | The network bindings of the container.
--
-- /Note:/ Consider using 'networkBindings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscNetworkBindings :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe [NetworkBinding])
scscNetworkBindings = Lens.lens (networkBindings :: SubmitContainerStateChange -> Lude.Maybe [NetworkBinding]) (\s a -> s {networkBindings = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscNetworkBindings "Use generic-lens or generic-optics with 'networkBindings' instead." #-}

-- | The status of the state change request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscStatus :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscStatus = Lens.lens (status :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The short name or full ARN of the cluster that hosts the container.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscCluster :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscCluster = Lens.lens (cluster :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The name of the container.
--
-- /Note:/ Consider using 'containerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscContainerName :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscContainerName = Lens.lens (containerName :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {containerName = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscContainerName "Use generic-lens or generic-optics with 'containerName' instead." #-}

-- | The reason for the state change request.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscReason :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscReason = Lens.lens (reason :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The exit code returned for the state change request.
--
-- /Note:/ Consider using 'exitCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscExitCode :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Int)
scscExitCode = Lens.lens (exitCode :: SubmitContainerStateChange -> Lude.Maybe Lude.Int) (\s a -> s {exitCode = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscExitCode "Use generic-lens or generic-optics with 'exitCode' instead." #-}

-- | The task ID or full Amazon Resource Name (ARN) of the task that hosts the container.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscTask :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscTask = Lens.lens (task :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {task = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The ID of the Docker container.
--
-- /Note:/ Consider using 'runtimeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscRuntimeId :: Lens.Lens' SubmitContainerStateChange (Lude.Maybe Lude.Text)
scscRuntimeId = Lens.lens (runtimeId :: SubmitContainerStateChange -> Lude.Maybe Lude.Text) (\s a -> s {runtimeId = a} :: SubmitContainerStateChange)
{-# DEPRECATED scscRuntimeId "Use generic-lens or generic-optics with 'runtimeId' instead." #-}

instance Lude.AWSRequest SubmitContainerStateChange where
  type
    Rs SubmitContainerStateChange =
      SubmitContainerStateChangeResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          SubmitContainerStateChangeResponse'
            Lude.<$> (x Lude..?> "acknowledgment")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SubmitContainerStateChange where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.SubmitContainerStateChange" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SubmitContainerStateChange where
  toJSON SubmitContainerStateChange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("networkBindings" Lude..=) Lude.<$> networkBindings,
            ("status" Lude..=) Lude.<$> status,
            ("cluster" Lude..=) Lude.<$> cluster,
            ("containerName" Lude..=) Lude.<$> containerName,
            ("reason" Lude..=) Lude.<$> reason,
            ("exitCode" Lude..=) Lude.<$> exitCode,
            ("task" Lude..=) Lude.<$> task,
            ("runtimeId" Lude..=) Lude.<$> runtimeId
          ]
      )

instance Lude.ToPath SubmitContainerStateChange where
  toPath = Lude.const "/"

instance Lude.ToQuery SubmitContainerStateChange where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSubmitContainerStateChangeResponse' smart constructor.
data SubmitContainerStateChangeResponse = SubmitContainerStateChangeResponse'
  { acknowledgment ::
      Lude.Maybe Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SubmitContainerStateChangeResponse' with the minimum fields required to make a request.
--
-- * 'acknowledgment' - Acknowledgement of the state change.
-- * 'responseStatus' - The response status code.
mkSubmitContainerStateChangeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SubmitContainerStateChangeResponse
mkSubmitContainerStateChangeResponse pResponseStatus_ =
  SubmitContainerStateChangeResponse'
    { acknowledgment =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrsAcknowledgment :: Lens.Lens' SubmitContainerStateChangeResponse (Lude.Maybe Lude.Text)
scscrsAcknowledgment = Lens.lens (acknowledgment :: SubmitContainerStateChangeResponse -> Lude.Maybe Lude.Text) (\s a -> s {acknowledgment = a} :: SubmitContainerStateChangeResponse)
{-# DEPRECATED scscrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scscrsResponseStatus :: Lens.Lens' SubmitContainerStateChangeResponse Lude.Int
scscrsResponseStatus = Lens.lens (responseStatus :: SubmitContainerStateChangeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SubmitContainerStateChangeResponse)
{-# DEPRECATED scscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
