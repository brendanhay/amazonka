{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.UpdateContainerAgent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Amazon ECS container agent on a specified container instance. Updating the Amazon ECS container agent does not interrupt running tasks or services on the container instance. The process for updating the agent differs depending on whether your container instance was launched with the Amazon ECS-optimized AMI or another operating system.
--
-- @UpdateContainerAgent@ requires the Amazon ECS-optimized AMI or Amazon Linux with the @ecs-init@ service installed and running. For help updating the Amazon ECS container agent on other operating systems, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-agent-update.html#manually_update_agent Manually Updating the Amazon ECS Container Agent> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.UpdateContainerAgent
  ( -- * Creating a request
    UpdateContainerAgent (..),
    mkUpdateContainerAgent,

    -- ** Request lenses
    ucaCluster,
    ucaContainerInstance,

    -- * Destructuring the response
    UpdateContainerAgentResponse (..),
    mkUpdateContainerAgentResponse,

    -- ** Response lenses
    ucarsContainerInstance,
    ucarsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateContainerAgent' smart constructor.
data UpdateContainerAgent = UpdateContainerAgent'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
    cluster :: Lude.Maybe Lude.Text,
    -- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
    containerInstance :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContainerAgent' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
-- * 'containerInstance' - The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
mkUpdateContainerAgent ::
  -- | 'containerInstance'
  Lude.Text ->
  UpdateContainerAgent
mkUpdateContainerAgent pContainerInstance_ =
  UpdateContainerAgent'
    { cluster = Lude.Nothing,
      containerInstance = pContainerInstance_
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that your container instance is running on. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaCluster :: Lens.Lens' UpdateContainerAgent (Lude.Maybe Lude.Text)
ucaCluster = Lens.lens (cluster :: UpdateContainerAgent -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: UpdateContainerAgent)
{-# DEPRECATED ucaCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The container instance ID or full ARN entries for the container instance on which you would like to update the Amazon ECS container agent.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucaContainerInstance :: Lens.Lens' UpdateContainerAgent Lude.Text
ucaContainerInstance = Lens.lens (containerInstance :: UpdateContainerAgent -> Lude.Text) (\s a -> s {containerInstance = a} :: UpdateContainerAgent)
{-# DEPRECATED ucaContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

instance Lude.AWSRequest UpdateContainerAgent where
  type Rs UpdateContainerAgent = UpdateContainerAgentResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateContainerAgentResponse'
            Lude.<$> (x Lude..?> "containerInstance")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateContainerAgent where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateContainerAgent" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateContainerAgent where
  toJSON UpdateContainerAgent' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("containerInstance" Lude..= containerInstance)
          ]
      )

instance Lude.ToPath UpdateContainerAgent where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateContainerAgent where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateContainerAgentResponse' smart constructor.
data UpdateContainerAgentResponse = UpdateContainerAgentResponse'
  { -- | The container instance for which the container agent was updated.
    containerInstance :: Lude.Maybe ContainerInstance,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateContainerAgentResponse' with the minimum fields required to make a request.
--
-- * 'containerInstance' - The container instance for which the container agent was updated.
-- * 'responseStatus' - The response status code.
mkUpdateContainerAgentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateContainerAgentResponse
mkUpdateContainerAgentResponse pResponseStatus_ =
  UpdateContainerAgentResponse'
    { containerInstance = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The container instance for which the container agent was updated.
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarsContainerInstance :: Lens.Lens' UpdateContainerAgentResponse (Lude.Maybe ContainerInstance)
ucarsContainerInstance = Lens.lens (containerInstance :: UpdateContainerAgentResponse -> Lude.Maybe ContainerInstance) (\s a -> s {containerInstance = a} :: UpdateContainerAgentResponse)
{-# DEPRECATED ucarsContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucarsResponseStatus :: Lens.Lens' UpdateContainerAgentResponse Lude.Int
ucarsResponseStatus = Lens.lens (responseStatus :: UpdateContainerAgentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateContainerAgentResponse)
{-# DEPRECATED ucarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
