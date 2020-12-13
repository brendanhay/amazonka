{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an endpoint for the Amazon ECS agent to poll for updates.
module Network.AWS.ECS.DiscoverPollEndpoint
  ( -- * Creating a request
    DiscoverPollEndpoint (..),
    mkDiscoverPollEndpoint,

    -- ** Request lenses
    dpeCluster,
    dpeContainerInstance,

    -- * Destructuring the response
    DiscoverPollEndpointResponse (..),
    mkDiscoverPollEndpointResponse,

    -- ** Response lenses
    dpersTelemetryEndpoint,
    dpersEndpoint,
    dpersResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDiscoverPollEndpoint' smart constructor.
data DiscoverPollEndpoint = DiscoverPollEndpoint'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster to which the container instance belongs.
    cluster :: Lude.Maybe Lude.Text,
    -- | The container instance ID or full ARN of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
    containerInstance :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverPollEndpoint' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to which the container instance belongs.
-- * 'containerInstance' - The container instance ID or full ARN of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
mkDiscoverPollEndpoint ::
  DiscoverPollEndpoint
mkDiscoverPollEndpoint =
  DiscoverPollEndpoint'
    { cluster = Lude.Nothing,
      containerInstance = Lude.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster to which the container instance belongs.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeCluster :: Lens.Lens' DiscoverPollEndpoint (Lude.Maybe Lude.Text)
dpeCluster = Lens.lens (cluster :: DiscoverPollEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DiscoverPollEndpoint)
{-# DEPRECATED dpeCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The container instance ID or full ARN of the container instance. The ARN contains the @arn:aws:ecs@ namespace, followed by the Region of the container instance, the AWS account ID of the container instance owner, the @container-instance@ namespace, and then the container instance ID. For example, @arn:aws:ecs:region:aws_account_id:container-instance/container_instance_ID@ .
--
-- /Note:/ Consider using 'containerInstance' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpeContainerInstance :: Lens.Lens' DiscoverPollEndpoint (Lude.Maybe Lude.Text)
dpeContainerInstance = Lens.lens (containerInstance :: DiscoverPollEndpoint -> Lude.Maybe Lude.Text) (\s a -> s {containerInstance = a} :: DiscoverPollEndpoint)
{-# DEPRECATED dpeContainerInstance "Use generic-lens or generic-optics with 'containerInstance' instead." #-}

instance Lude.AWSRequest DiscoverPollEndpoint where
  type Rs DiscoverPollEndpoint = DiscoverPollEndpointResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DiscoverPollEndpointResponse'
            Lude.<$> (x Lude..?> "telemetryEndpoint")
            Lude.<*> (x Lude..?> "endpoint")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DiscoverPollEndpoint where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DiscoverPollEndpoint" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DiscoverPollEndpoint where
  toJSON DiscoverPollEndpoint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("cluster" Lude..=) Lude.<$> cluster,
            ("containerInstance" Lude..=) Lude.<$> containerInstance
          ]
      )

instance Lude.ToPath DiscoverPollEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DiscoverPollEndpoint where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDiscoverPollEndpointResponse' smart constructor.
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
  { -- | The telemetry endpoint for the Amazon ECS agent.
    telemetryEndpoint :: Lude.Maybe Lude.Text,
    -- | The endpoint for the Amazon ECS agent to poll.
    endpoint :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DiscoverPollEndpointResponse' with the minimum fields required to make a request.
--
-- * 'telemetryEndpoint' - The telemetry endpoint for the Amazon ECS agent.
-- * 'endpoint' - The endpoint for the Amazon ECS agent to poll.
-- * 'responseStatus' - The response status code.
mkDiscoverPollEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DiscoverPollEndpointResponse
mkDiscoverPollEndpointResponse pResponseStatus_ =
  DiscoverPollEndpointResponse'
    { telemetryEndpoint = Lude.Nothing,
      endpoint = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The telemetry endpoint for the Amazon ECS agent.
--
-- /Note:/ Consider using 'telemetryEndpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersTelemetryEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Lude.Maybe Lude.Text)
dpersTelemetryEndpoint = Lens.lens (telemetryEndpoint :: DiscoverPollEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {telemetryEndpoint = a} :: DiscoverPollEndpointResponse)
{-# DEPRECATED dpersTelemetryEndpoint "Use generic-lens or generic-optics with 'telemetryEndpoint' instead." #-}

-- | The endpoint for the Amazon ECS agent to poll.
--
-- /Note:/ Consider using 'endpoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Lude.Maybe Lude.Text)
dpersEndpoint = Lens.lens (endpoint :: DiscoverPollEndpointResponse -> Lude.Maybe Lude.Text) (\s a -> s {endpoint = a} :: DiscoverPollEndpointResponse)
{-# DEPRECATED dpersEndpoint "Use generic-lens or generic-optics with 'endpoint' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersResponseStatus :: Lens.Lens' DiscoverPollEndpointResponse Lude.Int
dpersResponseStatus = Lens.lens (responseStatus :: DiscoverPollEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DiscoverPollEndpointResponse)
{-# DEPRECATED dpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
