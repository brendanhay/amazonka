{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Returns an endpoint for the Amazon ECS agent to poll for updates.
module Network.AWS.ECS.DiscoverPollEndpoint
  ( -- * Creating a Request
    DiscoverPollEndpoint (..),
    newDiscoverPollEndpoint,

    -- * Request Lenses
    discoverPollEndpoint_containerInstance,
    discoverPollEndpoint_cluster,

    -- * Destructuring the Response
    DiscoverPollEndpointResponse (..),
    newDiscoverPollEndpointResponse,

    -- * Response Lenses
    discoverPollEndpointResponse_telemetryEndpoint,
    discoverPollEndpointResponse_endpoint,
    discoverPollEndpointResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDiscoverPollEndpoint' smart constructor.
data DiscoverPollEndpoint = DiscoverPollEndpoint'
  { -- | The container instance ID or full ARN of the container instance. The ARN
    -- contains the @arn:aws:ecs@ namespace, followed by the Region of the
    -- container instance, the AWS account ID of the container instance owner,
    -- the @container-instance@ namespace, and then the container instance ID.
    -- For example,
    -- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
    containerInstance :: Core.Maybe Core.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- which the container instance belongs.
    cluster :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiscoverPollEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerInstance', 'discoverPollEndpoint_containerInstance' - The container instance ID or full ARN of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- container instance, the AWS account ID of the container instance owner,
-- the @container-instance@ namespace, and then the container instance ID.
-- For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
--
-- 'cluster', 'discoverPollEndpoint_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster to
-- which the container instance belongs.
newDiscoverPollEndpoint ::
  DiscoverPollEndpoint
newDiscoverPollEndpoint =
  DiscoverPollEndpoint'
    { containerInstance =
        Core.Nothing,
      cluster = Core.Nothing
    }

-- | The container instance ID or full ARN of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- container instance, the AWS account ID of the container instance owner,
-- the @container-instance@ namespace, and then the container instance ID.
-- For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
discoverPollEndpoint_containerInstance :: Lens.Lens' DiscoverPollEndpoint (Core.Maybe Core.Text)
discoverPollEndpoint_containerInstance = Lens.lens (\DiscoverPollEndpoint' {containerInstance} -> containerInstance) (\s@DiscoverPollEndpoint' {} a -> s {containerInstance = a} :: DiscoverPollEndpoint)

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- which the container instance belongs.
discoverPollEndpoint_cluster :: Lens.Lens' DiscoverPollEndpoint (Core.Maybe Core.Text)
discoverPollEndpoint_cluster = Lens.lens (\DiscoverPollEndpoint' {cluster} -> cluster) (\s@DiscoverPollEndpoint' {} a -> s {cluster = a} :: DiscoverPollEndpoint)

instance Core.AWSRequest DiscoverPollEndpoint where
  type
    AWSResponse DiscoverPollEndpoint =
      DiscoverPollEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverPollEndpointResponse'
            Core.<$> (x Core..?> "telemetryEndpoint")
            Core.<*> (x Core..?> "endpoint")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DiscoverPollEndpoint

instance Core.NFData DiscoverPollEndpoint

instance Core.ToHeaders DiscoverPollEndpoint where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.DiscoverPollEndpoint" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DiscoverPollEndpoint where
  toJSON DiscoverPollEndpoint' {..} =
    Core.object
      ( Core.catMaybes
          [ ("containerInstance" Core..=)
              Core.<$> containerInstance,
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.ToPath DiscoverPollEndpoint where
  toPath = Core.const "/"

instance Core.ToQuery DiscoverPollEndpoint where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDiscoverPollEndpointResponse' smart constructor.
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
  { -- | The telemetry endpoint for the Amazon ECS agent.
    telemetryEndpoint :: Core.Maybe Core.Text,
    -- | The endpoint for the Amazon ECS agent to poll.
    endpoint :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DiscoverPollEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'telemetryEndpoint', 'discoverPollEndpointResponse_telemetryEndpoint' - The telemetry endpoint for the Amazon ECS agent.
--
-- 'endpoint', 'discoverPollEndpointResponse_endpoint' - The endpoint for the Amazon ECS agent to poll.
--
-- 'httpStatus', 'discoverPollEndpointResponse_httpStatus' - The response's http status code.
newDiscoverPollEndpointResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DiscoverPollEndpointResponse
newDiscoverPollEndpointResponse pHttpStatus_ =
  DiscoverPollEndpointResponse'
    { telemetryEndpoint =
        Core.Nothing,
      endpoint = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The telemetry endpoint for the Amazon ECS agent.
discoverPollEndpointResponse_telemetryEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Core.Maybe Core.Text)
discoverPollEndpointResponse_telemetryEndpoint = Lens.lens (\DiscoverPollEndpointResponse' {telemetryEndpoint} -> telemetryEndpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {telemetryEndpoint = a} :: DiscoverPollEndpointResponse)

-- | The endpoint for the Amazon ECS agent to poll.
discoverPollEndpointResponse_endpoint :: Lens.Lens' DiscoverPollEndpointResponse (Core.Maybe Core.Text)
discoverPollEndpointResponse_endpoint = Lens.lens (\DiscoverPollEndpointResponse' {endpoint} -> endpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {endpoint = a} :: DiscoverPollEndpointResponse)

-- | The response's http status code.
discoverPollEndpointResponse_httpStatus :: Lens.Lens' DiscoverPollEndpointResponse Core.Int
discoverPollEndpointResponse_httpStatus = Lens.lens (\DiscoverPollEndpointResponse' {httpStatus} -> httpStatus) (\s@DiscoverPollEndpointResponse' {} a -> s {httpStatus = a} :: DiscoverPollEndpointResponse)

instance Core.NFData DiscoverPollEndpointResponse
