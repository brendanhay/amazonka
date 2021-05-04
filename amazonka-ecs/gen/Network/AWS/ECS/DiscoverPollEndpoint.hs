{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    containerInstance :: Prelude.Maybe Prelude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the cluster to
    -- which the container instance belongs.
    cluster :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      cluster = Prelude.Nothing
    }

-- | The container instance ID or full ARN of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- container instance, the AWS account ID of the container instance owner,
-- the @container-instance@ namespace, and then the container instance ID.
-- For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
discoverPollEndpoint_containerInstance :: Lens.Lens' DiscoverPollEndpoint (Prelude.Maybe Prelude.Text)
discoverPollEndpoint_containerInstance = Lens.lens (\DiscoverPollEndpoint' {containerInstance} -> containerInstance) (\s@DiscoverPollEndpoint' {} a -> s {containerInstance = a} :: DiscoverPollEndpoint)

-- | The short name or full Amazon Resource Name (ARN) of the cluster to
-- which the container instance belongs.
discoverPollEndpoint_cluster :: Lens.Lens' DiscoverPollEndpoint (Prelude.Maybe Prelude.Text)
discoverPollEndpoint_cluster = Lens.lens (\DiscoverPollEndpoint' {cluster} -> cluster) (\s@DiscoverPollEndpoint' {} a -> s {cluster = a} :: DiscoverPollEndpoint)

instance Prelude.AWSRequest DiscoverPollEndpoint where
  type
    Rs DiscoverPollEndpoint =
      DiscoverPollEndpointResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverPollEndpointResponse'
            Prelude.<$> (x Prelude..?> "telemetryEndpoint")
            Prelude.<*> (x Prelude..?> "endpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DiscoverPollEndpoint

instance Prelude.NFData DiscoverPollEndpoint

instance Prelude.ToHeaders DiscoverPollEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.DiscoverPollEndpoint" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DiscoverPollEndpoint where
  toJSON DiscoverPollEndpoint' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("containerInstance" Prelude..=)
              Prelude.<$> containerInstance,
            ("cluster" Prelude..=) Prelude.<$> cluster
          ]
      )

instance Prelude.ToPath DiscoverPollEndpoint where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DiscoverPollEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDiscoverPollEndpointResponse' smart constructor.
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
  { -- | The telemetry endpoint for the Amazon ECS agent.
    telemetryEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the Amazon ECS agent to poll.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DiscoverPollEndpointResponse
newDiscoverPollEndpointResponse pHttpStatus_ =
  DiscoverPollEndpointResponse'
    { telemetryEndpoint =
        Prelude.Nothing,
      endpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The telemetry endpoint for the Amazon ECS agent.
discoverPollEndpointResponse_telemetryEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Prelude.Maybe Prelude.Text)
discoverPollEndpointResponse_telemetryEndpoint = Lens.lens (\DiscoverPollEndpointResponse' {telemetryEndpoint} -> telemetryEndpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {telemetryEndpoint = a} :: DiscoverPollEndpointResponse)

-- | The endpoint for the Amazon ECS agent to poll.
discoverPollEndpointResponse_endpoint :: Lens.Lens' DiscoverPollEndpointResponse (Prelude.Maybe Prelude.Text)
discoverPollEndpointResponse_endpoint = Lens.lens (\DiscoverPollEndpointResponse' {endpoint} -> endpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {endpoint = a} :: DiscoverPollEndpointResponse)

-- | The response's http status code.
discoverPollEndpointResponse_httpStatus :: Lens.Lens' DiscoverPollEndpointResponse Prelude.Int
discoverPollEndpointResponse_httpStatus = Lens.lens (\DiscoverPollEndpointResponse' {httpStatus} -> httpStatus) (\s@DiscoverPollEndpointResponse' {} a -> s {httpStatus = a} :: DiscoverPollEndpointResponse)

instance Prelude.NFData DiscoverPollEndpointResponse
