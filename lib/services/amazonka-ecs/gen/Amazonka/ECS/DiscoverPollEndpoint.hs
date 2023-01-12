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
-- Module      : Amazonka.ECS.DiscoverPollEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action is only used by the Amazon ECS agent, and it is not intended
-- for use outside of the agent.
--
-- Returns an endpoint for the Amazon ECS agent to poll for updates.
module Amazonka.ECS.DiscoverPollEndpoint
  ( -- * Creating a Request
    DiscoverPollEndpoint (..),
    newDiscoverPollEndpoint,

    -- * Request Lenses
    discoverPollEndpoint_cluster,
    discoverPollEndpoint_containerInstance,

    -- * Destructuring the Response
    DiscoverPollEndpointResponse (..),
    newDiscoverPollEndpointResponse,

    -- * Response Lenses
    discoverPollEndpointResponse_endpoint,
    discoverPollEndpointResponse_serviceConnectEndpoint,
    discoverPollEndpointResponse_telemetryEndpoint,
    discoverPollEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDiscoverPollEndpoint' smart constructor.
data DiscoverPollEndpoint = DiscoverPollEndpoint'
  { -- | The short name or full Amazon Resource Name (ARN) of the cluster that
    -- the container instance belongs to.
    cluster :: Prelude.Maybe Prelude.Text,
    -- | The container instance ID or full ARN of the container instance. For
    -- more information about the ARN format, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
    -- in the /Amazon ECS Developer Guide/.
    containerInstance :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoverPollEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cluster', 'discoverPollEndpoint_cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that
-- the container instance belongs to.
--
-- 'containerInstance', 'discoverPollEndpoint_containerInstance' - The container instance ID or full ARN of the container instance. For
-- more information about the ARN format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
newDiscoverPollEndpoint ::
  DiscoverPollEndpoint
newDiscoverPollEndpoint =
  DiscoverPollEndpoint'
    { cluster = Prelude.Nothing,
      containerInstance = Prelude.Nothing
    }

-- | The short name or full Amazon Resource Name (ARN) of the cluster that
-- the container instance belongs to.
discoverPollEndpoint_cluster :: Lens.Lens' DiscoverPollEndpoint (Prelude.Maybe Prelude.Text)
discoverPollEndpoint_cluster = Lens.lens (\DiscoverPollEndpoint' {cluster} -> cluster) (\s@DiscoverPollEndpoint' {} a -> s {cluster = a} :: DiscoverPollEndpoint)

-- | The container instance ID or full ARN of the container instance. For
-- more information about the ARN format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/ecs-account-settings.html#ecs-resource-ids Amazon Resource Name (ARN)>
-- in the /Amazon ECS Developer Guide/.
discoverPollEndpoint_containerInstance :: Lens.Lens' DiscoverPollEndpoint (Prelude.Maybe Prelude.Text)
discoverPollEndpoint_containerInstance = Lens.lens (\DiscoverPollEndpoint' {containerInstance} -> containerInstance) (\s@DiscoverPollEndpoint' {} a -> s {containerInstance = a} :: DiscoverPollEndpoint)

instance Core.AWSRequest DiscoverPollEndpoint where
  type
    AWSResponse DiscoverPollEndpoint =
      DiscoverPollEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DiscoverPollEndpointResponse'
            Prelude.<$> (x Data..?> "endpoint")
            Prelude.<*> (x Data..?> "serviceConnectEndpoint")
            Prelude.<*> (x Data..?> "telemetryEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DiscoverPollEndpoint where
  hashWithSalt _salt DiscoverPollEndpoint' {..} =
    _salt `Prelude.hashWithSalt` cluster
      `Prelude.hashWithSalt` containerInstance

instance Prelude.NFData DiscoverPollEndpoint where
  rnf DiscoverPollEndpoint' {..} =
    Prelude.rnf cluster
      `Prelude.seq` Prelude.rnf containerInstance

instance Data.ToHeaders DiscoverPollEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.DiscoverPollEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DiscoverPollEndpoint where
  toJSON DiscoverPollEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("cluster" Data..=) Prelude.<$> cluster,
            ("containerInstance" Data..=)
              Prelude.<$> containerInstance
          ]
      )

instance Data.ToPath DiscoverPollEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DiscoverPollEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDiscoverPollEndpointResponse' smart constructor.
data DiscoverPollEndpointResponse = DiscoverPollEndpointResponse'
  { -- | The endpoint for the Amazon ECS agent to poll.
    endpoint :: Prelude.Maybe Prelude.Text,
    -- | The endpoint for the Amazon ECS agent to poll for Service Connect
    -- configuration. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    serviceConnectEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The telemetry endpoint for the Amazon ECS agent.
    telemetryEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DiscoverPollEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpoint', 'discoverPollEndpointResponse_endpoint' - The endpoint for the Amazon ECS agent to poll.
--
-- 'serviceConnectEndpoint', 'discoverPollEndpointResponse_serviceConnectEndpoint' - The endpoint for the Amazon ECS agent to poll for Service Connect
-- configuration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'telemetryEndpoint', 'discoverPollEndpointResponse_telemetryEndpoint' - The telemetry endpoint for the Amazon ECS agent.
--
-- 'httpStatus', 'discoverPollEndpointResponse_httpStatus' - The response's http status code.
newDiscoverPollEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DiscoverPollEndpointResponse
newDiscoverPollEndpointResponse pHttpStatus_ =
  DiscoverPollEndpointResponse'
    { endpoint =
        Prelude.Nothing,
      serviceConnectEndpoint = Prelude.Nothing,
      telemetryEndpoint = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The endpoint for the Amazon ECS agent to poll.
discoverPollEndpointResponse_endpoint :: Lens.Lens' DiscoverPollEndpointResponse (Prelude.Maybe Prelude.Text)
discoverPollEndpointResponse_endpoint = Lens.lens (\DiscoverPollEndpointResponse' {endpoint} -> endpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {endpoint = a} :: DiscoverPollEndpointResponse)

-- | The endpoint for the Amazon ECS agent to poll for Service Connect
-- configuration. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-connect.html Service Connect>
-- in the /Amazon Elastic Container Service Developer Guide/.
discoverPollEndpointResponse_serviceConnectEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Prelude.Maybe Prelude.Text)
discoverPollEndpointResponse_serviceConnectEndpoint = Lens.lens (\DiscoverPollEndpointResponse' {serviceConnectEndpoint} -> serviceConnectEndpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {serviceConnectEndpoint = a} :: DiscoverPollEndpointResponse)

-- | The telemetry endpoint for the Amazon ECS agent.
discoverPollEndpointResponse_telemetryEndpoint :: Lens.Lens' DiscoverPollEndpointResponse (Prelude.Maybe Prelude.Text)
discoverPollEndpointResponse_telemetryEndpoint = Lens.lens (\DiscoverPollEndpointResponse' {telemetryEndpoint} -> telemetryEndpoint) (\s@DiscoverPollEndpointResponse' {} a -> s {telemetryEndpoint = a} :: DiscoverPollEndpointResponse)

-- | The response's http status code.
discoverPollEndpointResponse_httpStatus :: Lens.Lens' DiscoverPollEndpointResponse Prelude.Int
discoverPollEndpointResponse_httpStatus = Lens.lens (\DiscoverPollEndpointResponse' {httpStatus} -> httpStatus) (\s@DiscoverPollEndpointResponse' {} a -> s {httpStatus = a} :: DiscoverPollEndpointResponse)

instance Prelude.NFData DiscoverPollEndpointResponse where
  rnf DiscoverPollEndpointResponse' {..} =
    Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf serviceConnectEndpoint
      `Prelude.seq` Prelude.rnf telemetryEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
