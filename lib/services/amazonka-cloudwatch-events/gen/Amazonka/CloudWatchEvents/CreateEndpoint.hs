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
-- Module      : Amazonka.CloudWatchEvents.CreateEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a global endpoint. Global endpoints improve your application\'s
-- availability by making it regional-fault tolerant. To do this, you
-- define a primary and secondary Region with event buses in each Region.
-- You also create a Amazon Route 53 health check that will tell
-- EventBridge to route events to the secondary Region when an
-- \"unhealthy\" state is encountered and events will be routed back to the
-- primary Region when the health check reports a \"healthy\" state.
module Amazonka.CloudWatchEvents.CreateEndpoint
  ( -- * Creating a Request
    CreateEndpoint (..),
    newCreateEndpoint,

    -- * Request Lenses
    createEndpoint_description,
    createEndpoint_replicationConfig,
    createEndpoint_roleArn,
    createEndpoint_name,
    createEndpoint_routingConfig,
    createEndpoint_eventBuses,

    -- * Destructuring the Response
    CreateEndpointResponse (..),
    newCreateEndpointResponse,

    -- * Response Lenses
    createEndpointResponse_arn,
    createEndpointResponse_eventBuses,
    createEndpointResponse_name,
    createEndpointResponse_replicationConfig,
    createEndpointResponse_roleArn,
    createEndpointResponse_routingConfig,
    createEndpointResponse_state,
    createEndpointResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEndpoint' smart constructor.
data CreateEndpoint = CreateEndpoint'
  { -- | A description of the global endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Enable or disable event replication.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | The ARN of the role used for replication.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the global endpoint. For example,
    -- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
    name :: Prelude.Text,
    -- | Configure the routing policy, including the health check and secondary
    -- Region..
    routingConfig :: RoutingConfig,
    -- | Define the event buses used.
    --
    -- The names of the event buses must be identical in each Region.
    eventBuses :: Prelude.NonEmpty EndpointEventBus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createEndpoint_description' - A description of the global endpoint.
--
-- 'replicationConfig', 'createEndpoint_replicationConfig' - Enable or disable event replication.
--
-- 'roleArn', 'createEndpoint_roleArn' - The ARN of the role used for replication.
--
-- 'name', 'createEndpoint_name' - The name of the global endpoint. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
--
-- 'routingConfig', 'createEndpoint_routingConfig' - Configure the routing policy, including the health check and secondary
-- Region..
--
-- 'eventBuses', 'createEndpoint_eventBuses' - Define the event buses used.
--
-- The names of the event buses must be identical in each Region.
newCreateEndpoint ::
  -- | 'name'
  Prelude.Text ->
  -- | 'routingConfig'
  RoutingConfig ->
  -- | 'eventBuses'
  Prelude.NonEmpty EndpointEventBus ->
  CreateEndpoint
newCreateEndpoint pName_ pRoutingConfig_ pEventBuses_ =
  CreateEndpoint'
    { description = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      name = pName_,
      routingConfig = pRoutingConfig_,
      eventBuses = Lens.coerced Lens.# pEventBuses_
    }

-- | A description of the global endpoint.
createEndpoint_description :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_description = Lens.lens (\CreateEndpoint' {description} -> description) (\s@CreateEndpoint' {} a -> s {description = a} :: CreateEndpoint)

-- | Enable or disable event replication.
createEndpoint_replicationConfig :: Lens.Lens' CreateEndpoint (Prelude.Maybe ReplicationConfig)
createEndpoint_replicationConfig = Lens.lens (\CreateEndpoint' {replicationConfig} -> replicationConfig) (\s@CreateEndpoint' {} a -> s {replicationConfig = a} :: CreateEndpoint)

-- | The ARN of the role used for replication.
createEndpoint_roleArn :: Lens.Lens' CreateEndpoint (Prelude.Maybe Prelude.Text)
createEndpoint_roleArn = Lens.lens (\CreateEndpoint' {roleArn} -> roleArn) (\s@CreateEndpoint' {} a -> s {roleArn = a} :: CreateEndpoint)

-- | The name of the global endpoint. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
createEndpoint_name :: Lens.Lens' CreateEndpoint Prelude.Text
createEndpoint_name = Lens.lens (\CreateEndpoint' {name} -> name) (\s@CreateEndpoint' {} a -> s {name = a} :: CreateEndpoint)

-- | Configure the routing policy, including the health check and secondary
-- Region..
createEndpoint_routingConfig :: Lens.Lens' CreateEndpoint RoutingConfig
createEndpoint_routingConfig = Lens.lens (\CreateEndpoint' {routingConfig} -> routingConfig) (\s@CreateEndpoint' {} a -> s {routingConfig = a} :: CreateEndpoint)

-- | Define the event buses used.
--
-- The names of the event buses must be identical in each Region.
createEndpoint_eventBuses :: Lens.Lens' CreateEndpoint (Prelude.NonEmpty EndpointEventBus)
createEndpoint_eventBuses = Lens.lens (\CreateEndpoint' {eventBuses} -> eventBuses) (\s@CreateEndpoint' {} a -> s {eventBuses = a} :: CreateEndpoint) Prelude.. Lens.coerced

instance Core.AWSRequest CreateEndpoint where
  type
    AWSResponse CreateEndpoint =
      CreateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateEndpointResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "EventBuses")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ReplicationConfig")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "RoutingConfig")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateEndpoint where
  hashWithSalt _salt CreateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` replicationConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` eventBuses

instance Prelude.NFData CreateEndpoint where
  rnf CreateEndpoint' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf eventBuses

instance Data.ToHeaders CreateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.CreateEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateEndpoint where
  toJSON CreateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ReplicationConfig" Data..=)
              Prelude.<$> replicationConfig,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("RoutingConfig" Data..= routingConfig),
            Prelude.Just ("EventBuses" Data..= eventBuses)
          ]
      )

instance Data.ToPath CreateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateEndpointResponse' smart constructor.
data CreateEndpointResponse = CreateEndpointResponse'
  { -- | The ARN of the endpoint that was created by this request.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The event buses used by this request.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The name of the endpoint that was created by this request.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether event replication was enabled or disabled by this request.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | The ARN of the role used by event replication for this request.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration defined by this request.
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | The state of the endpoint that was created by this request.
    state :: Prelude.Maybe EndpointState,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createEndpointResponse_arn' - The ARN of the endpoint that was created by this request.
--
-- 'eventBuses', 'createEndpointResponse_eventBuses' - The event buses used by this request.
--
-- 'name', 'createEndpointResponse_name' - The name of the endpoint that was created by this request.
--
-- 'replicationConfig', 'createEndpointResponse_replicationConfig' - Whether event replication was enabled or disabled by this request.
--
-- 'roleArn', 'createEndpointResponse_roleArn' - The ARN of the role used by event replication for this request.
--
-- 'routingConfig', 'createEndpointResponse_routingConfig' - The routing configuration defined by this request.
--
-- 'state', 'createEndpointResponse_state' - The state of the endpoint that was created by this request.
--
-- 'httpStatus', 'createEndpointResponse_httpStatus' - The response's http status code.
newCreateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateEndpointResponse
newCreateEndpointResponse pHttpStatus_ =
  CreateEndpointResponse'
    { arn = Prelude.Nothing,
      eventBuses = Prelude.Nothing,
      name = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      state = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the endpoint that was created by this request.
createEndpointResponse_arn :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Prelude.Text)
createEndpointResponse_arn = Lens.lens (\CreateEndpointResponse' {arn} -> arn) (\s@CreateEndpointResponse' {} a -> s {arn = a} :: CreateEndpointResponse)

-- | The event buses used by this request.
createEndpointResponse_eventBuses :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
createEndpointResponse_eventBuses = Lens.lens (\CreateEndpointResponse' {eventBuses} -> eventBuses) (\s@CreateEndpointResponse' {} a -> s {eventBuses = a} :: CreateEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the endpoint that was created by this request.
createEndpointResponse_name :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Prelude.Text)
createEndpointResponse_name = Lens.lens (\CreateEndpointResponse' {name} -> name) (\s@CreateEndpointResponse' {} a -> s {name = a} :: CreateEndpointResponse)

-- | Whether event replication was enabled or disabled by this request.
createEndpointResponse_replicationConfig :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe ReplicationConfig)
createEndpointResponse_replicationConfig = Lens.lens (\CreateEndpointResponse' {replicationConfig} -> replicationConfig) (\s@CreateEndpointResponse' {} a -> s {replicationConfig = a} :: CreateEndpointResponse)

-- | The ARN of the role used by event replication for this request.
createEndpointResponse_roleArn :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe Prelude.Text)
createEndpointResponse_roleArn = Lens.lens (\CreateEndpointResponse' {roleArn} -> roleArn) (\s@CreateEndpointResponse' {} a -> s {roleArn = a} :: CreateEndpointResponse)

-- | The routing configuration defined by this request.
createEndpointResponse_routingConfig :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe RoutingConfig)
createEndpointResponse_routingConfig = Lens.lens (\CreateEndpointResponse' {routingConfig} -> routingConfig) (\s@CreateEndpointResponse' {} a -> s {routingConfig = a} :: CreateEndpointResponse)

-- | The state of the endpoint that was created by this request.
createEndpointResponse_state :: Lens.Lens' CreateEndpointResponse (Prelude.Maybe EndpointState)
createEndpointResponse_state = Lens.lens (\CreateEndpointResponse' {state} -> state) (\s@CreateEndpointResponse' {} a -> s {state = a} :: CreateEndpointResponse)

-- | The response's http status code.
createEndpointResponse_httpStatus :: Lens.Lens' CreateEndpointResponse Prelude.Int
createEndpointResponse_httpStatus = Lens.lens (\CreateEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateEndpointResponse' {} a -> s {httpStatus = a} :: CreateEndpointResponse)

instance Prelude.NFData CreateEndpointResponse where
  rnf CreateEndpointResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf httpStatus
