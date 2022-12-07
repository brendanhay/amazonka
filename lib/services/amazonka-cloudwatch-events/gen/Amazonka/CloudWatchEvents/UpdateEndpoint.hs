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
-- Module      : Amazonka.CloudWatchEvents.UpdateEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing endpoint. For more information about global
-- endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide..
module Amazonka.CloudWatchEvents.UpdateEndpoint
  ( -- * Creating a Request
    UpdateEndpoint (..),
    newUpdateEndpoint,

    -- * Request Lenses
    updateEndpoint_roleArn,
    updateEndpoint_routingConfig,
    updateEndpoint_replicationConfig,
    updateEndpoint_description,
    updateEndpoint_eventBuses,
    updateEndpoint_name,

    -- * Destructuring the Response
    UpdateEndpointResponse (..),
    newUpdateEndpointResponse,

    -- * Response Lenses
    updateEndpointResponse_name,
    updateEndpointResponse_endpointId,
    updateEndpointResponse_roleArn,
    updateEndpointResponse_routingConfig,
    updateEndpointResponse_arn,
    updateEndpointResponse_state,
    updateEndpointResponse_replicationConfig,
    updateEndpointResponse_endpointUrl,
    updateEndpointResponse_eventBuses,
    updateEndpointResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | The ARN of the role used by event replication for this request.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Configure the routing policy, including the health check and secondary
    -- Region..
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | Whether event replication was enabled or disabled by this request.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | A description for the endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Define event buses used for replication.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The name of the endpoint you want to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'updateEndpoint_roleArn' - The ARN of the role used by event replication for this request.
--
-- 'routingConfig', 'updateEndpoint_routingConfig' - Configure the routing policy, including the health check and secondary
-- Region..
--
-- 'replicationConfig', 'updateEndpoint_replicationConfig' - Whether event replication was enabled or disabled by this request.
--
-- 'description', 'updateEndpoint_description' - A description for the endpoint.
--
-- 'eventBuses', 'updateEndpoint_eventBuses' - Define event buses used for replication.
--
-- 'name', 'updateEndpoint_name' - The name of the endpoint you want to update.
newUpdateEndpoint ::
  -- | 'name'
  Prelude.Text ->
  UpdateEndpoint
newUpdateEndpoint pName_ =
  UpdateEndpoint'
    { roleArn = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      eventBuses = Prelude.Nothing,
      name = pName_
    }

-- | The ARN of the role used by event replication for this request.
updateEndpoint_roleArn :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Text)
updateEndpoint_roleArn = Lens.lens (\UpdateEndpoint' {roleArn} -> roleArn) (\s@UpdateEndpoint' {} a -> s {roleArn = a} :: UpdateEndpoint)

-- | Configure the routing policy, including the health check and secondary
-- Region..
updateEndpoint_routingConfig :: Lens.Lens' UpdateEndpoint (Prelude.Maybe RoutingConfig)
updateEndpoint_routingConfig = Lens.lens (\UpdateEndpoint' {routingConfig} -> routingConfig) (\s@UpdateEndpoint' {} a -> s {routingConfig = a} :: UpdateEndpoint)

-- | Whether event replication was enabled or disabled by this request.
updateEndpoint_replicationConfig :: Lens.Lens' UpdateEndpoint (Prelude.Maybe ReplicationConfig)
updateEndpoint_replicationConfig = Lens.lens (\UpdateEndpoint' {replicationConfig} -> replicationConfig) (\s@UpdateEndpoint' {} a -> s {replicationConfig = a} :: UpdateEndpoint)

-- | A description for the endpoint.
updateEndpoint_description :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Text)
updateEndpoint_description = Lens.lens (\UpdateEndpoint' {description} -> description) (\s@UpdateEndpoint' {} a -> s {description = a} :: UpdateEndpoint)

-- | Define event buses used for replication.
updateEndpoint_eventBuses :: Lens.Lens' UpdateEndpoint (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
updateEndpoint_eventBuses = Lens.lens (\UpdateEndpoint' {eventBuses} -> eventBuses) (\s@UpdateEndpoint' {} a -> s {eventBuses = a} :: UpdateEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The name of the endpoint you want to update.
updateEndpoint_name :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_name = Lens.lens (\UpdateEndpoint' {name} -> name) (\s@UpdateEndpoint' {} a -> s {name = a} :: UpdateEndpoint)

instance Core.AWSRequest UpdateEndpoint where
  type
    AWSResponse UpdateEndpoint =
      UpdateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateEndpointResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "EndpointId")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "RoutingConfig")
            Prelude.<*> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "ReplicationConfig")
            Prelude.<*> (x Data..?> "EndpointUrl")
            Prelude.<*> (x Data..?> "EventBuses")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEndpoint where
  hashWithSalt _salt UpdateEndpoint' {..} =
    _salt `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` replicationConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` eventBuses
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateEndpoint where
  rnf UpdateEndpoint' {..} =
    Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.UpdateEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("RoutingConfig" Data..=) Prelude.<$> routingConfig,
            ("ReplicationConfig" Data..=)
              Prelude.<$> replicationConfig,
            ("Description" Data..=) Prelude.<$> description,
            ("EventBuses" Data..=) Prelude.<$> eventBuses,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The name of the endpoint you updated in this request.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint you updated in this request.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role used by event replication for the endpoint you
    -- updated in this request.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration you updated in this request.
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | The ARN of the endpoint you updated in this request.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The state of the endpoint you updated in this request.
    state :: Prelude.Maybe EndpointState,
    -- | Whether event replication was enabled or disabled for the endpoint you
    -- updated in this request.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | The URL of the endpoint you updated in this request.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The event buses used for replication for the endpoint you updated in
    -- this request.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateEndpointResponse_name' - The name of the endpoint you updated in this request.
--
-- 'endpointId', 'updateEndpointResponse_endpointId' - The ID of the endpoint you updated in this request.
--
-- 'roleArn', 'updateEndpointResponse_roleArn' - The ARN of the role used by event replication for the endpoint you
-- updated in this request.
--
-- 'routingConfig', 'updateEndpointResponse_routingConfig' - The routing configuration you updated in this request.
--
-- 'arn', 'updateEndpointResponse_arn' - The ARN of the endpoint you updated in this request.
--
-- 'state', 'updateEndpointResponse_state' - The state of the endpoint you updated in this request.
--
-- 'replicationConfig', 'updateEndpointResponse_replicationConfig' - Whether event replication was enabled or disabled for the endpoint you
-- updated in this request.
--
-- 'endpointUrl', 'updateEndpointResponse_endpointUrl' - The URL of the endpoint you updated in this request.
--
-- 'eventBuses', 'updateEndpointResponse_eventBuses' - The event buses used for replication for the endpoint you updated in
-- this request.
--
-- 'httpStatus', 'updateEndpointResponse_httpStatus' - The response's http status code.
newUpdateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEndpointResponse
newUpdateEndpointResponse pHttpStatus_ =
  UpdateEndpointResponse'
    { name = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      eventBuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the endpoint you updated in this request.
updateEndpointResponse_name :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe Prelude.Text)
updateEndpointResponse_name = Lens.lens (\UpdateEndpointResponse' {name} -> name) (\s@UpdateEndpointResponse' {} a -> s {name = a} :: UpdateEndpointResponse)

-- | The ID of the endpoint you updated in this request.
updateEndpointResponse_endpointId :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe Prelude.Text)
updateEndpointResponse_endpointId = Lens.lens (\UpdateEndpointResponse' {endpointId} -> endpointId) (\s@UpdateEndpointResponse' {} a -> s {endpointId = a} :: UpdateEndpointResponse)

-- | The ARN of the role used by event replication for the endpoint you
-- updated in this request.
updateEndpointResponse_roleArn :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe Prelude.Text)
updateEndpointResponse_roleArn = Lens.lens (\UpdateEndpointResponse' {roleArn} -> roleArn) (\s@UpdateEndpointResponse' {} a -> s {roleArn = a} :: UpdateEndpointResponse)

-- | The routing configuration you updated in this request.
updateEndpointResponse_routingConfig :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe RoutingConfig)
updateEndpointResponse_routingConfig = Lens.lens (\UpdateEndpointResponse' {routingConfig} -> routingConfig) (\s@UpdateEndpointResponse' {} a -> s {routingConfig = a} :: UpdateEndpointResponse)

-- | The ARN of the endpoint you updated in this request.
updateEndpointResponse_arn :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe Prelude.Text)
updateEndpointResponse_arn = Lens.lens (\UpdateEndpointResponse' {arn} -> arn) (\s@UpdateEndpointResponse' {} a -> s {arn = a} :: UpdateEndpointResponse)

-- | The state of the endpoint you updated in this request.
updateEndpointResponse_state :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe EndpointState)
updateEndpointResponse_state = Lens.lens (\UpdateEndpointResponse' {state} -> state) (\s@UpdateEndpointResponse' {} a -> s {state = a} :: UpdateEndpointResponse)

-- | Whether event replication was enabled or disabled for the endpoint you
-- updated in this request.
updateEndpointResponse_replicationConfig :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe ReplicationConfig)
updateEndpointResponse_replicationConfig = Lens.lens (\UpdateEndpointResponse' {replicationConfig} -> replicationConfig) (\s@UpdateEndpointResponse' {} a -> s {replicationConfig = a} :: UpdateEndpointResponse)

-- | The URL of the endpoint you updated in this request.
updateEndpointResponse_endpointUrl :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe Prelude.Text)
updateEndpointResponse_endpointUrl = Lens.lens (\UpdateEndpointResponse' {endpointUrl} -> endpointUrl) (\s@UpdateEndpointResponse' {} a -> s {endpointUrl = a} :: UpdateEndpointResponse)

-- | The event buses used for replication for the endpoint you updated in
-- this request.
updateEndpointResponse_eventBuses :: Lens.Lens' UpdateEndpointResponse (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
updateEndpointResponse_eventBuses = Lens.lens (\UpdateEndpointResponse' {eventBuses} -> eventBuses) (\s@UpdateEndpointResponse' {} a -> s {eventBuses = a} :: UpdateEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateEndpointResponse_httpStatus :: Lens.Lens' UpdateEndpointResponse Prelude.Int
updateEndpointResponse_httpStatus = Lens.lens (\UpdateEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointResponse' {} a -> s {httpStatus = a} :: UpdateEndpointResponse)

instance Prelude.NFData UpdateEndpointResponse where
  rnf UpdateEndpointResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf endpointUrl
      `Prelude.seq` Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf httpStatus
