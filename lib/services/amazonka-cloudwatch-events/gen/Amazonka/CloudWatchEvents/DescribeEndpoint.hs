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
-- Module      : Amazonka.CloudWatchEvents.DescribeEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an existing global endpoint. For more
-- information about global endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide..
module Amazonka.CloudWatchEvents.DescribeEndpoint
  ( -- * Creating a Request
    DescribeEndpoint (..),
    newDescribeEndpoint,

    -- * Request Lenses
    describeEndpoint_homeRegion,
    describeEndpoint_name,

    -- * Destructuring the Response
    DescribeEndpointResponse (..),
    newDescribeEndpointResponse,

    -- * Response Lenses
    describeEndpointResponse_arn,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_description,
    describeEndpointResponse_endpointId,
    describeEndpointResponse_endpointUrl,
    describeEndpointResponse_eventBuses,
    describeEndpointResponse_lastModifiedTime,
    describeEndpointResponse_name,
    describeEndpointResponse_replicationConfig,
    describeEndpointResponse_roleArn,
    describeEndpointResponse_routingConfig,
    describeEndpointResponse_state,
    describeEndpointResponse_stateReason,
    describeEndpointResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEndpoint' smart constructor.
data DescribeEndpoint = DescribeEndpoint'
  { -- | The primary Region of the endpoint you want to get information about.
    -- For example @\"HomeRegion\": \"us-east-1\"@.
    homeRegion :: Prelude.Maybe Prelude.Text,
    -- | The name of the endpoint you want to get information about. For example,
    -- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeRegion', 'describeEndpoint_homeRegion' - The primary Region of the endpoint you want to get information about.
-- For example @\"HomeRegion\": \"us-east-1\"@.
--
-- 'name', 'describeEndpoint_name' - The name of the endpoint you want to get information about. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
newDescribeEndpoint ::
  -- | 'name'
  Prelude.Text ->
  DescribeEndpoint
newDescribeEndpoint pName_ =
  DescribeEndpoint'
    { homeRegion = Prelude.Nothing,
      name = pName_
    }

-- | The primary Region of the endpoint you want to get information about.
-- For example @\"HomeRegion\": \"us-east-1\"@.
describeEndpoint_homeRegion :: Lens.Lens' DescribeEndpoint (Prelude.Maybe Prelude.Text)
describeEndpoint_homeRegion = Lens.lens (\DescribeEndpoint' {homeRegion} -> homeRegion) (\s@DescribeEndpoint' {} a -> s {homeRegion = a} :: DescribeEndpoint)

-- | The name of the endpoint you want to get information about. For example,
-- @\"Name\":\"us-east-2-custom_bus_A-endpoint\"@.
describeEndpoint_name :: Lens.Lens' DescribeEndpoint Prelude.Text
describeEndpoint_name = Lens.lens (\DescribeEndpoint' {name} -> name) (\s@DescribeEndpoint' {} a -> s {name = a} :: DescribeEndpoint)

instance Core.AWSRequest DescribeEndpoint where
  type
    AWSResponse DescribeEndpoint =
      DescribeEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEndpointResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "EndpointId")
            Prelude.<*> (x Data..?> "EndpointUrl")
            Prelude.<*> (x Data..?> "EventBuses")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "ReplicationConfig")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "RoutingConfig")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEndpoint where
  hashWithSalt _salt DescribeEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeEndpoint where
  rnf DescribeEndpoint' {..} =
    Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DescribeEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DescribeEndpoint" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEndpoint where
  toJSON DescribeEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HomeRegion" Data..=) Prelude.<$> homeRegion,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DescribeEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEndpointResponse' smart constructor.
data DescribeEndpointResponse = DescribeEndpointResponse'
  { -- | The ARN of the endpoint you asked for information about.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the endpoint you asked for information about was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The description of the endpoint you asked for information about.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the endpoint you asked for information about.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The URL of the endpoint you asked for information about.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The event buses being used by the endpoint you asked for information
    -- about.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The last time the endpoint you asked for information about was modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the endpoint you asked for information about.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether replication is enabled or disabled for the endpoint you asked
    -- for information about.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | The ARN of the role used by the endpoint you asked for information
    -- about.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration of the endpoint you asked for information
    -- about.
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | The current state of the endpoint you asked for information about.
    state :: Prelude.Maybe EndpointState,
    -- | The reason the endpoint you asked for information about is in its
    -- current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeEndpointResponse_arn' - The ARN of the endpoint you asked for information about.
--
-- 'creationTime', 'describeEndpointResponse_creationTime' - The time the endpoint you asked for information about was created.
--
-- 'description', 'describeEndpointResponse_description' - The description of the endpoint you asked for information about.
--
-- 'endpointId', 'describeEndpointResponse_endpointId' - The ID of the endpoint you asked for information about.
--
-- 'endpointUrl', 'describeEndpointResponse_endpointUrl' - The URL of the endpoint you asked for information about.
--
-- 'eventBuses', 'describeEndpointResponse_eventBuses' - The event buses being used by the endpoint you asked for information
-- about.
--
-- 'lastModifiedTime', 'describeEndpointResponse_lastModifiedTime' - The last time the endpoint you asked for information about was modified.
--
-- 'name', 'describeEndpointResponse_name' - The name of the endpoint you asked for information about.
--
-- 'replicationConfig', 'describeEndpointResponse_replicationConfig' - Whether replication is enabled or disabled for the endpoint you asked
-- for information about.
--
-- 'roleArn', 'describeEndpointResponse_roleArn' - The ARN of the role used by the endpoint you asked for information
-- about.
--
-- 'routingConfig', 'describeEndpointResponse_routingConfig' - The routing configuration of the endpoint you asked for information
-- about.
--
-- 'state', 'describeEndpointResponse_state' - The current state of the endpoint you asked for information about.
--
-- 'stateReason', 'describeEndpointResponse_stateReason' - The reason the endpoint you asked for information about is in its
-- current state.
--
-- 'httpStatus', 'describeEndpointResponse_httpStatus' - The response's http status code.
newDescribeEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEndpointResponse
newDescribeEndpointResponse pHttpStatus_ =
  DescribeEndpointResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      eventBuses = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      state = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the endpoint you asked for information about.
describeEndpointResponse_arn :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_arn = Lens.lens (\DescribeEndpointResponse' {arn} -> arn) (\s@DescribeEndpointResponse' {} a -> s {arn = a} :: DescribeEndpointResponse)

-- | The time the endpoint you asked for information about was created.
describeEndpointResponse_creationTime :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.UTCTime)
describeEndpointResponse_creationTime = Lens.lens (\DescribeEndpointResponse' {creationTime} -> creationTime) (\s@DescribeEndpointResponse' {} a -> s {creationTime = a} :: DescribeEndpointResponse) Prelude.. Lens.mapping Data._Time

-- | The description of the endpoint you asked for information about.
describeEndpointResponse_description :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_description = Lens.lens (\DescribeEndpointResponse' {description} -> description) (\s@DescribeEndpointResponse' {} a -> s {description = a} :: DescribeEndpointResponse)

-- | The ID of the endpoint you asked for information about.
describeEndpointResponse_endpointId :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_endpointId = Lens.lens (\DescribeEndpointResponse' {endpointId} -> endpointId) (\s@DescribeEndpointResponse' {} a -> s {endpointId = a} :: DescribeEndpointResponse)

-- | The URL of the endpoint you asked for information about.
describeEndpointResponse_endpointUrl :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_endpointUrl = Lens.lens (\DescribeEndpointResponse' {endpointUrl} -> endpointUrl) (\s@DescribeEndpointResponse' {} a -> s {endpointUrl = a} :: DescribeEndpointResponse)

-- | The event buses being used by the endpoint you asked for information
-- about.
describeEndpointResponse_eventBuses :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
describeEndpointResponse_eventBuses = Lens.lens (\DescribeEndpointResponse' {eventBuses} -> eventBuses) (\s@DescribeEndpointResponse' {} a -> s {eventBuses = a} :: DescribeEndpointResponse) Prelude.. Lens.mapping Lens.coerced

-- | The last time the endpoint you asked for information about was modified.
describeEndpointResponse_lastModifiedTime :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.UTCTime)
describeEndpointResponse_lastModifiedTime = Lens.lens (\DescribeEndpointResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeEndpointResponse' {} a -> s {lastModifiedTime = a} :: DescribeEndpointResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the endpoint you asked for information about.
describeEndpointResponse_name :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_name = Lens.lens (\DescribeEndpointResponse' {name} -> name) (\s@DescribeEndpointResponse' {} a -> s {name = a} :: DescribeEndpointResponse)

-- | Whether replication is enabled or disabled for the endpoint you asked
-- for information about.
describeEndpointResponse_replicationConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe ReplicationConfig)
describeEndpointResponse_replicationConfig = Lens.lens (\DescribeEndpointResponse' {replicationConfig} -> replicationConfig) (\s@DescribeEndpointResponse' {} a -> s {replicationConfig = a} :: DescribeEndpointResponse)

-- | The ARN of the role used by the endpoint you asked for information
-- about.
describeEndpointResponse_roleArn :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_roleArn = Lens.lens (\DescribeEndpointResponse' {roleArn} -> roleArn) (\s@DescribeEndpointResponse' {} a -> s {roleArn = a} :: DescribeEndpointResponse)

-- | The routing configuration of the endpoint you asked for information
-- about.
describeEndpointResponse_routingConfig :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe RoutingConfig)
describeEndpointResponse_routingConfig = Lens.lens (\DescribeEndpointResponse' {routingConfig} -> routingConfig) (\s@DescribeEndpointResponse' {} a -> s {routingConfig = a} :: DescribeEndpointResponse)

-- | The current state of the endpoint you asked for information about.
describeEndpointResponse_state :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe EndpointState)
describeEndpointResponse_state = Lens.lens (\DescribeEndpointResponse' {state} -> state) (\s@DescribeEndpointResponse' {} a -> s {state = a} :: DescribeEndpointResponse)

-- | The reason the endpoint you asked for information about is in its
-- current state.
describeEndpointResponse_stateReason :: Lens.Lens' DescribeEndpointResponse (Prelude.Maybe Prelude.Text)
describeEndpointResponse_stateReason = Lens.lens (\DescribeEndpointResponse' {stateReason} -> stateReason) (\s@DescribeEndpointResponse' {} a -> s {stateReason = a} :: DescribeEndpointResponse)

-- | The response's http status code.
describeEndpointResponse_httpStatus :: Lens.Lens' DescribeEndpointResponse Prelude.Int
describeEndpointResponse_httpStatus = Lens.lens (\DescribeEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeEndpointResponse' {} a -> s {httpStatus = a} :: DescribeEndpointResponse)

instance Prelude.NFData DescribeEndpointResponse where
  rnf DescribeEndpointResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf endpointUrl
      `Prelude.seq` Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf httpStatus
