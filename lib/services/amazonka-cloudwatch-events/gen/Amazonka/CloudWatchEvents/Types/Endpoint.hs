{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudWatchEvents.Types.Endpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Endpoint where

import Amazonka.CloudWatchEvents.Types.EndpointEventBus
import Amazonka.CloudWatchEvents.Types.EndpointState
import Amazonka.CloudWatchEvents.Types.ReplicationConfig
import Amazonka.CloudWatchEvents.Types.RoutingConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An global endpoint used to improve your application\'s availability by
-- making it regional-fault tolerant. For more information about global
-- endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide..
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
    -- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
    -- @abcde.veo@.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role used by event replication for the endpoint.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration of the endpoint.
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | The ARN of the endpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the endpoint.
    state :: Prelude.Maybe EndpointState,
    -- | Whether event replication was enabled or disabled for this endpoint.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | A description for the endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The last time the endpoint was modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The URL of the endpoint.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The time the endpoint was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The event buses being used by the endpoint.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The reason the endpoint is in its current state.
    stateReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'endpoint_name' - The name of the endpoint.
--
-- 'endpointId', 'endpoint_endpointId' - The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
-- @abcde.veo@.
--
-- 'roleArn', 'endpoint_roleArn' - The ARN of the role used by event replication for the endpoint.
--
-- 'routingConfig', 'endpoint_routingConfig' - The routing configuration of the endpoint.
--
-- 'arn', 'endpoint_arn' - The ARN of the endpoint.
--
-- 'state', 'endpoint_state' - The current state of the endpoint.
--
-- 'replicationConfig', 'endpoint_replicationConfig' - Whether event replication was enabled or disabled for this endpoint.
--
-- 'description', 'endpoint_description' - A description for the endpoint.
--
-- 'lastModifiedTime', 'endpoint_lastModifiedTime' - The last time the endpoint was modified.
--
-- 'endpointUrl', 'endpoint_endpointUrl' - The URL of the endpoint.
--
-- 'creationTime', 'endpoint_creationTime' - The time the endpoint was created.
--
-- 'eventBuses', 'endpoint_eventBuses' - The event buses being used by the endpoint.
--
-- 'stateReason', 'endpoint_stateReason' - The reason the endpoint is in its current state.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { name = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      routingConfig = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      replicationConfig = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      endpointUrl = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      eventBuses = Prelude.Nothing,
      stateReason = Prelude.Nothing
    }

-- | The name of the endpoint.
endpoint_name :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_name = Lens.lens (\Endpoint' {name} -> name) (\s@Endpoint' {} a -> s {name = a} :: Endpoint)

-- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is abcde.veo.endpoints.event.amazonaws.com, then the EndpointId is
-- @abcde.veo@.
endpoint_endpointId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointId = Lens.lens (\Endpoint' {endpointId} -> endpointId) (\s@Endpoint' {} a -> s {endpointId = a} :: Endpoint)

-- | The ARN of the role used by event replication for the endpoint.
endpoint_roleArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_roleArn = Lens.lens (\Endpoint' {roleArn} -> roleArn) (\s@Endpoint' {} a -> s {roleArn = a} :: Endpoint)

-- | The routing configuration of the endpoint.
endpoint_routingConfig :: Lens.Lens' Endpoint (Prelude.Maybe RoutingConfig)
endpoint_routingConfig = Lens.lens (\Endpoint' {routingConfig} -> routingConfig) (\s@Endpoint' {} a -> s {routingConfig = a} :: Endpoint)

-- | The ARN of the endpoint.
endpoint_arn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_arn = Lens.lens (\Endpoint' {arn} -> arn) (\s@Endpoint' {} a -> s {arn = a} :: Endpoint)

-- | The current state of the endpoint.
endpoint_state :: Lens.Lens' Endpoint (Prelude.Maybe EndpointState)
endpoint_state = Lens.lens (\Endpoint' {state} -> state) (\s@Endpoint' {} a -> s {state = a} :: Endpoint)

-- | Whether event replication was enabled or disabled for this endpoint.
endpoint_replicationConfig :: Lens.Lens' Endpoint (Prelude.Maybe ReplicationConfig)
endpoint_replicationConfig = Lens.lens (\Endpoint' {replicationConfig} -> replicationConfig) (\s@Endpoint' {} a -> s {replicationConfig = a} :: Endpoint)

-- | A description for the endpoint.
endpoint_description :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_description = Lens.lens (\Endpoint' {description} -> description) (\s@Endpoint' {} a -> s {description = a} :: Endpoint)

-- | The last time the endpoint was modified.
endpoint_lastModifiedTime :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.UTCTime)
endpoint_lastModifiedTime = Lens.lens (\Endpoint' {lastModifiedTime} -> lastModifiedTime) (\s@Endpoint' {} a -> s {lastModifiedTime = a} :: Endpoint) Prelude.. Lens.mapping Core._Time

-- | The URL of the endpoint.
endpoint_endpointUrl :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointUrl = Lens.lens (\Endpoint' {endpointUrl} -> endpointUrl) (\s@Endpoint' {} a -> s {endpointUrl = a} :: Endpoint)

-- | The time the endpoint was created.
endpoint_creationTime :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.UTCTime)
endpoint_creationTime = Lens.lens (\Endpoint' {creationTime} -> creationTime) (\s@Endpoint' {} a -> s {creationTime = a} :: Endpoint) Prelude.. Lens.mapping Core._Time

-- | The event buses being used by the endpoint.
endpoint_eventBuses :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
endpoint_eventBuses = Lens.lens (\Endpoint' {eventBuses} -> eventBuses) (\s@Endpoint' {} a -> s {eventBuses = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | The reason the endpoint is in its current state.
endpoint_stateReason :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_stateReason = Lens.lens (\Endpoint' {stateReason} -> stateReason) (\s@Endpoint' {} a -> s {stateReason = a} :: Endpoint)

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "EndpointId")
            Prelude.<*> (x Core..:? "RoleArn")
            Prelude.<*> (x Core..:? "RoutingConfig")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "ReplicationConfig")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "EndpointUrl")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "EventBuses")
            Prelude.<*> (x Core..:? "StateReason")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` replicationConfig
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` endpointUrl
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` eventBuses
      `Prelude.hashWithSalt` stateReason

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf routingConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf replicationConfig
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf endpointUrl
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf eventBuses
      `Prelude.seq` Prelude.rnf stateReason
