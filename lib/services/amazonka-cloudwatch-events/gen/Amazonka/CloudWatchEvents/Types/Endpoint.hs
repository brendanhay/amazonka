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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A global endpoint used to improve your application\'s availability by
-- making it regional-fault tolerant. For more information about global
-- endpoints, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-global-endpoints.html Making applications Regional-fault tolerant with global endpoints and event replication>
-- in the Amazon EventBridge User Guide.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The ARN of the endpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the endpoint was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | A description for the endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
    -- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
    -- EndpointId is @abcde.veo@.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | The URL of the endpoint.
    endpointUrl :: Prelude.Maybe Prelude.Text,
    -- | The event buses being used by the endpoint.
    eventBuses :: Prelude.Maybe (Prelude.NonEmpty EndpointEventBus),
    -- | The last time the endpoint was modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the endpoint.
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether event replication was enabled or disabled for this endpoint. The
    -- default state is @ENABLED@ which means you must supply a @RoleArn@. If
    -- you don\'t have a @RoleArn@ or you don\'t want event replication
    -- enabled, set the state to @DISABLED@.
    replicationConfig :: Prelude.Maybe ReplicationConfig,
    -- | The ARN of the role used by event replication for the endpoint.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The routing configuration of the endpoint.
    routingConfig :: Prelude.Maybe RoutingConfig,
    -- | The current state of the endpoint.
    state :: Prelude.Maybe EndpointState,
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
-- 'arn', 'endpoint_arn' - The ARN of the endpoint.
--
-- 'creationTime', 'endpoint_creationTime' - The time the endpoint was created.
--
-- 'description', 'endpoint_description' - A description for the endpoint.
--
-- 'endpointId', 'endpoint_endpointId' - The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
-- EndpointId is @abcde.veo@.
--
-- 'endpointUrl', 'endpoint_endpointUrl' - The URL of the endpoint.
--
-- 'eventBuses', 'endpoint_eventBuses' - The event buses being used by the endpoint.
--
-- 'lastModifiedTime', 'endpoint_lastModifiedTime' - The last time the endpoint was modified.
--
-- 'name', 'endpoint_name' - The name of the endpoint.
--
-- 'replicationConfig', 'endpoint_replicationConfig' - Whether event replication was enabled or disabled for this endpoint. The
-- default state is @ENABLED@ which means you must supply a @RoleArn@. If
-- you don\'t have a @RoleArn@ or you don\'t want event replication
-- enabled, set the state to @DISABLED@.
--
-- 'roleArn', 'endpoint_roleArn' - The ARN of the role used by event replication for the endpoint.
--
-- 'routingConfig', 'endpoint_routingConfig' - The routing configuration of the endpoint.
--
-- 'state', 'endpoint_state' - The current state of the endpoint.
--
-- 'stateReason', 'endpoint_stateReason' - The reason the endpoint is in its current state.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
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
      stateReason = Prelude.Nothing
    }

-- | The ARN of the endpoint.
endpoint_arn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_arn = Lens.lens (\Endpoint' {arn} -> arn) (\s@Endpoint' {} a -> s {arn = a} :: Endpoint)

-- | The time the endpoint was created.
endpoint_creationTime :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.UTCTime)
endpoint_creationTime = Lens.lens (\Endpoint' {creationTime} -> creationTime) (\s@Endpoint' {} a -> s {creationTime = a} :: Endpoint) Prelude.. Lens.mapping Data._Time

-- | A description for the endpoint.
endpoint_description :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_description = Lens.lens (\Endpoint' {description} -> description) (\s@Endpoint' {} a -> s {description = a} :: Endpoint)

-- | The URL subdomain of the endpoint. For example, if the URL for Endpoint
-- is https:\/\/abcde.veo.endpoints.event.amazonaws.com, then the
-- EndpointId is @abcde.veo@.
endpoint_endpointId :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointId = Lens.lens (\Endpoint' {endpointId} -> endpointId) (\s@Endpoint' {} a -> s {endpointId = a} :: Endpoint)

-- | The URL of the endpoint.
endpoint_endpointUrl :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_endpointUrl = Lens.lens (\Endpoint' {endpointUrl} -> endpointUrl) (\s@Endpoint' {} a -> s {endpointUrl = a} :: Endpoint)

-- | The event buses being used by the endpoint.
endpoint_eventBuses :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.NonEmpty EndpointEventBus))
endpoint_eventBuses = Lens.lens (\Endpoint' {eventBuses} -> eventBuses) (\s@Endpoint' {} a -> s {eventBuses = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | The last time the endpoint was modified.
endpoint_lastModifiedTime :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.UTCTime)
endpoint_lastModifiedTime = Lens.lens (\Endpoint' {lastModifiedTime} -> lastModifiedTime) (\s@Endpoint' {} a -> s {lastModifiedTime = a} :: Endpoint) Prelude.. Lens.mapping Data._Time

-- | The name of the endpoint.
endpoint_name :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_name = Lens.lens (\Endpoint' {name} -> name) (\s@Endpoint' {} a -> s {name = a} :: Endpoint)

-- | Whether event replication was enabled or disabled for this endpoint. The
-- default state is @ENABLED@ which means you must supply a @RoleArn@. If
-- you don\'t have a @RoleArn@ or you don\'t want event replication
-- enabled, set the state to @DISABLED@.
endpoint_replicationConfig :: Lens.Lens' Endpoint (Prelude.Maybe ReplicationConfig)
endpoint_replicationConfig = Lens.lens (\Endpoint' {replicationConfig} -> replicationConfig) (\s@Endpoint' {} a -> s {replicationConfig = a} :: Endpoint)

-- | The ARN of the role used by event replication for the endpoint.
endpoint_roleArn :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_roleArn = Lens.lens (\Endpoint' {roleArn} -> roleArn) (\s@Endpoint' {} a -> s {roleArn = a} :: Endpoint)

-- | The routing configuration of the endpoint.
endpoint_routingConfig :: Lens.Lens' Endpoint (Prelude.Maybe RoutingConfig)
endpoint_routingConfig = Lens.lens (\Endpoint' {routingConfig} -> routingConfig) (\s@Endpoint' {} a -> s {routingConfig = a} :: Endpoint)

-- | The current state of the endpoint.
endpoint_state :: Lens.Lens' Endpoint (Prelude.Maybe EndpointState)
endpoint_state = Lens.lens (\Endpoint' {state} -> state) (\s@Endpoint' {} a -> s {state = a} :: Endpoint)

-- | The reason the endpoint is in its current state.
endpoint_stateReason :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_stateReason = Lens.lens (\Endpoint' {stateReason} -> stateReason) (\s@Endpoint' {} a -> s {stateReason = a} :: Endpoint)

instance Data.FromJSON Endpoint where
  parseJSON =
    Data.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "EndpointId")
            Prelude.<*> (x Data..:? "EndpointUrl")
            Prelude.<*> (x Data..:? "EventBuses")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ReplicationConfig")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "RoutingConfig")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateReason")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` endpointUrl
      `Prelude.hashWithSalt` eventBuses
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` replicationConfig
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` routingConfig
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateReason

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
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
