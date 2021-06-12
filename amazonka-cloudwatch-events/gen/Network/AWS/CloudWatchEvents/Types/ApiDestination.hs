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
-- Module      : Network.AWS.CloudWatchEvents.Types.ApiDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ApiDestination where

import Network.AWS.CloudWatchEvents.Types.ApiDestinationHttpMethod
import Network.AWS.CloudWatchEvents.Types.ApiDestinationState
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains details about an API destination.
--
-- /See:/ 'newApiDestination' smart constructor.
data ApiDestination = ApiDestination'
  { -- | The method to use to connect to the HTTP endpoint.
    httpMethod :: Core.Maybe ApiDestinationHttpMethod,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Core.Maybe Core.POSIX,
    -- | The ARN of the API destination.
    apiDestinationArn :: Core.Maybe Core.Text,
    -- | The URL to the endpoint for the API destination.
    invocationEndpoint :: Core.Maybe Core.Text,
    -- | The state of the API destination.
    apiDestinationState :: Core.Maybe ApiDestinationState,
    -- | The ARN of the connection specified for the API destination.
    connectionArn :: Core.Maybe Core.Text,
    -- | The name of the API destination.
    name :: Core.Maybe Core.Text,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The maximum number of invocations per second to send to the HTTP
    -- endpoint.
    invocationRateLimitPerSecond :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'apiDestination_httpMethod' - The method to use to connect to the HTTP endpoint.
--
-- 'creationTime', 'apiDestination_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'apiDestinationArn', 'apiDestination_apiDestinationArn' - The ARN of the API destination.
--
-- 'invocationEndpoint', 'apiDestination_invocationEndpoint' - The URL to the endpoint for the API destination.
--
-- 'apiDestinationState', 'apiDestination_apiDestinationState' - The state of the API destination.
--
-- 'connectionArn', 'apiDestination_connectionArn' - The ARN of the connection specified for the API destination.
--
-- 'name', 'apiDestination_name' - The name of the API destination.
--
-- 'lastModifiedTime', 'apiDestination_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'invocationRateLimitPerSecond', 'apiDestination_invocationRateLimitPerSecond' - The maximum number of invocations per second to send to the HTTP
-- endpoint.
newApiDestination ::
  ApiDestination
newApiDestination =
  ApiDestination'
    { httpMethod = Core.Nothing,
      creationTime = Core.Nothing,
      apiDestinationArn = Core.Nothing,
      invocationEndpoint = Core.Nothing,
      apiDestinationState = Core.Nothing,
      connectionArn = Core.Nothing,
      name = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      invocationRateLimitPerSecond = Core.Nothing
    }

-- | The method to use to connect to the HTTP endpoint.
apiDestination_httpMethod :: Lens.Lens' ApiDestination (Core.Maybe ApiDestinationHttpMethod)
apiDestination_httpMethod = Lens.lens (\ApiDestination' {httpMethod} -> httpMethod) (\s@ApiDestination' {} a -> s {httpMethod = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was created.
apiDestination_creationTime :: Lens.Lens' ApiDestination (Core.Maybe Core.UTCTime)
apiDestination_creationTime = Lens.lens (\ApiDestination' {creationTime} -> creationTime) (\s@ApiDestination' {} a -> s {creationTime = a} :: ApiDestination) Core.. Lens.mapping Core._Time

-- | The ARN of the API destination.
apiDestination_apiDestinationArn :: Lens.Lens' ApiDestination (Core.Maybe Core.Text)
apiDestination_apiDestinationArn = Lens.lens (\ApiDestination' {apiDestinationArn} -> apiDestinationArn) (\s@ApiDestination' {} a -> s {apiDestinationArn = a} :: ApiDestination)

-- | The URL to the endpoint for the API destination.
apiDestination_invocationEndpoint :: Lens.Lens' ApiDestination (Core.Maybe Core.Text)
apiDestination_invocationEndpoint = Lens.lens (\ApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@ApiDestination' {} a -> s {invocationEndpoint = a} :: ApiDestination)

-- | The state of the API destination.
apiDestination_apiDestinationState :: Lens.Lens' ApiDestination (Core.Maybe ApiDestinationState)
apiDestination_apiDestinationState = Lens.lens (\ApiDestination' {apiDestinationState} -> apiDestinationState) (\s@ApiDestination' {} a -> s {apiDestinationState = a} :: ApiDestination)

-- | The ARN of the connection specified for the API destination.
apiDestination_connectionArn :: Lens.Lens' ApiDestination (Core.Maybe Core.Text)
apiDestination_connectionArn = Lens.lens (\ApiDestination' {connectionArn} -> connectionArn) (\s@ApiDestination' {} a -> s {connectionArn = a} :: ApiDestination)

-- | The name of the API destination.
apiDestination_name :: Lens.Lens' ApiDestination (Core.Maybe Core.Text)
apiDestination_name = Lens.lens (\ApiDestination' {name} -> name) (\s@ApiDestination' {} a -> s {name = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was last modified.
apiDestination_lastModifiedTime :: Lens.Lens' ApiDestination (Core.Maybe Core.UTCTime)
apiDestination_lastModifiedTime = Lens.lens (\ApiDestination' {lastModifiedTime} -> lastModifiedTime) (\s@ApiDestination' {} a -> s {lastModifiedTime = a} :: ApiDestination) Core.. Lens.mapping Core._Time

-- | The maximum number of invocations per second to send to the HTTP
-- endpoint.
apiDestination_invocationRateLimitPerSecond :: Lens.Lens' ApiDestination (Core.Maybe Core.Natural)
apiDestination_invocationRateLimitPerSecond = Lens.lens (\ApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@ApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: ApiDestination)

instance Core.FromJSON ApiDestination where
  parseJSON =
    Core.withObject
      "ApiDestination"
      ( \x ->
          ApiDestination'
            Core.<$> (x Core..:? "HttpMethod")
            Core.<*> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "ApiDestinationArn")
            Core.<*> (x Core..:? "InvocationEndpoint")
            Core.<*> (x Core..:? "ApiDestinationState")
            Core.<*> (x Core..:? "ConnectionArn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "LastModifiedTime")
            Core.<*> (x Core..:? "InvocationRateLimitPerSecond")
      )

instance Core.Hashable ApiDestination

instance Core.NFData ApiDestination
