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
-- Module      : Amazonka.CloudWatchEvents.Types.ApiDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ApiDestination where

import Amazonka.CloudWatchEvents.Types.ApiDestinationHttpMethod
import Amazonka.CloudWatchEvents.Types.ApiDestinationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an API destination.
--
-- /See:/ 'newApiDestination' smart constructor.
data ApiDestination = ApiDestination'
  { -- | The name of the API destination.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of invocations per second to send to the HTTP
    -- endpoint.
    invocationRateLimitPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | The URL to the endpoint for the API destination.
    invocationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the connection specified for the API destination.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The method to use to connect to the HTTP endpoint.
    httpMethod :: Prelude.Maybe ApiDestinationHttpMethod,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The state of the API destination.
    apiDestinationState :: Prelude.Maybe ApiDestinationState,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the API destination.
    apiDestinationArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'apiDestination_name' - The name of the API destination.
--
-- 'invocationRateLimitPerSecond', 'apiDestination_invocationRateLimitPerSecond' - The maximum number of invocations per second to send to the HTTP
-- endpoint.
--
-- 'invocationEndpoint', 'apiDestination_invocationEndpoint' - The URL to the endpoint for the API destination.
--
-- 'connectionArn', 'apiDestination_connectionArn' - The ARN of the connection specified for the API destination.
--
-- 'httpMethod', 'apiDestination_httpMethod' - The method to use to connect to the HTTP endpoint.
--
-- 'lastModifiedTime', 'apiDestination_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'apiDestinationState', 'apiDestination_apiDestinationState' - The state of the API destination.
--
-- 'creationTime', 'apiDestination_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'apiDestinationArn', 'apiDestination_apiDestinationArn' - The ARN of the API destination.
newApiDestination ::
  ApiDestination
newApiDestination =
  ApiDestination'
    { name = Prelude.Nothing,
      invocationRateLimitPerSecond = Prelude.Nothing,
      invocationEndpoint = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      apiDestinationState = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      apiDestinationArn = Prelude.Nothing
    }

-- | The name of the API destination.
apiDestination_name :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_name = Lens.lens (\ApiDestination' {name} -> name) (\s@ApiDestination' {} a -> s {name = a} :: ApiDestination)

-- | The maximum number of invocations per second to send to the HTTP
-- endpoint.
apiDestination_invocationRateLimitPerSecond :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Natural)
apiDestination_invocationRateLimitPerSecond = Lens.lens (\ApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@ApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: ApiDestination)

-- | The URL to the endpoint for the API destination.
apiDestination_invocationEndpoint :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_invocationEndpoint = Lens.lens (\ApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@ApiDestination' {} a -> s {invocationEndpoint = a} :: ApiDestination)

-- | The ARN of the connection specified for the API destination.
apiDestination_connectionArn :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_connectionArn = Lens.lens (\ApiDestination' {connectionArn} -> connectionArn) (\s@ApiDestination' {} a -> s {connectionArn = a} :: ApiDestination)

-- | The method to use to connect to the HTTP endpoint.
apiDestination_httpMethod :: Lens.Lens' ApiDestination (Prelude.Maybe ApiDestinationHttpMethod)
apiDestination_httpMethod = Lens.lens (\ApiDestination' {httpMethod} -> httpMethod) (\s@ApiDestination' {} a -> s {httpMethod = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was last modified.
apiDestination_lastModifiedTime :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.UTCTime)
apiDestination_lastModifiedTime = Lens.lens (\ApiDestination' {lastModifiedTime} -> lastModifiedTime) (\s@ApiDestination' {} a -> s {lastModifiedTime = a} :: ApiDestination) Prelude.. Lens.mapping Core._Time

-- | The state of the API destination.
apiDestination_apiDestinationState :: Lens.Lens' ApiDestination (Prelude.Maybe ApiDestinationState)
apiDestination_apiDestinationState = Lens.lens (\ApiDestination' {apiDestinationState} -> apiDestinationState) (\s@ApiDestination' {} a -> s {apiDestinationState = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was created.
apiDestination_creationTime :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.UTCTime)
apiDestination_creationTime = Lens.lens (\ApiDestination' {creationTime} -> creationTime) (\s@ApiDestination' {} a -> s {creationTime = a} :: ApiDestination) Prelude.. Lens.mapping Core._Time

-- | The ARN of the API destination.
apiDestination_apiDestinationArn :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_apiDestinationArn = Lens.lens (\ApiDestination' {apiDestinationArn} -> apiDestinationArn) (\s@ApiDestination' {} a -> s {apiDestinationArn = a} :: ApiDestination)

instance Core.FromJSON ApiDestination where
  parseJSON =
    Core.withObject
      "ApiDestination"
      ( \x ->
          ApiDestination'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "InvocationRateLimitPerSecond")
            Prelude.<*> (x Core..:? "InvocationEndpoint")
            Prelude.<*> (x Core..:? "ConnectionArn")
            Prelude.<*> (x Core..:? "HttpMethod")
            Prelude.<*> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "ApiDestinationState")
            Prelude.<*> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "ApiDestinationArn")
      )

instance Prelude.Hashable ApiDestination where
  hashWithSalt _salt ApiDestination' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` invocationRateLimitPerSecond
      `Prelude.hashWithSalt` invocationEndpoint
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` apiDestinationState
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` apiDestinationArn

instance Prelude.NFData ApiDestination where
  rnf ApiDestination' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf invocationRateLimitPerSecond
      `Prelude.seq` Prelude.rnf invocationEndpoint
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf apiDestinationState
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf apiDestinationArn
