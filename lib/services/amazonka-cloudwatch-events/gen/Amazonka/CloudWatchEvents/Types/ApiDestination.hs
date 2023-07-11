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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ApiDestination where

import Amazonka.CloudWatchEvents.Types.ApiDestinationHttpMethod
import Amazonka.CloudWatchEvents.Types.ApiDestinationState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an API destination.
--
-- /See:/ 'newApiDestination' smart constructor.
data ApiDestination = ApiDestination'
  { -- | The ARN of the API destination.
    apiDestinationArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the API destination.
    apiDestinationState :: Prelude.Maybe ApiDestinationState,
    -- | The ARN of the connection specified for the API destination.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | A time stamp for the time that the API destination was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The method to use to connect to the HTTP endpoint.
    httpMethod :: Prelude.Maybe ApiDestinationHttpMethod,
    -- | The URL to the endpoint for the API destination.
    invocationEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of invocations per second to send to the HTTP
    -- endpoint.
    invocationRateLimitPerSecond :: Prelude.Maybe Prelude.Natural,
    -- | A time stamp for the time that the API destination was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the API destination.
    name :: Prelude.Maybe Prelude.Text
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
-- 'apiDestinationArn', 'apiDestination_apiDestinationArn' - The ARN of the API destination.
--
-- 'apiDestinationState', 'apiDestination_apiDestinationState' - The state of the API destination.
--
-- 'connectionArn', 'apiDestination_connectionArn' - The ARN of the connection specified for the API destination.
--
-- 'creationTime', 'apiDestination_creationTime' - A time stamp for the time that the API destination was created.
--
-- 'httpMethod', 'apiDestination_httpMethod' - The method to use to connect to the HTTP endpoint.
--
-- 'invocationEndpoint', 'apiDestination_invocationEndpoint' - The URL to the endpoint for the API destination.
--
-- 'invocationRateLimitPerSecond', 'apiDestination_invocationRateLimitPerSecond' - The maximum number of invocations per second to send to the HTTP
-- endpoint.
--
-- 'lastModifiedTime', 'apiDestination_lastModifiedTime' - A time stamp for the time that the API destination was last modified.
--
-- 'name', 'apiDestination_name' - The name of the API destination.
newApiDestination ::
  ApiDestination
newApiDestination =
  ApiDestination'
    { apiDestinationArn =
        Prelude.Nothing,
      apiDestinationState = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpMethod = Prelude.Nothing,
      invocationEndpoint = Prelude.Nothing,
      invocationRateLimitPerSecond = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ARN of the API destination.
apiDestination_apiDestinationArn :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_apiDestinationArn = Lens.lens (\ApiDestination' {apiDestinationArn} -> apiDestinationArn) (\s@ApiDestination' {} a -> s {apiDestinationArn = a} :: ApiDestination)

-- | The state of the API destination.
apiDestination_apiDestinationState :: Lens.Lens' ApiDestination (Prelude.Maybe ApiDestinationState)
apiDestination_apiDestinationState = Lens.lens (\ApiDestination' {apiDestinationState} -> apiDestinationState) (\s@ApiDestination' {} a -> s {apiDestinationState = a} :: ApiDestination)

-- | The ARN of the connection specified for the API destination.
apiDestination_connectionArn :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_connectionArn = Lens.lens (\ApiDestination' {connectionArn} -> connectionArn) (\s@ApiDestination' {} a -> s {connectionArn = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was created.
apiDestination_creationTime :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.UTCTime)
apiDestination_creationTime = Lens.lens (\ApiDestination' {creationTime} -> creationTime) (\s@ApiDestination' {} a -> s {creationTime = a} :: ApiDestination) Prelude.. Lens.mapping Data._Time

-- | The method to use to connect to the HTTP endpoint.
apiDestination_httpMethod :: Lens.Lens' ApiDestination (Prelude.Maybe ApiDestinationHttpMethod)
apiDestination_httpMethod = Lens.lens (\ApiDestination' {httpMethod} -> httpMethod) (\s@ApiDestination' {} a -> s {httpMethod = a} :: ApiDestination)

-- | The URL to the endpoint for the API destination.
apiDestination_invocationEndpoint :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_invocationEndpoint = Lens.lens (\ApiDestination' {invocationEndpoint} -> invocationEndpoint) (\s@ApiDestination' {} a -> s {invocationEndpoint = a} :: ApiDestination)

-- | The maximum number of invocations per second to send to the HTTP
-- endpoint.
apiDestination_invocationRateLimitPerSecond :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Natural)
apiDestination_invocationRateLimitPerSecond = Lens.lens (\ApiDestination' {invocationRateLimitPerSecond} -> invocationRateLimitPerSecond) (\s@ApiDestination' {} a -> s {invocationRateLimitPerSecond = a} :: ApiDestination)

-- | A time stamp for the time that the API destination was last modified.
apiDestination_lastModifiedTime :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.UTCTime)
apiDestination_lastModifiedTime = Lens.lens (\ApiDestination' {lastModifiedTime} -> lastModifiedTime) (\s@ApiDestination' {} a -> s {lastModifiedTime = a} :: ApiDestination) Prelude.. Lens.mapping Data._Time

-- | The name of the API destination.
apiDestination_name :: Lens.Lens' ApiDestination (Prelude.Maybe Prelude.Text)
apiDestination_name = Lens.lens (\ApiDestination' {name} -> name) (\s@ApiDestination' {} a -> s {name = a} :: ApiDestination)

instance Data.FromJSON ApiDestination where
  parseJSON =
    Data.withObject
      "ApiDestination"
      ( \x ->
          ApiDestination'
            Prelude.<$> (x Data..:? "ApiDestinationArn")
            Prelude.<*> (x Data..:? "ApiDestinationState")
            Prelude.<*> (x Data..:? "ConnectionArn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "HttpMethod")
            Prelude.<*> (x Data..:? "InvocationEndpoint")
            Prelude.<*> (x Data..:? "InvocationRateLimitPerSecond")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable ApiDestination where
  hashWithSalt _salt ApiDestination' {..} =
    _salt
      `Prelude.hashWithSalt` apiDestinationArn
      `Prelude.hashWithSalt` apiDestinationState
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` invocationEndpoint
      `Prelude.hashWithSalt` invocationRateLimitPerSecond
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData ApiDestination where
  rnf ApiDestination' {..} =
    Prelude.rnf apiDestinationArn
      `Prelude.seq` Prelude.rnf apiDestinationState
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf invocationEndpoint
      `Prelude.seq` Prelude.rnf invocationRateLimitPerSecond
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
