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
-- Module      : Amazonka.CloudWatchEvents.Types.ConnectionAuthResponseParameters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.ConnectionAuthResponseParameters where

import Amazonka.CloudWatchEvents.Types.ConnectionApiKeyAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionBasicAuthResponseParameters
import Amazonka.CloudWatchEvents.Types.ConnectionHttpParameters
import Amazonka.CloudWatchEvents.Types.ConnectionOAuthResponseParameters
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the authorization parameters to use for the connection.
--
-- /See:/ 'newConnectionAuthResponseParameters' smart constructor.
data ConnectionAuthResponseParameters = ConnectionAuthResponseParameters'
  { -- | The OAuth parameters to use for authorization.
    oAuthParameters :: Prelude.Maybe ConnectionOAuthResponseParameters,
    -- | Additional parameters for the connection that are passed through with
    -- every invocation to the HTTP endpoint.
    invocationHttpParameters :: Prelude.Maybe ConnectionHttpParameters,
    -- | The authorization parameters for Basic authorization.
    basicAuthParameters :: Prelude.Maybe ConnectionBasicAuthResponseParameters,
    -- | The API Key parameters to use for authorization.
    apiKeyAuthParameters :: Prelude.Maybe ConnectionApiKeyAuthResponseParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectionAuthResponseParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oAuthParameters', 'connectionAuthResponseParameters_oAuthParameters' - The OAuth parameters to use for authorization.
--
-- 'invocationHttpParameters', 'connectionAuthResponseParameters_invocationHttpParameters' - Additional parameters for the connection that are passed through with
-- every invocation to the HTTP endpoint.
--
-- 'basicAuthParameters', 'connectionAuthResponseParameters_basicAuthParameters' - The authorization parameters for Basic authorization.
--
-- 'apiKeyAuthParameters', 'connectionAuthResponseParameters_apiKeyAuthParameters' - The API Key parameters to use for authorization.
newConnectionAuthResponseParameters ::
  ConnectionAuthResponseParameters
newConnectionAuthResponseParameters =
  ConnectionAuthResponseParameters'
    { oAuthParameters =
        Prelude.Nothing,
      invocationHttpParameters =
        Prelude.Nothing,
      basicAuthParameters = Prelude.Nothing,
      apiKeyAuthParameters = Prelude.Nothing
    }

-- | The OAuth parameters to use for authorization.
connectionAuthResponseParameters_oAuthParameters :: Lens.Lens' ConnectionAuthResponseParameters (Prelude.Maybe ConnectionOAuthResponseParameters)
connectionAuthResponseParameters_oAuthParameters = Lens.lens (\ConnectionAuthResponseParameters' {oAuthParameters} -> oAuthParameters) (\s@ConnectionAuthResponseParameters' {} a -> s {oAuthParameters = a} :: ConnectionAuthResponseParameters)

-- | Additional parameters for the connection that are passed through with
-- every invocation to the HTTP endpoint.
connectionAuthResponseParameters_invocationHttpParameters :: Lens.Lens' ConnectionAuthResponseParameters (Prelude.Maybe ConnectionHttpParameters)
connectionAuthResponseParameters_invocationHttpParameters = Lens.lens (\ConnectionAuthResponseParameters' {invocationHttpParameters} -> invocationHttpParameters) (\s@ConnectionAuthResponseParameters' {} a -> s {invocationHttpParameters = a} :: ConnectionAuthResponseParameters)

-- | The authorization parameters for Basic authorization.
connectionAuthResponseParameters_basicAuthParameters :: Lens.Lens' ConnectionAuthResponseParameters (Prelude.Maybe ConnectionBasicAuthResponseParameters)
connectionAuthResponseParameters_basicAuthParameters = Lens.lens (\ConnectionAuthResponseParameters' {basicAuthParameters} -> basicAuthParameters) (\s@ConnectionAuthResponseParameters' {} a -> s {basicAuthParameters = a} :: ConnectionAuthResponseParameters)

-- | The API Key parameters to use for authorization.
connectionAuthResponseParameters_apiKeyAuthParameters :: Lens.Lens' ConnectionAuthResponseParameters (Prelude.Maybe ConnectionApiKeyAuthResponseParameters)
connectionAuthResponseParameters_apiKeyAuthParameters = Lens.lens (\ConnectionAuthResponseParameters' {apiKeyAuthParameters} -> apiKeyAuthParameters) (\s@ConnectionAuthResponseParameters' {} a -> s {apiKeyAuthParameters = a} :: ConnectionAuthResponseParameters)

instance
  Core.FromJSON
    ConnectionAuthResponseParameters
  where
  parseJSON =
    Core.withObject
      "ConnectionAuthResponseParameters"
      ( \x ->
          ConnectionAuthResponseParameters'
            Prelude.<$> (x Core..:? "OAuthParameters")
            Prelude.<*> (x Core..:? "InvocationHttpParameters")
            Prelude.<*> (x Core..:? "BasicAuthParameters")
            Prelude.<*> (x Core..:? "ApiKeyAuthParameters")
      )

instance
  Prelude.Hashable
    ConnectionAuthResponseParameters
  where
  hashWithSalt
    _salt
    ConnectionAuthResponseParameters' {..} =
      _salt `Prelude.hashWithSalt` oAuthParameters
        `Prelude.hashWithSalt` invocationHttpParameters
        `Prelude.hashWithSalt` basicAuthParameters
        `Prelude.hashWithSalt` apiKeyAuthParameters

instance
  Prelude.NFData
    ConnectionAuthResponseParameters
  where
  rnf ConnectionAuthResponseParameters' {..} =
    Prelude.rnf oAuthParameters
      `Prelude.seq` Prelude.rnf invocationHttpParameters
      `Prelude.seq` Prelude.rnf basicAuthParameters
      `Prelude.seq` Prelude.rnf apiKeyAuthParameters
