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
-- Module      : Network.AWS.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.CreateConnectionAuthRequestParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.CreateConnectionOAuthRequestParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the authorization parameters for the connection.
--
-- /See:/ 'newCreateConnectionAuthRequestParameters' smart constructor.
data CreateConnectionAuthRequestParameters = CreateConnectionAuthRequestParameters'
  { -- | A @CreateConnectionBasicAuthRequestParameters@ object that contains the
    -- Basic authorization parameters to use for the connection.
    basicAuthParameters :: Core.Maybe CreateConnectionBasicAuthRequestParameters,
    -- | A @CreateConnectionOAuthRequestParameters@ object that contains the
    -- OAuth authorization parameters to use for the connection.
    oAuthParameters :: Core.Maybe CreateConnectionOAuthRequestParameters,
    -- | A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
    -- API key authorization parameters to use for the connection.
    apiKeyAuthParameters :: Core.Maybe CreateConnectionApiKeyAuthRequestParameters,
    -- | A @ConnectionHttpParameters@ object that contains the API key
    -- authorization parameters to use for the connection. Note that if you
    -- include additional parameters for the target of a rule via
    -- @HttpParameters@, including query strings, the parameters added for the
    -- connection take precedence.
    invocationHttpParameters :: Core.Maybe ConnectionHttpParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateConnectionAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuthParameters', 'createConnectionAuthRequestParameters_basicAuthParameters' - A @CreateConnectionBasicAuthRequestParameters@ object that contains the
-- Basic authorization parameters to use for the connection.
--
-- 'oAuthParameters', 'createConnectionAuthRequestParameters_oAuthParameters' - A @CreateConnectionOAuthRequestParameters@ object that contains the
-- OAuth authorization parameters to use for the connection.
--
-- 'apiKeyAuthParameters', 'createConnectionAuthRequestParameters_apiKeyAuthParameters' - A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
-- API key authorization parameters to use for the connection.
--
-- 'invocationHttpParameters', 'createConnectionAuthRequestParameters_invocationHttpParameters' - A @ConnectionHttpParameters@ object that contains the API key
-- authorization parameters to use for the connection. Note that if you
-- include additional parameters for the target of a rule via
-- @HttpParameters@, including query strings, the parameters added for the
-- connection take precedence.
newCreateConnectionAuthRequestParameters ::
  CreateConnectionAuthRequestParameters
newCreateConnectionAuthRequestParameters =
  CreateConnectionAuthRequestParameters'
    { basicAuthParameters =
        Core.Nothing,
      oAuthParameters = Core.Nothing,
      apiKeyAuthParameters = Core.Nothing,
      invocationHttpParameters =
        Core.Nothing
    }

-- | A @CreateConnectionBasicAuthRequestParameters@ object that contains the
-- Basic authorization parameters to use for the connection.
createConnectionAuthRequestParameters_basicAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Core.Maybe CreateConnectionBasicAuthRequestParameters)
createConnectionAuthRequestParameters_basicAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {basicAuthParameters} -> basicAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {basicAuthParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @CreateConnectionOAuthRequestParameters@ object that contains the
-- OAuth authorization parameters to use for the connection.
createConnectionAuthRequestParameters_oAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Core.Maybe CreateConnectionOAuthRequestParameters)
createConnectionAuthRequestParameters_oAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {oAuthParameters} -> oAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {oAuthParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @CreateConnectionApiKeyAuthRequestParameters@ object that contains the
-- API key authorization parameters to use for the connection.
createConnectionAuthRequestParameters_apiKeyAuthParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Core.Maybe CreateConnectionApiKeyAuthRequestParameters)
createConnectionAuthRequestParameters_apiKeyAuthParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {apiKeyAuthParameters} -> apiKeyAuthParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {apiKeyAuthParameters = a} :: CreateConnectionAuthRequestParameters)

-- | A @ConnectionHttpParameters@ object that contains the API key
-- authorization parameters to use for the connection. Note that if you
-- include additional parameters for the target of a rule via
-- @HttpParameters@, including query strings, the parameters added for the
-- connection take precedence.
createConnectionAuthRequestParameters_invocationHttpParameters :: Lens.Lens' CreateConnectionAuthRequestParameters (Core.Maybe ConnectionHttpParameters)
createConnectionAuthRequestParameters_invocationHttpParameters = Lens.lens (\CreateConnectionAuthRequestParameters' {invocationHttpParameters} -> invocationHttpParameters) (\s@CreateConnectionAuthRequestParameters' {} a -> s {invocationHttpParameters = a} :: CreateConnectionAuthRequestParameters)

instance
  Core.Hashable
    CreateConnectionAuthRequestParameters

instance
  Core.NFData
    CreateConnectionAuthRequestParameters

instance
  Core.ToJSON
    CreateConnectionAuthRequestParameters
  where
  toJSON CreateConnectionAuthRequestParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BasicAuthParameters" Core..=)
              Core.<$> basicAuthParameters,
            ("OAuthParameters" Core..=) Core.<$> oAuthParameters,
            ("ApiKeyAuthParameters" Core..=)
              Core.<$> apiKeyAuthParameters,
            ("InvocationHttpParameters" Core..=)
              Core.<$> invocationHttpParameters
          ]
      )
