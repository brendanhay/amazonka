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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionAuthRequestParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionApiKeyAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionBasicAuthRequestParameters
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the additional parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionAuthRequestParameters' smart constructor.
data UpdateConnectionAuthRequestParameters = UpdateConnectionAuthRequestParameters'
  { -- | A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
    -- authorization parameters for Basic authorization.
    basicAuthParameters :: Core.Maybe UpdateConnectionBasicAuthRequestParameters,
    -- | A @UpdateConnectionOAuthRequestParameters@ object that contains the
    -- authorization parameters for OAuth authorization.
    oAuthParameters :: Core.Maybe UpdateConnectionOAuthRequestParameters,
    -- | A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
    -- authorization parameters for API key authorization.
    apiKeyAuthParameters :: Core.Maybe UpdateConnectionApiKeyAuthRequestParameters,
    -- | A @ConnectionHttpParameters@ object that contains the additional
    -- parameters to use for the connection.
    invocationHttpParameters :: Core.Maybe ConnectionHttpParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectionAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'basicAuthParameters', 'updateConnectionAuthRequestParameters_basicAuthParameters' - A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
-- authorization parameters for Basic authorization.
--
-- 'oAuthParameters', 'updateConnectionAuthRequestParameters_oAuthParameters' - A @UpdateConnectionOAuthRequestParameters@ object that contains the
-- authorization parameters for OAuth authorization.
--
-- 'apiKeyAuthParameters', 'updateConnectionAuthRequestParameters_apiKeyAuthParameters' - A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
-- authorization parameters for API key authorization.
--
-- 'invocationHttpParameters', 'updateConnectionAuthRequestParameters_invocationHttpParameters' - A @ConnectionHttpParameters@ object that contains the additional
-- parameters to use for the connection.
newUpdateConnectionAuthRequestParameters ::
  UpdateConnectionAuthRequestParameters
newUpdateConnectionAuthRequestParameters =
  UpdateConnectionAuthRequestParameters'
    { basicAuthParameters =
        Core.Nothing,
      oAuthParameters = Core.Nothing,
      apiKeyAuthParameters = Core.Nothing,
      invocationHttpParameters =
        Core.Nothing
    }

-- | A @UpdateConnectionBasicAuthRequestParameters@ object that contains the
-- authorization parameters for Basic authorization.
updateConnectionAuthRequestParameters_basicAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Core.Maybe UpdateConnectionBasicAuthRequestParameters)
updateConnectionAuthRequestParameters_basicAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {basicAuthParameters} -> basicAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {basicAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @UpdateConnectionOAuthRequestParameters@ object that contains the
-- authorization parameters for OAuth authorization.
updateConnectionAuthRequestParameters_oAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Core.Maybe UpdateConnectionOAuthRequestParameters)
updateConnectionAuthRequestParameters_oAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {oAuthParameters} -> oAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {oAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @UpdateConnectionApiKeyAuthRequestParameters@ object that contains the
-- authorization parameters for API key authorization.
updateConnectionAuthRequestParameters_apiKeyAuthParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Core.Maybe UpdateConnectionApiKeyAuthRequestParameters)
updateConnectionAuthRequestParameters_apiKeyAuthParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {apiKeyAuthParameters} -> apiKeyAuthParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {apiKeyAuthParameters = a} :: UpdateConnectionAuthRequestParameters)

-- | A @ConnectionHttpParameters@ object that contains the additional
-- parameters to use for the connection.
updateConnectionAuthRequestParameters_invocationHttpParameters :: Lens.Lens' UpdateConnectionAuthRequestParameters (Core.Maybe ConnectionHttpParameters)
updateConnectionAuthRequestParameters_invocationHttpParameters = Lens.lens (\UpdateConnectionAuthRequestParameters' {invocationHttpParameters} -> invocationHttpParameters) (\s@UpdateConnectionAuthRequestParameters' {} a -> s {invocationHttpParameters = a} :: UpdateConnectionAuthRequestParameters)

instance
  Core.Hashable
    UpdateConnectionAuthRequestParameters

instance
  Core.NFData
    UpdateConnectionAuthRequestParameters

instance
  Core.ToJSON
    UpdateConnectionAuthRequestParameters
  where
  toJSON UpdateConnectionAuthRequestParameters' {..} =
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
