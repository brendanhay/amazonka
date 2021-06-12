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
-- Module      : Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthRequestParameters where

import Network.AWS.CloudWatchEvents.Types.ConnectionHttpParameters
import Network.AWS.CloudWatchEvents.Types.ConnectionOAuthHttpMethod
import Network.AWS.CloudWatchEvents.Types.UpdateConnectionOAuthClientRequestParameters
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains the OAuth request parameters to use for the connection.
--
-- /See:/ 'newUpdateConnectionOAuthRequestParameters' smart constructor.
data UpdateConnectionOAuthRequestParameters = UpdateConnectionOAuthRequestParameters'
  { -- | The method used to connect to the HTTP endpoint.
    httpMethod :: Core.Maybe ConnectionOAuthHttpMethod,
    -- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
    -- the client parameters to use for the connection when OAuth is specified
    -- as the authorization type.
    clientParameters :: Core.Maybe UpdateConnectionOAuthClientRequestParameters,
    -- | The URL to the authorization endpoint when OAuth is specified as the
    -- authorization type.
    authorizationEndpoint :: Core.Maybe Core.Text,
    -- | The additional HTTP parameters used for the OAuth authorization request.
    oAuthHttpParameters :: Core.Maybe ConnectionHttpParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateConnectionOAuthRequestParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpMethod', 'updateConnectionOAuthRequestParameters_httpMethod' - The method used to connect to the HTTP endpoint.
--
-- 'clientParameters', 'updateConnectionOAuthRequestParameters_clientParameters' - A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
--
-- 'authorizationEndpoint', 'updateConnectionOAuthRequestParameters_authorizationEndpoint' - The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
--
-- 'oAuthHttpParameters', 'updateConnectionOAuthRequestParameters_oAuthHttpParameters' - The additional HTTP parameters used for the OAuth authorization request.
newUpdateConnectionOAuthRequestParameters ::
  UpdateConnectionOAuthRequestParameters
newUpdateConnectionOAuthRequestParameters =
  UpdateConnectionOAuthRequestParameters'
    { httpMethod =
        Core.Nothing,
      clientParameters = Core.Nothing,
      authorizationEndpoint =
        Core.Nothing,
      oAuthHttpParameters = Core.Nothing
    }

-- | The method used to connect to the HTTP endpoint.
updateConnectionOAuthRequestParameters_httpMethod :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Core.Maybe ConnectionOAuthHttpMethod)
updateConnectionOAuthRequestParameters_httpMethod = Lens.lens (\UpdateConnectionOAuthRequestParameters' {httpMethod} -> httpMethod) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {httpMethod = a} :: UpdateConnectionOAuthRequestParameters)

-- | A @UpdateConnectionOAuthClientRequestParameters@ object that contains
-- the client parameters to use for the connection when OAuth is specified
-- as the authorization type.
updateConnectionOAuthRequestParameters_clientParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Core.Maybe UpdateConnectionOAuthClientRequestParameters)
updateConnectionOAuthRequestParameters_clientParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {clientParameters} -> clientParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {clientParameters = a} :: UpdateConnectionOAuthRequestParameters)

-- | The URL to the authorization endpoint when OAuth is specified as the
-- authorization type.
updateConnectionOAuthRequestParameters_authorizationEndpoint :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Core.Maybe Core.Text)
updateConnectionOAuthRequestParameters_authorizationEndpoint = Lens.lens (\UpdateConnectionOAuthRequestParameters' {authorizationEndpoint} -> authorizationEndpoint) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {authorizationEndpoint = a} :: UpdateConnectionOAuthRequestParameters)

-- | The additional HTTP parameters used for the OAuth authorization request.
updateConnectionOAuthRequestParameters_oAuthHttpParameters :: Lens.Lens' UpdateConnectionOAuthRequestParameters (Core.Maybe ConnectionHttpParameters)
updateConnectionOAuthRequestParameters_oAuthHttpParameters = Lens.lens (\UpdateConnectionOAuthRequestParameters' {oAuthHttpParameters} -> oAuthHttpParameters) (\s@UpdateConnectionOAuthRequestParameters' {} a -> s {oAuthHttpParameters = a} :: UpdateConnectionOAuthRequestParameters)

instance
  Core.Hashable
    UpdateConnectionOAuthRequestParameters

instance
  Core.NFData
    UpdateConnectionOAuthRequestParameters

instance
  Core.ToJSON
    UpdateConnectionOAuthRequestParameters
  where
  toJSON UpdateConnectionOAuthRequestParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HttpMethod" Core..=) Core.<$> httpMethod,
            ("ClientParameters" Core..=)
              Core.<$> clientParameters,
            ("AuthorizationEndpoint" Core..=)
              Core.<$> authorizationEndpoint,
            ("OAuthHttpParameters" Core..=)
              Core.<$> oAuthHttpParameters
          ]
      )
